{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse where

import qualified Data.IntMap.Strict as IMAP
import Data.List (intersperse)

import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified Box as B
import qualified Setting as S
import qualified Command as C
import qualified Unit

import qualified Debug.Trace as T

type FontInfoMap = IMAP.IntMap TFMM.TexFont

data State = State { currentFontNr :: Maybe Int
                   , fontInfoMap :: FontInfoMap } deriving Show

newState :: State
newState = State {currentFontNr=Nothing, fontInfoMap=IMAP.empty}

theParIndent :: S.BreakableHListElem
theParIndent = S.HHBox B.HBox{contents=[], desiredLength=B.To $ fromIntegral (round $ Unit.pointToScaledPoint 20 :: Int)}

theDesiredWidth :: Int
theDesiredWidth = 30750000

theLineTolerance :: Int
theLineTolerance = 500

theLinePenalty :: Int
theLinePenalty = 10

theDesiredHeight :: Int
theDesiredHeight = 45000000

theInterLineGlue :: S.Glue
theInterLineGlue = S.Glue{dimen = 400000, stretch=S.noFlex, shrink=S.noFlex}

currentFontInfo :: State -> Maybe TFMM.TexFont
currentFontInfo state = do
  -- Maybe font number isn't set.
  fontNr <- currentFontNr state
  -- Or maybe there's no font where there should be.
  -- TODO: I think I can make this case impossible, maybe by storing the
  -- current font info directly instead of a lookup.
  IMAP.lookup fontNr $ fontInfoMap state

defineFont :: State -> Int -> IO (State, B.FontDefinition)
defineFont state nr = do
    font <- TFMM.readTFM "support/cmr10.tfm"
    let
      fontDef = B.FontDefinition { fontNr = nr
                                 , fontPath = "support/cmr10.tfm"
                                 , fontName = "cmr10"
                                 , fontInfo = font
                                 , scaleFactorRatio = 1.0
                                 }
      stateNext = state{fontInfoMap=IMAP.insert nr font $ fontInfoMap state}
    return (stateNext, fontDef)

selectFont :: State -> Int -> (State, B.FontSelection)
selectFont state n =
  (state{currentFontNr=Just n}, B.FontSelection{fontNr = n})

characterBox :: State -> Int -> Maybe B.Character
characterBox state code = do
  font <- currentFontInfo state
  let toSP = TFMM.designScaleSP font
  TFMC.Character{width=w, height=h, depth=d} <- IMAP.lookup code $ TFMM.characters font
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

spaceGlue :: State -> Maybe S.Glue
spaceGlue state = do
  font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr} <- currentFontInfo state
  let
    toSP = TFMM.designScaleSP font
    toFlex = S.finiteFlex . toSP
  return S.Glue{dimen=toSP d, stretch=toFlex str, shrink=toFlex shr}

-- We build a paragraph list in reverse order.
extractParagraphInner :: State -> [S.BreakableHListElem] -> C.Stream -> C.HModeCommand -> IO (State, [S.BreakableHListElem], C.Stream)
extractParagraphInner state acc stream (C.HAllModesCommand aCom)
  = case aCom of
    C.Assign C.Assignment{body=C.DefineFont fNr} ->
      do
      (state1, fontDef) <- defineFont state fNr
      extractParagraph state1 (S.HFontDefinition fontDef:acc) stream
    C.Assign C.Assignment{body=C.SelectFont fNr} ->
      do
      let (state1, fontSel) = selectFont state fNr
      extractParagraph state1 (S.HFontSelection fontSel:acc) stream
    C.Relax ->
      extractParagraph state acc stream
    -- \par: end the current paragraph.
    C.EndParagraph ->
      return (state, acc, stream)
    C.AddKern k ->
      extractParagraph state ((S.HKern $ B.Kern k):acc) stream
    -- \indent: An empty box of width \parindent is appended to the current
    -- list, and the space factor is set to 1000.
    -- TODO: Space factor.
    C.StartParagraph True ->
      extractParagraph state (theParIndent:acc) stream
    -- \noindent: has no effect in horizontal modes.
    C.StartParagraph False ->
      extractParagraph state acc stream
    C.AddSpace ->
      do
      glue <- case spaceGlue state of
        Just sg -> return $ S.HGlue sg
        Nothing -> fail "Could not get space glue"
      extractParagraph state (glue:acc) stream
    -- _ ->
    --   fail $ "Unknown all-mode command in horizontal mode: " ++ show aCom
extractParagraphInner state acc stream C.AddCharacter{code=i}
  = do
    charBox <- case characterBox state i of
      Just c -> return $ S.HCharacter c
      Nothing -> fail "Could not get character info"
    extractParagraph state (charBox:acc) stream
-- Inner mode: forbidden.
-- Outer mode: insert the control sequence "\par" into the input. The control
-- sequence's current meaning will be used, which might no longer be the \par
-- primitive.
extractParagraphInner state acc stream C.LeaveHMode
  -- TODO: Do something.
  = T.traceShow stream $ extractParagraph state acc stream
-- extractParagraphInner _ _ _ com
--   = fail $ "Unknown h-mode command in horizontal mode: " ++ show com

extractParagraph :: State -> [S.BreakableHListElem] -> C.Stream -> IO (State, [S.BreakableHListElem], C.Stream)
extractParagraph state acc stream =
  case C.extractHModeCommand stream of
    -- Run out of commands: return the list so far.
    -- Left x -> return (state, acc, stream)
    Left x -> error $ show x
    Right (com, streamNext) -> extractParagraphInner state acc streamNext com

extractBoxedParagraph :: Bool -> Int -> Int -> Int -> S.Glue -> State -> C.Stream -> IO (State, [S.BreakableVListElem], C.Stream)
extractBoxedParagraph indent desiredWidth lineTolerance linePenalty interLineGlue state stream = do
  let
    initial True = [theParIndent]
    initial False = []
  (stateNext, hList, streamNext) <- extractParagraph state (initial indent) stream
  let
    lineBoxes = S.setParagraph desiredWidth lineTolerance linePenalty hList
    paraBoxes = intersperse (S.VGlue interLineGlue) lineBoxes
  return (stateNext, paraBoxes, streamNext)

addParagraphToPage :: State -> [S.BreakableVListElem] -> C.Stream -> Bool -> IO (State, B.Page, [S.BreakableVListElem], C.Stream)
addParagraphToPage state acc stream indent
  = do
    -- Paraboxes returned in normal order.
    (stateNext, paraBoxes, streamNext) <- extractBoxedParagraph indent theDesiredWidth theLineTolerance theLinePenalty theInterLineGlue state stream
    let
      breakItem = S.GlueBreak theInterLineGlue
      extra = S.VGlue theInterLineGlue:reverse paraBoxes
      accNext = extra ++ acc
      -- TODO: Discard when adding to empty page.
      -- TODO: Keep best rather than taking last.
      pen = S.breakPenalty breakItem
      -- Expects normal order.
      stat = S.listGlueSetRatio theDesiredHeight $ reverse accNext
      bad = S.listStatusBadness stat
      cost = S.pageCost pen bad 0
      -- Expects normal order.
      page = B.Page $ reverse $ S.setListElems stat acc
    if (cost == S.oneMillion) || (pen <= -S.tenK)
      then return (stateNext, page, extra, streamNext)
          else extractPage stateNext accNext streamNext

extractPage :: State -> [S.BreakableVListElem] -> C.Stream -> IO (State, B.Page, [S.BreakableVListElem], C.Stream)
extractPage state acc stream =
  case C.extractVModeCommand stream of
    -- Expects normal order.
    Left _ -> return (state, B.Page $ reverse $ S.setListElems S.NaturallyGood acc, [], stream)
    -- Left x -> error $ show x
    -- If the command shifts to horizontal mode, run '\indent', and re-read the
    -- stream as if the commands just seen hadn't been read.
    -- (Note that we pass 'stream', not 'streamNext'.)
    Right (C.EnterHMode, _) ->
      addParagraphToPage state acc stream True
      -- extractPageInner state acc stream (C.VAllModesCommand $ C.StartParagraph True)
    Right (C.VAllModesCommand aCom, streamNext) ->
      case aCom of
        C.Assign C.Assignment{body=C.DefineFont fNr} ->
          do
          (stateNext, fontDef) <- defineFont state fNr
          extractPage stateNext (S.VFontDefinition fontDef:acc) streamNext
        C.Assign C.Assignment{body=C.SelectFont fNr} ->
          do
          let (stateNext, fontSel) = selectFont state fNr
          extractPage stateNext (S.VFontSelection fontSel:acc) streamNext
        C.Relax ->
          extractPage state acc streamNext
        -- \par does nothing in vertical mode.
        C.EndParagraph ->
          extractPage state acc streamNext
        C.AddKern k ->
          extractPage state ((S.VKern $ B.Kern k):acc) streamNext
        C.StartParagraph indent ->
          addParagraphToPage state acc streamNext indent
        -- <space token> has no effect in vertical modes.
        C.AddSpace ->
          extractPage state acc streamNext
        -- _ ->
        --   fail $ "Unknown all-mode command in vertical mode: " ++ show aCom
    -- extractPageInner _ _ _ com
    --   = fail $ "Unknown v-mode command in vertical mode: " ++ show com



extractPages :: State -> [S.BreakableVListElem] -> C.Stream -> IO [B.Page]
extractPages _ _ C.Stream{codes=[]} = return []
extractPages state acc stream = do
    (stateNext, page, vListRemain, streamNext) <- extractPage state acc stream
    if C.atEnd streamNext
      then T.trace "at end" $ return [page]
      else do
        pagesRest <- extractPages stateNext vListRemain streamNext
        return $ page:pagesRest
