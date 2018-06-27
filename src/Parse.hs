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
import qualified Lex
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
extractParagraph :: State -> [S.BreakableHListElem] -> C.Stream -> IO (State, [S.BreakableHListElem], C.Stream)
extractParagraph state acc stream =
  case C.extractHModeCommand stream of
    -- Run out of commands: return the list so far.
    -- Left x -> return (state, acc, stream)
    Left x -> error $ show x
    Right (C.HAllModesCommand aCom, streamNext) ->
      case aCom of
        C.Assign C.Assignment{body=C.DefineFont fNr} ->
          do
          (stateNext, fontDef) <- defineFont state fNr
          extractParagraph stateNext (S.HFontDefinition fontDef:acc) streamNext
        C.Assign C.Assignment{body=C.SelectFont fNr} ->
          do
          let (stateNext, fontSel) = selectFont state fNr
          extractParagraph stateNext (S.HFontSelection fontSel:acc) streamNext
        C.Relax ->
          extractParagraph state acc streamNext
        -- \par: end the current paragraph.
        C.EndParagraph ->
          return (state, acc, streamNext)
        C.AddKern k ->
          extractParagraph state ((S.HKern $ B.Kern k):acc) streamNext
        -- \indent: An empty box of width \parindent is appended to the current
        -- list, and the space factor is set to 1000.
        -- TODO: Space factor.
        C.StartParagraph True ->
          extractParagraph state (theParIndent:acc) streamNext
        -- \noindent: has no effect in horizontal modes.
        C.StartParagraph False ->
          extractParagraph state acc streamNext
        C.AddSpace ->
          do
          glue <- case spaceGlue state of
            Just sg -> return $ S.HGlue sg
            Nothing -> fail "Could not get space glue"
          extractParagraph state (glue:acc) streamNext
        -- _ ->
        --   fail $ "Unknown all-mode command in horizontal mode: " ++ show aCom
    Right (C.AddCharacter{code=i}, streamNext) ->
      do
      charBox <- case characterBox state i of
        Just c -> return $ S.HCharacter c
        Nothing -> fail "Could not get character info"
      extractParagraph state (charBox:acc) streamNext
    Right (C.LeaveHMode, _) ->
      -- Inner mode: forbidden. TODO.
      -- Outer mode: insert the control sequence "\par" into the input. The control
      -- sequence's current meaning will be used, which might no longer be the \par
      -- primitive.
    -- (Note that we pass 'stream', not 'streamNext'.)
      do
      let parToken = Lex.ControlSequence $ Lex.ControlWord "par"
      extractParagraph state acc $ C.insertLexToken stream parToken

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

addParagraphToPage :: State -> [B.Page] -> [S.BreakableVListElem] -> C.Stream -> Bool -> IO (State, [B.Page], [S.BreakableVListElem], C.Stream)
addParagraphToPage state pages acc stream indent
  = do
    -- Paraboxes returned in normal order.
    (stateNext, paraBoxes, streamNext) <- extractBoxedParagraph indent theDesiredWidth theLineTolerance theLinePenalty theInterLineGlue state stream
    let
      breakItem = S.GlueBreak theInterLineGlue
      accBreak = S.VGlue theInterLineGlue:reverse paraBoxes
      accNoBreak = accBreak ++ acc
      -- TODO: Discard when adding to empty page.
      -- TODO: Keep best rather than taking last.
      pen = S.breakPenalty breakItem
      -- Expects normal order.
      stat = S.listGlueSetRatio theDesiredHeight $ reverse accNoBreak
      bad = S.listStatusBadness stat
      cost = S.pageCost pen bad 0
      -- Expects normal order.
      page = B.Page $ reverse $ S.setListElems stat acc
    if (cost == S.oneMillion) || (pen <= -S.tenK)
      then extractPages stateNext (page:pages) accBreak streamNext
      else extractPages stateNext pages accNoBreak streamNext

extractPages :: State -> [B.Page] -> [S.BreakableVListElem] -> C.Stream -> IO (State, [B.Page], [S.BreakableVListElem], C.Stream)
extractPages state pages acc stream =
  case C.extractVModeCommand stream of
    -- Expects normal order.
    -- Left _ -> return (state, B.Page $ reverse $ S.setListElems S.NaturallyGood acc, [], stream)
    Left x -> error $ show x
    -- If the command shifts to horizontal mode, run '\indent', and re-read the
    -- stream as if the commands just seen hadn't been read.
    -- (Note that we pass 'stream', not 'streamNext'.)
    Right (C.EnterHMode, _) ->
      addParagraphToPage state pages acc stream True
    Right (C.End, streamNext) ->
      do
      let lastPage = B.Page $ reverse $ S.setListElems S.NaturallyGood acc
      return (state, lastPage:pages, acc, streamNext)
    Right (C.VAllModesCommand aCom, streamNext) ->
      case aCom of
        C.Assign C.Assignment{body=C.DefineFont fNr} ->
          do
          (stateNext, fontDef) <- defineFont state fNr
          extractPages stateNext pages (S.VFontDefinition fontDef:acc) streamNext
        C.Assign C.Assignment{body=C.SelectFont fNr} ->
          do
          let (stateNext, fontSel) = selectFont state fNr
          T.trace "selected font" extractPages stateNext pages (S.VFontSelection fontSel:acc) streamNext
        C.Relax ->
          extractPages state pages acc streamNext
        -- \par does nothing in vertical mode.
        C.EndParagraph ->
          extractPages state pages acc streamNext
        C.AddKern k ->
          extractPages state pages ((S.VKern $ B.Kern k):acc) streamNext
        C.StartParagraph indent ->
          addParagraphToPage state pages acc streamNext indent
        -- <space token> has no effect in vertical modes.
        C.AddSpace ->
          extractPages state pages acc streamNext
