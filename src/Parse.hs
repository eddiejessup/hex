{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse where

import qualified Data.IntMap.Strict as IMAP
import Data.List (intersperse)

import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified Box as B
import qualified Setting as S
import qualified Cat
import qualified Lex
import qualified Command as C
import qualified Unit

import qualified Debug.Trace as T

type FontInfoMap = IMAP.IntMap TFMM.TexFont

data State = State { currentFontNr :: Maybe Int
                   , fontInfoMap :: FontInfoMap } deriving Show

newState :: State
newState = State {currentFontNr=Nothing, fontInfoMap=IMAP.empty}

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
      newState = state{fontInfoMap=IMAP.insert nr font $ fontInfoMap state}
    return (newState, fontDef)

selectFont :: State -> Int -> (State, B.FontSelection)
selectFont state n =
  (state{currentFontNr=Just n}, B.FontSelection{fontNr = n})

isModeIndependent :: C.Command -> Bool
isModeIndependent C.Relax = True
isModeIndependent _ = False

runAllModeCommand :: State -> C.Stream -> C.Command -> IO (State, C.Stream)
runAllModeCommand state stream com
  | C.Relax <- com = return (state, stream)
  | otherwise = return (state, stream)

theFontNr = 1

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
extractParagraphInner :: State -> [S.BreakableHListElem] -> C.Stream -> C.Command -> IO (State, [S.BreakableHListElem], C.Stream)
extractParagraphInner state acc stream com1
  | C.Assign {assignment=C.DefineFont} <- com1 = do
    (state1, fontDef) <- defineFont state theFontNr
    extractParagraph state1 (S.HFontDefinition fontDef:acc) stream
  | C.Assign {assignment=C.SelectFont} <- com1 = do
    let (state1, fontSel) = selectFont state theFontNr
    extractParagraph state1 (S.HFontSelection fontSel:acc) stream
  | isModeIndependent com1 = do
    (state1, streamNext) <- runAllModeCommand state stream com1
    extractParagraph state1 acc streamNext
  | C.AddCharacter{code=i} <- com1 = do
    charBox <- case characterBox state i of
      Just c -> return $ S.HCharacter c
      Nothing -> fail "Could not get character info"
    extractParagraph state (charBox:acc) stream
  | C.AddSpace{} <- com1 = do
    glue <- case spaceGlue state of
      Just sg -> return $ S.HGlue sg
      Nothing -> fail "Could not get space glue"
    extractParagraph state (glue:acc) stream
  | C.AddKern k <- com1 = do
    extractParagraph state ((S.HKern $ B.Kern k):acc) stream
  -- See \par: Return to outer mode with completed list.
  | C.EndParagraph{} <- com1 = return (state, acc, stream)
  -- See something else: Ignore and continue.
  | otherwise = fail $ "Unknown command in horizontal mode: " ++ (show com1)

extractParagraph :: State -> [S.BreakableHListElem] -> C.Stream -> IO (State, [S.BreakableHListElem], C.Stream)
extractParagraph state acc stream =
  case C.extractCommand stream of
    -- Run out of commands: return the list so far.
    Nothing -> return (state, acc, stream)
    Just (com1, streamNext) -> extractParagraphInner state acc streamNext com1

parIndent = S.HHBox B.HBox{contents=[], desiredLength=B.To $ fromIntegral $ round $ Unit.pointToScaledPoint 20}

extractBoxedParagraph :: Int -> Int -> Int -> S.Glue -> State -> C.Stream -> IO (State, [S.BreakableVListElem], C.Stream)
extractBoxedParagraph desiredWidth lineTolerance linePenalty interLineGlue state stream = do
  (stateNext, hList, streamNext) <- extractParagraph state [parIndent] stream
  let
    lineBoxes = S.setParagraph desiredWidth lineTolerance linePenalty hList
    paraBoxes = intersperse (S.VGlue interLineGlue) lineBoxes
  return (stateNext, paraBoxes, streamNext)

desiredWidth :: Int
desiredWidth = 30750000

lineTolerance :: Int
lineTolerance = 500

linePenalty :: Int
linePenalty = 10

desiredHeight :: Int
desiredHeight = 45000000

interLineGlue :: S.Glue
interLineGlue = S.Glue{dimen = 400000, stretch=S.noFlex, shrink=S.noFlex}

startsParagraph :: C.Command -> Bool
startsParagraph C.AddCharacter{} = True
startsParagraph C.AddSpace{} = True
startsParagraph _ = False

extractPageInner :: State -> [S.BreakableVListElem] -> C.Stream -> C.Command -> IO (State, B.Page, [S.BreakableVListElem], C.Stream)
extractPageInner state acc stream com1
  | C.Assign {assignment=C.DefineFont} <- com1 = do
    (state1, fontDef) <- defineFont state theFontNr
    extractPage state1 (S.VFontDefinition fontDef:acc) stream
  | C.Assign {assignment=C.SelectFont} <- com1 = do
    let (state1, fontSel) = selectFont state theFontNr
    extractPage state1 (S.VFontSelection fontSel:acc) stream
  | isModeIndependent com1 = do
    (state1, streamNext) <- runAllModeCommand state stream com1
    extractPage state1 acc streamNext
  -- '\par' does nothing in vertical mode.
  | C.EndParagraph{} <- com1 =
    extractPage state acc stream
  | C.AddKern k <- com1 =
    extractPage state ((S.VKern $ B.Kern k):acc) stream
  | otherwise =
    fail $ "Unknown command in vertical mode: " ++ (show com1)

extractPage :: State -> [S.BreakableVListElem] -> C.Stream -> IO (State, B.Page, [S.BreakableVListElem], C.Stream)
extractPage state acc stream =
  case C.extractCommand stream of
    -- Expects normal order.
    Nothing -> return (state, B.Page $ reverse $ S.setListElems S.NaturallyGood acc, [], stream)
    Just (com1, streamNext) ->
      -- If the command shifts to horizontal mode, re-read the stream in
      -- horizontal mode. Note that we pass 'stream', not 'streamNext'.
      if startsParagraph com1
        then do
          -- Paraboxes returned in normal order.
          (stateNext, paraBoxes, streamNextH) <- extractBoxedParagraph desiredWidth lineTolerance linePenalty interLineGlue state stream
          let
            breakItem = S.GlueBreak interLineGlue
            extra = S.VGlue interLineGlue:reverse paraBoxes
            accNext = extra ++ acc
            -- TODO: Discard when adding to empty page.
            -- TODO: Keep best rather than taking last.
            pen = S.breakPenalty breakItem
            -- Expects normal order.
            stat = S.listGlueSetRatio desiredHeight $ reverse accNext
            bad = S.listStatusBadness stat
            cost = S.pageCost pen bad 0
            -- Expects normal order.
            page = B.Page $ reverse $ S.setListElems stat acc
          if (cost == S.oneMillion) || (pen <= -S.tenK)
            then return (stateNext, page, extra, streamNextH)
            else extractPage stateNext accNext streamNextH
        else
          extractPageInner state acc streamNext com1

extractPages :: State -> [S.BreakableVListElem] -> C.Stream -> IO [B.Page]
extractPages _ _ C.Stream{codes=[]} = return []
extractPages state acc stream = do
    (stateNext, page, vListRemain, streamNext) <- extractPage state acc stream
    if C.atEnd streamNext
      then return [page]
      else do
        pagesRest <- extractPages stateNext vListRemain streamNext
        return $ page:pagesRest
