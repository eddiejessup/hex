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

runAllModeCommand :: State -> Cat.CharCatMap -> C.Command -> IO (State, Cat.CharCatMap)
runAllModeCommand state ccMap com
  | C.Relax <- com = return (state, ccMap)
  | otherwise = return (state, ccMap)

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

extractParagraphInner :: State -> [S.BreakableHListElem] -> Cat.CharCatMap -> Lex.LexState -> C.Command -> [Cat.CharCode] -> IO (State, [S.BreakableHListElem], Cat.CharCatMap, Lex.LexState, [Cat.CharCode])
extractParagraphInner state acc ccMap lexState com1 cs
  | C.Assign {assignment=C.DefineFont} <- com1 = do
    (state1, fontDef) <- defineFont state theFontNr
    extractParagraph state1 (acc ++ [S.HFontDefinition fontDef]) ccMap lexState cs
  | C.Assign {assignment=C.SelectFont} <- com1 = do
    let (state1, fontSel) = selectFont state theFontNr
    extractParagraph state1 (acc ++ [S.HFontSelection fontSel]) ccMap lexState cs
  | isModeIndependent com1 = do
    (state2, ccMap2) <- runAllModeCommand state ccMap com1
    extractParagraph state2 acc ccMap2 lexState cs
  | C.AddCharacter{code=i} <- com1 = do
    charBox <- case characterBox state i of
      Just c -> return $ S.HCharacter c
      Nothing -> fail "Could not get character info"
    extractParagraph state (charBox:acc) ccMap lexState cs
  | C.AddSpace{} <- com1 = do
    glue <- case spaceGlue state of
      Just sg -> return $ S.HGlue sg
      Nothing -> fail "Could not get space glue"
    extractParagraph state (glue:acc) ccMap lexState cs
  | C.AddKern k <- com1 = do
    extractParagraph state ((S.HKern $ B.Kern k):acc) ccMap lexState cs
  -- See \par: Return to outer mode with completed list.
  | C.StartParagraph{} <- com1 = return (state, acc, ccMap, lexState, cs)
  -- See something else: Ignore and continue.
  | otherwise = fail $ "Unknown command in horizontal mode: " ++ (show com1)

extractParagraph :: State -> [S.BreakableHListElem] -> Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> IO (State, [S.BreakableHListElem], Cat.CharCatMap, Lex.LexState, [Cat.CharCode])
-- Run out of characters: return the list so far.
extractParagraph state acc ccMap lexState [] = return (state, acc, ccMap, lexState, [])
extractParagraph state acc ccMap lexState cs =
  case C.extractCommand ccMap lexState cs of
    Nothing -> return (state, acc, ccMap, lexState, [])
    Just (com1, rest, lexState1) -> extractParagraphInner state acc ccMap lexState1 com1 rest

parIndent = S.HHBox B.HBox{contents=[], desiredLength=B.To $ fromIntegral $ round $ Unit.pointToScaledPoint 20}

extractBoxedParagraph :: Int -> Int -> Int -> S.Glue -> State -> Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> IO (State, [S.BreakableVListElem], Cat.CharCatMap, Lex.LexState, [Cat.CharCode])
extractBoxedParagraph desiredWidth lineTolerance linePenalty interLineGlue state ccMap lexState cs = do
  (stateNext, hList, ccMapNext, lexStateNext, rest) <- extractParagraph state [parIndent] ccMap lexState cs
  let
    lineBoxes = S.setParagraph desiredWidth lineTolerance linePenalty hList
    paraBoxes = intersperse (S.VGlue interLineGlue) lineBoxes
    -- paraBoxes = T.traceShow (lineBoxes !! 0) intersperse (S.VGlue interLineGlue) lineBoxes
  return (stateNext, paraBoxes, ccMapNext, lexStateNext, rest)

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

extractPageInner :: State -> [S.BreakableVListElem] -> Cat.CharCatMap -> Lex.LexState -> C.Command -> [Cat.CharCode] -> IO (State, B.Page, [S.BreakableVListElem], Cat.CharCatMap, Lex.LexState, [Cat.CharCode])
extractPageInner state acc ccMap lexState com1 cs
  | C.Assign {assignment=C.DefineFont} <- com1 = do
    (state1, fontDef) <- defineFont state theFontNr
    extractPage state1 (acc ++ [S.VFontDefinition fontDef]) ccMap lexState cs
  | C.Assign {assignment=C.SelectFont} <- com1 = do
    let (state1, fontSel) = selectFont state theFontNr
    extractPage state1 (acc ++ [S.VFontSelection fontSel]) ccMap lexState cs
  | isModeIndependent com1 = do
    (state1, ccMap2) <- runAllModeCommand state ccMap com1
    extractPage state1 acc ccMap2 lexState cs
  | C.StartParagraph{} <- com1 =
    extractPage state acc ccMap lexState cs
  | otherwise =
    fail $ "Unknown command in vertical mode: " ++ (show com1)

extractPage :: State -> [S.BreakableVListElem] -> Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> IO (State, B.Page, [S.BreakableVListElem], Cat.CharCatMap, Lex.LexState, [Cat.CharCode])
extractPage state acc ccMap lexState [] = return (state, B.Page $ S.setListElems S.NaturallyGood acc, [], ccMap, lexState, [])
extractPage state acc ccMap lexState cs =
  case C.extractCommand ccMap lexState cs of
    Nothing -> return (state, B.Page $ S.setListElems S.NaturallyGood acc, [], ccMap, lexState, [])
    Just (com1, rest, lexState1) ->
      if startsParagraph com1
        then do
          (state2, paraBoxes, ccMap1, lexState2, rest2) <- extractBoxedParagraph desiredWidth lineTolerance linePenalty interLineGlue state ccMap lexState cs
          let
            breakItem = S.GlueBreak interLineGlue
            extra = paraBoxes ++ [S.VGlue interLineGlue]
            -- TODO: Discard when adding to empty page.
            -- TODO: Keep best rather than taking last.
            pen = S.breakPenalty breakItem
            stat = S.listGlueSetRatio desiredHeight $ acc ++ extra
            bad = S.listStatusBadness stat
            cost = T.traceShow stat $ S.pageCost pen bad 0
            page = B.Page $ S.setListElems stat acc
          if (cost == S.oneMillion) || (pen <= -S.tenK)
            then return (state2, page, extra, ccMap1, lexState2, rest2)
            else extractPage state2 (acc ++ extra) ccMap1 lexState2 rest2
        else
          extractPageInner state acc ccMap lexState1 com1 rest

extractPages :: State -> [S.BreakableVListElem] -> Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> IO [B.Page]
extractPages state0 [] ccMap0 lexState0 [] = return []
extractPages state0 acc ccMap0 lexState0 cs = do
    (state1, page, vListRemain, ccMap1, lexState1, rest1) <- extractPage state0 acc ccMap0 lexState0 cs
    pagesRest <- extractPages state1 vListRemain ccMap1 lexState1 rest1
    return $ page:pagesRest
