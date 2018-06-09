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

import qualified Debug.Trace as T

newtype State = State { currentFontInfo :: Maybe TFMM.TexFont } deriving Show

characterBox :: State -> Int -> Maybe B.Character
characterBox state code = do
  font <- currentFontInfo state
  let toSP = TFMM.toScaledPoint font
  TFMC.Character{width=w, height=h, depth=d} <- IMAP.lookup code $ TFMM.characters font
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

spaceGlue :: State -> Maybe S.Glue
spaceGlue state = do
  font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr} <- currentFontInfo state
  let
    toSP = TFMM.toScaledPoint font
    toFlex = S.finiteGlueFlex . toSP
  return S.Glue{dimen=toSP d, stretch=toFlex str, shrink=toFlex shr}

extractParagraph :: State -> [S.BreakableHListElem] -> Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> IO (State, [S.BreakableHListElem], Cat.CharCatMap, Lex.LexState, [Cat.CharCode])
-- Run out of characters: return the list so far.
extractParagraph state0 acc ccMap0 lexState0 [] = return (state0, acc, ccMap0, lexState0, [])
extractParagraph state0 acc ccMap0 lexState0 cs
  | C.AddCharacter{code=i} <- com1 = do
    charBox <- case characterBox state0 i of
      Just c -> return $ S.HCharacter c
      Nothing -> fail "Could not get character info"
    extractParagraph state0 (charBox:acc) ccMap0 lexState1 rest1
  | C.AddSpace{} <- com1 = do
    glue <- case spaceGlue state0 of
      Just sg -> return $ S.HGlue sg
      Nothing -> fail "Could not get space glue"
    extractParagraph state0 (glue:acc) ccMap0 lexState1 rest1
  -- See com1 \par: Return to outer mode with completed list.
  | C.StartParagraph{} <- com1 = return (state0, acc, ccMap0, lexState1, rest1)
  -- See something else: Ignore and continue.
  | otherwise = extractParagraph state0 acc ccMap0 lexState1 rest1
  where
    (com1, rest1, lexState1) = C.extractCommand ccMap0 lexState0 cs

extractBoxedParagraph :: Int -> Int -> Int -> S.Glue -> State -> Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> IO (State, [S.BreakableVListElem], Cat.CharCatMap, Lex.LexState, [Cat.CharCode])
extractBoxedParagraph desiredWidth lineTolerance linePenalty interLineGlue state ccMap lexState cs = do
  (stateNext, hList, ccMapNext, lexStateNext, rest) <- extractParagraph state [] ccMap lexState cs
  let
    lineBoxes = S.setParagraph desiredWidth lineTolerance linePenalty hList
    paraBoxes = intersperse (S.VGlue interLineGlue) lineBoxes
  return (stateNext, paraBoxes, ccMapNext, lexStateNext, rest)

desiredWidth :: Int
desiredWidth = 28000000

lineTolerance :: Int
lineTolerance = 200

linePenalty :: Int
linePenalty = 10

desiredHeight :: Int
desiredHeight = 60000000

interLineGlue :: S.Glue
interLineGlue = S.Glue{dimen = 400000, stretch=S.noGlueFlex, shrink=S.noGlueFlex}

interParGlue :: S.Glue
interParGlue = S.Glue {dimen = 1000000, stretch = S.noGlueFlex, shrink = S.noGlueFlex}

startsParagraph :: C.Command -> Bool
startsParagraph C.AddCharacter{} = True
startsParagraph C.AddSpace{} = True
startsParagraph _ = False

extractPage :: State -> [S.BreakableVListElem] -> Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> IO (State, B.Page, [S.BreakableVListElem], Cat.CharCatMap, Lex.LexState, [Cat.CharCode])
extractPage state0 acc ccMap0 lexState0 [] = return (state0, B.Page $ S.setListElems S.NaturallyGood acc, [], ccMap0, lexState0, [])
extractPage state0 acc ccMap0 lexState0 cs
  | startsParagraph com1 = do
    (state2, paraBoxes, ccMap2, lexState2, rest2) <- extractBoxedParagraph desiredWidth lineTolerance linePenalty interLineGlue state0 ccMap0 lexState0 cs
    let
      breakItem = S.GlueBreak interParGlue
      extra = paraBoxes ++ [S.VGlue interParGlue]
      -- TODO: Discard when adding to empty page.
      -- TODO: Keep best rather than taking last.
      pen = S.breakPenalty breakItem
      stat = S.listGlueSetRatio desiredHeight $ acc ++ extra
      bad = S.listStatusBadness stat
      cost = T.traceShow stat $ S.pageCost pen bad 0
      page = B.Page $ S.setListElems stat acc
    if (cost == S.oneMillion) || (pen <= -S.tenK)
      then return (state2, page, extra, ccMap2, lexState2, rest2)
      else extractPage state2 (acc ++ extra) ccMap2 lexState2 rest2
  | C.Assign {assignment=C.SelectFont} <- com1 = do
    fontInfo <- TFMM.readTFM "cmr10.tfm"
    let fontDef =
          S.VFontDefinition
            B.FontDefinition
            { fontNr = 1
            , fontPath = "cmr10.tfm"
            , fontName = "cmr10"
            , fontInfo = fontInfo
            , scaleFactorRatio = 1.0
            }
        fontSel = S.VFontSelection B.FontSelection {fontNr = 1}
    extractPage state0{currentFontInfo=Just fontInfo} (acc ++ [fontDef, fontSel]) ccMap0 lexState1 rest1
  | otherwise = extractPage state0 acc ccMap0 lexState1 rest1
  where
    (com1, rest1, lexState1) = C.extractCommand ccMap0 lexState0 cs

extractPages :: State -> [S.BreakableVListElem] -> Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> IO [B.Page]
extractPages state0 [] ccMap0 lexState0 [] = return []
extractPages state0 acc ccMap0 lexState0 cs = do
    (state1, page, vListRemain, ccMap1, lexState1, rest1) <- extractPage state0 acc ccMap0 lexState0 cs
    pagesRest <- extractPages state1 vListRemain ccMap1 lexState1 rest1
    return $ page:pagesRest
