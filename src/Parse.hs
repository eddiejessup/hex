{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse where

import qualified Data.Char as C
import qualified Data.IntMap.Strict as IMAP
import Data.List (intersperse)

import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified Cat
import qualified Lex
import qualified Box as B
import qualified Setting as S

import qualified Debug.Trace as T

newtype State = State { currentFontInfo :: Maybe TFMM.TexFont } deriving Show

character :: State -> Int -> Maybe B.Character
character state code = do
  font <- currentFontInfo state
  let toSP = TFMM.toScaledPoint font
  TFMC.Character{width=w, height=h, depth=d} <- IMAP.lookup code $ TFMM.characters font
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

-- Returns: state, paragraph's-worth of h-list elements, remaining tokens.
extractParagraph :: State -> [Lex.LexToken] -> IO (State, [S.BreakableHListElem], [Lex.LexToken])
extractParagraph s [] = return (s, [], [])
extractParagraph s (Lex.CharCat Cat.CharCat {char = char, cat = cat}:rest)
  | cat `elem` [Cat.Letter, Cat.Other] = do
    extra <- case character s $ C.ord char of
      Just c -> return [S.HCharacter c]
      Nothing -> fail "Could not get character info"
    (sNext, nextResult, nextRemain) <- extractParagraph s rest
    return (sNext, extra ++ nextResult, nextRemain)
  | cat == Cat.Space = do
    extra <- case currentFontInfo s of
      (Just (font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr})) -> do
        let toSP = TFMM.toScaledPoint font
        return [S.HGlue S.Glue {dimen = toSP d, stretch = S.finiteGlueFlex $ toSP str, shrink = S.finiteGlueFlex $ toSP shr}]
      Nothing ->
        fail "No font selected"
    (sNext, nextResult, nextRemain) <- extractParagraph s rest
    return (sNext, extra ++ nextResult, nextRemain)
  | otherwise = extractParagraph s rest
extractParagraph s (Lex.ControlSequenceCall {name = name}:rest)
  | name == "par" = return (s, [], rest)
  | otherwise = extractParagraph s rest

-- Returns: state, a page of set v-box elements, remaining v-list elements, remaining tokens.
extractPage :: State -> [Lex.LexToken] -> [S.BreakableVListElem] -> IO (State, B.Page, [S.BreakableVListElem], [Lex.LexToken])
extractPage s [] acc = return (s, B.Page $ S.setListElems S.NaturallyGood acc, [], [])
extractPage s (a@Lex.CharCat {}:rest) acc = do
  (sNext, hConts, hRemain) <- extractParagraph s (a : rest)
  let 
    width = 28000000
    tolerance = 200
    linePenalty = 10
    lineBoxes = S.setParagraph width tolerance linePenalty hConts
    paraBoxes = intersperse (S.VGlue S.Glue{dimen = 400000, stretch=S.noGlueFlex, shrink=S.noGlueFlex}) lineBoxes
    downGlue = S.Glue {dimen = 1000000, stretch = S.noGlueFlex, shrink = S.noGlueFlex}
    down = S.VGlue downGlue
    breakItem = S.GlueBreak downGlue
    extra = paraBoxes ++ [down]

    height = 60000000
    -- TODO: Discard when adding to empty page.
    -- TODO: Keep best rather than taking last.
    pen = S.breakPenalty breakItem
    stat = S.listGlueSetRatio height $ acc ++ extra
    bad = S.listStatusBadness stat
    cost = T.traceShow stat $ S.pageCost pen bad 0
    page = B.Page $ S.setListElems stat acc
  if (cost == S.oneMillion) || (pen <= -S.tenK)
    then return (sNext, page, extra, hRemain)
    else extractPage sNext hRemain (acc ++ extra)
extractPage s (Lex.ControlSequenceCall{name = name}:rest) acc
  | name == "font" = do
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
        extra = [fontDef, fontSel]
        sNext = s{currentFontInfo=Just fontInfo}
        accNext = acc ++ extra
    extractPage sNext rest accNext
  | name == "par" = extractPage s rest acc
  | otherwise = extractPage s rest acc

-- Returns: state, some pages.
extractPages :: State -> [Lex.LexToken] -> [S.BreakableVListElem] -> IO (State, [B.Page])
extractPages s [] [] = return (s, [])
extractPages state tokens acc = do
    (stateNext, page, vListRemain, tokensNext) <- extractPage state tokens acc
    (stateEnd, pagesRest) <- extractPages stateNext tokensNext vListRemain
    return (stateEnd, page:pagesRest)
