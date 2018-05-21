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

newtype State = State { currentFontInfo :: Maybe TFMM.TexFont } deriving Show

character :: State -> Int -> Maybe B.Character
character state code = do
  font <- currentFontInfo state
  let toSP = TFMM.toScaledPoint font
  TFMC.Character{width=w, height=h, depth=d} <- IMAP.lookup code $ TFMM.characters font
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

extractHElems :: State -> [Lex.LexToken] -> IO (State, [S.HListElement], [Lex.LexToken])
extractHElems s [] = return (s, [], [])
extractHElems s (Lex.CharCat Cat.CharCat {char = char, cat = cat}:rest)
  | cat `elem` [Cat.Letter, Cat.Other] = do
    extra <- case character s $ C.ord char of
      Just c -> return [S.HCharacter c]
      Nothing -> fail "Could not get character info"
    (sNext, nextResult, nextRemain) <- extractHElems s rest
    return (sNext, extra ++ nextResult, nextRemain)
  | cat == Cat.Space = do
    extra <- case currentFontInfo s of
      (Just (font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr})) -> do
        let toSP = TFMM.toScaledPoint font
        return [S.HGlue S.Glue {dimen = toSP d, stretch = S.finiteGlueFlex $ toSP str, shrink = S.finiteGlueFlex $ toSP shr}]
      Nothing ->
        fail "No font selected"
    (sNext, nextResult, nextRemain) <- extractHElems s rest
    return (sNext, extra ++ nextResult, nextRemain)
  | otherwise = extractHElems s rest
extractHElems s (Lex.ControlSequenceCall {name = name}:rest)
  | name == "par" = return (s, [], rest)
  | otherwise = extractHElems s rest

extractVElems :: State -> [Lex.LexToken] -> IO (State, [S.VListElement], [Lex.LexToken])
extractVElems s [] = return (s, [], [])
extractVElems s (a@Lex.CharCat {}:rest) = do
  (sNext, hConts, hRemain) <- extractHElems s (a : rest)
  let 
    width = 28000000
    tolerance = 200
    linePenalty = 10
    bestRouteLists = S.contents <$> S.bestRoute width tolerance linePenalty hConts
    bestRouteBoxes = fmap (S.simpleHSet width) bestRouteLists
    route = intersperse (S.VGlue S.Glue {dimen = 400000, stretch=S.noGlueFlex, shrink=S.noGlueFlex}) bestRouteBoxes
    -- route = superRoute width tolerance linePenalty cs

    down = S.VGlue S.Glue {dimen = 1000000, stretch = S.noGlueFlex, shrink = S.noGlueFlex}
  (sNextNext, nextResult, vRemain) <- extractVElems sNext hRemain
  return (sNextNext, route ++ [down] ++ nextResult, vRemain)
extractVElems s (Lex.ControlSequenceCall {name = name}:rest)
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
        extra = [fontDef, fontSel]
        fontSel = S.VFontSelection B.FontSelection {fontNr = 1}
        sNext = s{currentFontInfo=Just fontInfo}
    (sNextNext, nextResult, nextRemain) <- extractVElems sNext rest
    return (sNextNext, extra ++ nextResult, nextRemain)
  | name == "par" = extractVElems s rest
  | otherwise = extractVElems s rest
