{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse where

import qualified Data.Char as C
import Data.IntMap.Strict ((!?))
import qualified Data.IntMap.Strict as IntMap

import qualified Debug.Trace as T

import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified Cat
import qualified Lex

data DesiredLength
  = Natural
  | Spread Int
  | To Int
  deriving (Show)

data VList = VList
  { contents :: [VListElement]
  , desiredLength :: DesiredLength
  } deriving (Show)

data HList = HList
  { contents :: [HListElement]
  , desiredLength :: DesiredLength
  } deriving (Show)

data Rule = Rule
  { width :: Int
  , height :: Int
  , depth :: Int
  } deriving (Show)

data Kern = Kern
  { dimen :: Int
  } deriving (Show)

data FontDefinition = FontDefinition
  { fontNr :: Int
  , fontPath :: FilePath
  , fontName :: FilePath
  , scaleFactorRatio :: Rational
  , fontInfo :: TFMM.TexFont
  } deriving (Show)

data FontSelection = FontSelection
  { fontNr :: Int
  } deriving (Show)

data Character = Character
  { code :: Int
  , width :: Int
  , height :: Int
  , depth :: Int
  } deriving (Show)
              -- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

-- Can't have these things in a box, only a list.
data Glue = Glue
  { dimen :: Int
  , stretch :: Int
  , shrink :: Int
  } deriving (Show)

data Penalty = Penalty
  { size :: Int
  } deriving (Show)
             -- TODO: WhatsIt, Leaders, Mark, Insertion
              -- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

data HListElement
  = HVList VList
  | HHList HList
  | HRule Rule
  | HGlue Glue
  | HKern Kern
  | HPenalty Penalty
  | HFontDefinition FontDefinition
  | HFontSelection FontSelection
  | HCharacter Character
  deriving (Show)

data VListElement
  = VVList VList
  | VHList HList
  | VRule Rule
  | VGlue Glue
  | VKern Kern
  | VPenalty Penalty
  | VFontDefinition FontDefinition
  | VFontSelection FontSelection
  deriving (Show)

isDiscardable :: HListElement -> Bool
isDiscardable a =
  case a of
    HGlue _ -> True
    HKern _ -> True
  -- HLeaders _ -> True
    HPenalty _ -> True
  -- HMathOn _ -> True
  -- HMathOff _ -> True
    a -> False

data State = State { currentFontInfo :: Maybe TFMM.TexFont }


character :: State -> Int -> Maybe Character
character state code = do
  font <- currentFontInfo state
  let toSP = TFMM.toScaledPoint font
  c <- TFMM.characters font !? code
  let (TFMC.Character{width=w, height=h, depth=d}) = c
  return Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

-- TODO: Penalty of MathOff, DiscretionaryBreak.
extractHElems :: State -> [Lex.LexToken] -> IO (State, [HListElement], [Lex.LexToken])
extractHElems s [] = return (s, [], [])
extractHElems s (Lex.CharCat Cat.CharCat {char = char, cat = cat}:rest)
  | cat `elem` [Cat.Letter, Cat.Other] = do
    extra <- case character s $ C.ord char of
      Just c -> return [HCharacter c]
      Nothing -> fail "Could not get character info"
    (s, nextResult, nextRemain) <- extractHElems s rest
    return (s, extra ++ nextResult, nextRemain)
  | cat == Cat.Space = do
    extra <- case currentFontInfo s of
      (Just (font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr})) -> do
        let toSP = TFMM.toScaledPoint font
        return [HGlue Glue {dimen = toSP d, stretch = toSP str, shrink = toSP shr}]
      Nothing ->
        fail "No font selected"
    (s, nextResult, nextRemain) <- extractHElems s rest
    return (s, extra ++ nextResult, nextRemain)
  | otherwise = extractHElems s rest
extractHElems s (Lex.ControlSequenceCall {name = name}:rest)
  | name == "font" = do
    fontInfo <- TFMM.readTFM "cmr10.tfm"
    let fontDef =
          HFontDefinition
            FontDefinition
            { fontNr = 1
            , fontPath = "cmr10.tfm"
            , fontName = "cmr10"
            , fontInfo = fontInfo
            , scaleFactorRatio = 1.0
            }
        extra = [fontDef, fontSel]
        fontSel = HFontSelection FontSelection {fontNr = 1}
        sNew = s{currentFontInfo=Just fontInfo}
    (sNew, nextResult, nextRemain) <- extractHElems sNew rest
    return (sNew, extra ++ nextResult, nextRemain)
  | name == "par" = return (s, [], rest)
  | otherwise = extractHElems s rest

extractVElems :: State -> [Lex.LexToken] -> IO (State, [VListElement], [Lex.LexToken])
extractVElems s [] = return (s, [], [])
extractVElems s (a@Lex.CharCat {}:rest) = do
  (s, hConts, hRemain) <- extractHElems s (a : rest)
  let 
    hList = VHList HList {contents = hConts, desiredLength = Natural}
    down = VGlue Glue {dimen = 1000000, stretch = 0, shrink = 0}
  (s, nextResult, vRemain) <- extractVElems s hRemain
  return (s, hList : down : nextResult, vRemain)
extractVElems s (Lex.ControlSequenceCall {name = name}:rest)
  | name == "font" = do
    fontInfo <- TFMM.readTFM "cmr10.tfm"
    let fontDef =
          VFontDefinition
            FontDefinition
            { fontNr = 1
            , fontPath = "cmr10.tfm"
            , fontName = "cmr10"
            , fontInfo = fontInfo
            , scaleFactorRatio = 1.0
            }
        fontSel = VFontSelection FontSelection {fontNr = 1}
        sNew = s{currentFontInfo=Just fontInfo}
    (sNew, nextResult, nextRemain) <- extractVElems sNew rest
    return (sNew, fontDef : fontSel : nextResult, nextRemain)
  | name == "par" = extractVElems s rest
  | otherwise = extractVElems s rest
