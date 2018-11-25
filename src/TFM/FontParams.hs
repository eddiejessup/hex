module TFM.FontParams where

-- Define some structures containing core parameters of the font, and some
import Control.Monad
import Data.Binary
import Data.ByteString hiding (elem, pack)
import Data.ByteString.Char8 (pack)

import TFM.Common

-- Character coding schemes that should have the corresponding extra sets of
-- font parameters.
mathSymbolSchemes :: [ByteString]
mathSymbolSchemes = fmap pack ["TeX math symbols"]

mathExtensionSchemes :: [ByteString]
mathExtensionSchemes =
  fmap pack ["TeX math extension", "euler substitutions only"]

-- optional extra sets of parameters.
data FontParams = FontParams
  { slant :: Rational
  , spacing :: Rational
  , spaceStretch :: Rational
  , spaceShrink :: Rational
  , xHeight :: Rational
  , quad :: Rational
  , extraSpace :: Rational
  , mathSymbolParams :: Maybe MathSymbolParams
  , mathExtensionParams :: Maybe MathExtensionParams
  } deriving (Show)

data MathSymbolParams = MathSymbolParams
  { num1 :: Rational
  , num2 :: Rational
  , num3 :: Rational
  , denom1 :: Rational
  , denom2 :: Rational
  , sup1 :: Rational
  , sup2 :: Rational
  , sup3 :: Rational
  , sub1 :: Rational
  , sub2 :: Rational
  , supdrop :: Rational
  , subdrop :: Rational
  , delim1 :: Rational
  , delim2 :: Rational
  , axisHeight :: Rational
  } deriving (Show)

data MathExtensionParams = MathExtensionParams
  { defaultRuleThickness :: Rational
  , bigOpSpacing :: [Rational]
  } deriving (Show)

readMathSymbolParams :: ByteString -> Get (Maybe MathSymbolParams)
readMathSymbolParams scheme =
  if scheme `elem` mathSymbolSchemes
    then do
      [_num1, _num2, _num3, _denom1, _denom2, _sup1, _sup2, _sup3, _sub1, _sub2, _supdrop, _subdrop, _delim1, _delim2, _axisHeight] <-
        replicateM 15 getFixWord
      pure $
        Just
          MathSymbolParams
          { num1 = _num1
          , num2 = _num2
          , num3 = _num3
          , denom1 = _denom1
          , denom2 = _denom2
          , sup1 = _sup1
          , sup2 = _sup2
          , sup3 = _sup3
          , sub1 = _sub1
          , sub2 = _sub2
          , supdrop = _supdrop
          , subdrop = _subdrop
          , delim1 = _delim1
          , delim2 = _delim2
          , axisHeight = _axisHeight
          }
    else pure Nothing

readMathExtensionParams :: ByteString -> Get (Maybe MathExtensionParams)
readMathExtensionParams scheme =
  if scheme `elem` mathExtensionSchemes
    then do
      _defaultRuleThickness <- getFixWord
      _bigOpSpacing <- replicateM 5 getFixWord
      pure $
        Just
          MathExtensionParams
          { defaultRuleThickness = _defaultRuleThickness
          , bigOpSpacing = _bigOpSpacing
          }
    else pure Nothing

getFontParams :: ByteString -> Get FontParams
getFontParams scheme = do
  when (scheme == pack "TeX math italic") $
    fail "Unsupported character coding scheme"
  [_slant, _spacing, _spaceStretch, _spaceShrink, _xHeight, _quad, _extraSpace] <-
    replicateM 7 getFixWord
    -- Read parameters relating to math symbols and extensions, if present.
  _mathSymbolParams <- readMathSymbolParams scheme
  _mathExtensionParams <- readMathExtensionParams scheme
  return
    FontParams
    { slant = _slant
    , spacing = _spacing
    , spaceStretch = _spaceStretch
    , spaceShrink = _spaceShrink
    , xHeight = _xHeight
    , quad = _quad
    , extraSpace = _extraSpace
    , mathSymbolParams = _mathSymbolParams
    , mathExtensionParams = _mathExtensionParams
    }
