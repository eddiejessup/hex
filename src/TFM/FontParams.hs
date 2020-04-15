module TFM.FontParams where

import           Hexlude

import           Data.Ascii      (AsciiString)
import qualified Data.Binary.Get as B.G

import           TFM.Common

data FontParams = FontParams
    { slant
    , spacing
    , spaceStretch
    , spaceShrink
    , xHeight
    , quad
    , extraSpace :: Rational
    , extraParams :: Maybe ExtraFontParams
    } deriving stock (Show)

data ExtraFontParams
    = MathSymbolFontParams MathSymbolParams
    | MathExtensionFontParams MathExtensionParams
     deriving stock (Show)

data MathSymbolParams = MathSymbolParams
    { num1
    , num2
    , num3
    , denom1
    , denom2
    , sup1
    , sup2
    , sup3
    , sub1
    , sub2
    , supdrop
    , subdrop
    , delim1
    , delim2
    , axisHeight :: Rational
    } deriving stock (Show)

data MathExtensionParams = MathExtensionParams
    { defaultRuleThickness
    , bigOpSpacing1
    , bigOpSpacing2
    , bigOpSpacing3
    , bigOpSpacing4
    , bigOpSpacing5 :: Rational
    } deriving stock (Show)

readMathSymbolParams :: B.G.Get MathSymbolParams
readMathSymbolParams = MathSymbolParams
    <$> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord

readMathExtensionParams :: B.G.Get MathExtensionParams
readMathExtensionParams = MathExtensionParams
    <$> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord
    <*> getFixWord

getFontParams :: Maybe AsciiString -> B.G.Get FontParams
getFontParams scheme =
    FontParams
        <$> getFixWord
        <*> getFixWord
        <*> getFixWord
        <*> getFixWord
        <*> getFixWord
        <*> getFixWord
        <*> getFixWord
        <*> case scheme of
            Just "TeX math symbols" ->
                Just . MathSymbolFontParams <$> readMathSymbolParams
            Just "TeX math extension" ->
                Just . MathExtensionFontParams <$> readMathExtensionParams
            _ ->
                pure Nothing
