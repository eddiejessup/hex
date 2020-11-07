module Hex.Config.Parameters where

import           Hexlude

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import qualified Hex.BreakList.Glue  as BL.G
import           Hex.Resolve.Token
import           Hex.Quantity

newtype IntParamVal (a :: TeXIntParameter) =
    IntParamVal { unIntParam :: TeXInt }
    deriving newtype ( Eq, Enum, Ord, Show, Num, Real, Integral )

newtype LenParamVal (a :: LengthParameter) =
    LenParamVal { unLenParam :: Length }
    deriving newtype ( Eq, Enum, Ord, Show, Num, Real, Integral )

newtype GlueParamVal (a :: GlueParameter) =
    GlueParamVal { unGlueParam :: BL.G.Glue Length }
    deriving newtype (Show)

newtype MathGlueParamVal (a :: MathGlueParameter) =
    MathGlueParamVal { unMathGlueunMathGlueParam :: BL.G.Glue MathLength }
    deriving newtype (Show)

newtype TokenListParamVal (a :: TokenListParameter) =
    TokenListParamVal { unTokenListParam :: BalancedText }
    deriving newtype (Show)

newTeXIntParameters :: HashMap TeXIntParameter TeXInt
newTeXIntParameters =
    HashMap.fromList
        [ (Tolerance, 10000)
        , (EscapeChar, 92)  -- '\'
        , (EndLineChar, 13)  -- '\r'
        , (MaxDeadCycles, 25)
        , (HangAfter, 1)
        , (Mag, 1000)
        , (Time, 1)
        , (Day, 1)
        , (Month, 1)
        , (Year, 1970)
        ]

newLengthParameters :: HashMap LengthParameter Length
newLengthParameters = mempty

newGlueParameters :: HashMap GlueParameter (BL.G.Glue Length)
newGlueParameters = mempty

newMathGlueParameters :: HashMap MathGlueParameter (BL.G.Glue MathLength)
newMathGlueParameters = mempty

newTokenListParameters :: HashMap TokenListParameter BalancedText
newTokenListParameters = mempty

newSpecialTeXInts :: HashMap SpecialTeXInt TeXInt
newSpecialTeXInts = mempty

newSpecialLengths :: HashMap SpecialLength Length
newSpecialLengths =
    HashMap.fromList
        [ (PrevDepth, fromIntegral $ -oneKPt)
        ]

usableTeXIntParameters :: HashMap TeXIntParameter TeXInt
usableTeXIntParameters =
    HashMap.union newTeXIntParameters $ HashMap.fromList
        [ (Tolerance, 500)
        , (LinePenalty, 10)
        , (Mag, 1000)
        ]

usableLengthParameters :: HashMap LengthParameter Length
usableLengthParameters =
    HashMap.union newLengthParameters $ HashMap.fromList
        [ (HSize, 30750000)
        , (VSize, 37500000)
        , (ParIndent, toScaledPointApprox (20 :: Int) Point)
        ]

usableGlueParameters :: HashMap GlueParameter (BL.G.Glue Length)
usableGlueParameters =
    HashMap.union newGlueParameters $ HashMap.fromList
        [ (BaselineSkip , BL.G.fixedGlue $ toScaledPointApprox (12 :: Int) Point )
        , (LineSkip, BL.G.fixedGlue $ toScaledPointApprox (1 :: Int) Point)
        ]
