module HeX.Config.Parameters where

import           HeXlude

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

import qualified HeX.BreakList.Glue  as BL.G
import           HeX.Parse.Token
import           HeX.Quantity

newtype IntParamVal (a :: TeXIntParameter) =
    IntParamVal { unIntParam :: TeXInt }
    deriving ( Eq, Enum, Ord, Show, Num, Real, Integral )

newtype LenParamVal (a :: LengthParameter) =
    LenParamVal { unLenParam :: Length }
    deriving ( Eq, Enum, Ord, Show, Num, Real, Integral )

newtype GlueParamVal (a :: GlueParameter) =
    GlueParamVal { unGlueParam :: BL.G.Glue Length }
    deriving ( Show )

newtype MathGlueParamVal (a :: MathGlueParameter) =
    MathGlueParamVal { unMathGlueunMathGlueParam :: BL.G.Glue MathLength }
    deriving ( Show )

newtype TokenListParamVal (a :: TokenListParameter) =
    TokenListParamVal { unTokenListParam :: BalancedText }
    deriving ( Show )

newTeXIntParameters :: Map TeXIntParameter TeXInt
newTeXIntParameters =
    Map.fromList
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

newLengthParameters :: Map LengthParameter Length
newLengthParameters = mempty

newGlueParameters :: Map GlueParameter (BL.G.Glue Length)
newGlueParameters = mempty

newMathGlueParameters :: Map MathGlueParameter (BL.G.Glue MathLength)
newMathGlueParameters = mempty

newTokenListParameters :: Map TokenListParameter BalancedText
newTokenListParameters = mempty

newSpecialTeXInts :: Map SpecialTeXInt TeXInt
newSpecialTeXInts = mempty

newSpecialLengths :: Map SpecialLength Length
newSpecialLengths =
    Map.fromList
        [ (PrevDepth, fromIntegral $ -oneKPt)
        ]

usableTeXIntParameters :: Map TeXIntParameter TeXInt
usableTeXIntParameters =
    Map.union newTeXIntParameters $ Map.fromList
        [ (Tolerance, 500)
        , (LinePenalty, 10)
        , (Mag, 1000)
        ]

usableLengthParameters :: Map LengthParameter Length
usableLengthParameters =
    Map.union newLengthParameters $ Map.fromList
        [ (HSize, 30750000)
        , (VSize, 37500000)
        , (ParIndent, toScaledPointApprox (20 :: Int) Point)
        ]

usableGlueParameters :: Map GlueParameter (BL.G.Glue Length)
usableGlueParameters =
    Map.union newGlueParameters $ Map.fromList
        [ (BaselineSkip , BL.G.fixedGlue $ toScaledPointApprox (12 :: Int) Point )
        , (LineSkip, BL.G.fixedGlue $ toScaledPointApprox (1 :: Int) Point)
        ]
