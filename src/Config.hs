{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Config where

import qualified Data.HashMap.Strict as HMap
import Control.Monad.State.Lazy (StateT)
import Control.Monad.Trans.Reader (ReaderT)

import qualified TFM.Main as TFMM

import qualified BoxDraw as B
import qualified Arrange as A
import qualified Unit

type FontInfoMap = HMap.HashMap Int TFMM.TexFont

data IntegerParameterName
  = LineTolerance
  | LinePenalty
  | Magnification
  deriving (Show, Enum, Bounded, Eq)

data LengthParameterName
  = DesiredWidth
  | DesiredHeight
    -- Minimum distance between baselines.
  | BaselineLengthMin
  | ParIndent
  deriving (Show, Enum, Bounded, Eq)

data GlueParameterName
  -- Aimed actual distance between baselines.
  = BaselineGlue
  | MinBaselineGlue
  deriving (Show, Enum, Bounded, Eq)

data SpecialIntegerParameterName
  = PreviousBoxDepth
  deriving (Show, Enum, Bounded, Eq)

instance (Enum a, Bounded a, Show a, Show b) => Show (a -> b) where
  show f = show $ fmap (\p -> (p, f p)) [minBound ..]

data Config = Config { currentFontNr :: Maybe Int
                     , fontInfoMap :: FontInfoMap

                     , integerParameter :: IntegerParameterName -> Int
                     , lengthParameter :: LengthParameterName -> Int
                     , glueParameter :: GlueParameterName -> A.Glue

                     , specialIntegerParameter :: SpecialIntegerParameterName -> Int } deriving Show

type ConfStateT = StateT Config
type ConfReaderT = ReaderT Config

newConfig :: Config
newConfig = Config { currentFontNr=Nothing
                   , fontInfoMap=HMap.empty

                   , integerParameter=newIntegerParameter
                   , lengthParameter=newLengthParameter
                   , glueParameter=newGlueParameter

                   , specialIntegerParameter=newSpecialIntegerParameter }

parIndentBox :: Config -> A.BreakableHListElem
parIndentBox conf =
  A.HHBox B.HBox{contents=[] , desiredLength=B.To $ conf `lengthParameter` ParIndent}

newIntegerParameter :: IntegerParameterName -> Int
newIntegerParameter LineTolerance = 500
newIntegerParameter LinePenalty = 10
newIntegerParameter Magnification = 1000

newLengthParameter :: LengthParameterName -> Int
newLengthParameter DesiredWidth = 30750000
newLengthParameter DesiredHeight = 37500000
newLengthParameter BaselineLengthMin = 0
newLengthParameter ParIndent = Unit.toScaledPointApprox (20 :: Int) Unit.Point

newGlueParameter :: GlueParameterName -> A.Glue
newGlueParameter BaselineGlue = A.Glue (Unit.toScaledPointApprox (12 :: Int) Unit.Point) A.noFlex A.noFlex
newGlueParameter MinBaselineGlue = A.Glue (Unit.toScaledPointApprox (1 :: Int) Unit.Point) A.noFlex A.noFlex

newSpecialIntegerParameter :: SpecialIntegerParameterName -> Int
newSpecialIntegerParameter PreviousBoxDepth = -Unit.oneKPt

updateFuncMap :: Eq a => (a -> b) -> a -> b -> (a -> b)
updateFuncMap f k v   k' = if k' == k then v else f k'
