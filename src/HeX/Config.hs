{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module HeX.Config where

import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.State.Lazy       ( StateT )
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader     ( ReaderT )
import qualified Data.HashMap.Strict           as HMap
import           Data.Maybe                     ( fromJust )
import           Path
import           System.Directory

import           TFM                            ( TexFont )

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import qualified HeX.Unit                      as Unit

type FontInfoMap = HMap.HashMap Int TexFont

-- Integers.
newtype LineTolerance = LineTolerance { unLineTolerance :: Int } deriving (Eq, Show, Num)
newtype LinePenalty = LinePenalty { unLinePenalty :: Int } deriving (Eq, Show, Num)
newtype Magnification = Magnification { unMagnification :: Int } deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

-- Lengths.
newtype DesiredWidth = DesiredWidth { unDesiredWidth :: Int } deriving (Eq, Show, Num)
newtype DesiredHeight = DesiredHeight { unDesiredHeight :: Int } deriving (Eq, Show, Num)
-- Minimum distance between baselines.
newtype BaselineLengthMin = BaselineLengthMin { unBaselineLengthMin :: Int } deriving (Eq, Show, Num)
newtype ParIndent = ParIndent { unParIndent :: Int } deriving (Eq, Show, Num)

-- Glues.
-- Aimed actual distance between baselines.
newtype BaselineGlue = BaselineGlue { unBaselineGlue :: BL.Glue } deriving (Show)
newtype MinBaselineGlue = MinBaselineGlue { unMinBaselineGlue :: BL.Glue } deriving (Show)

-- Special integers.
newtype PreviousBoxDepth = PreviousBoxDepth { unPreviousBoxDepth :: Int } deriving (Show)

data Config = Config
  { currentFontNr :: Maybe Int
  , fontInfoMap :: FontInfoMap
  , fontDirectories :: [AbsPathToDir]

  , lineTolerance :: LineTolerance
  , linePenalty :: LinePenalty
  , magnification :: Magnification

  , desiredWidth :: DesiredWidth
  , desiredHeight :: DesiredHeight
  , baselineLengthMin :: BaselineLengthMin
  , parIndent :: ParIndent

  , baselineGlue :: BaselineGlue
  , minBaselineGlue :: MinBaselineGlue

  , previousBoxDepth :: PreviousBoxDepth
  }

type ConfStateT = StateT Config

type ConfReaderT = ReaderT Config

newConfig :: IO Config
newConfig = do
  cwd <- fromJust . parseAbsDir <$> getCurrentDirectory
  pure $
    Config
    { currentFontNr = Nothing
    , fontInfoMap = HMap.empty
    , fontDirectories = [cwd]
    , lineTolerance = LineTolerance 500
    , linePenalty = LinePenalty 10
    , magnification = Magnification 1000
    , desiredWidth = DesiredWidth 30750000
    , desiredHeight = DesiredHeight 37500000
    , baselineLengthMin = BaselineLengthMin 0
    , parIndent = ParIndent $ Unit.toScaledPointApprox (20 :: Int) Unit.Point
    , baselineGlue = BaselineGlue $ BL.Glue (Unit.toScaledPointApprox (12 :: Int) Unit.Point) BL.noFlex BL.noFlex
    , minBaselineGlue = MinBaselineGlue $ BL.Glue (Unit.toScaledPointApprox (1 :: Int) Unit.Point) BL.noFlex BL.noFlex
    , previousBoxDepth = PreviousBoxDepth $ -Unit.oneKPt
    }

parIndentBox :: Config -> BL.BreakableHListElem
parIndentBox conf = BL.HVListElem $ BL.ListBox $
  B.Box { contents = B.HBoxContents []
        , desiredLength = B.To $ unParIndent $ parIndent conf }

-- Path stuff
type PathToFile b = Path b File

type RelPathToFile = PathToFile Rel

type AbsPathToDir = Path Abs Dir

firstExistingPath :: [PathToFile b] -> MaybeT IO (PathToFile b)
firstExistingPath ps
  -- Make a MaybeT of...
 =
  MaybeT $
    -- The result of an IO function, lifted to our MaybeT IO monad.
  liftIO $
      -- namely, 'find', but using a predicate which acts in the IO monad,
      -- and which tests if a file exists.
  findM (doesFileExist . toFilePath) ps

findFilePath :: RelPathToFile -> [AbsPathToDir] -> MaybeT IO (PathToFile Abs)
findFilePath name dirs = firstExistingPath $ fmap (</> name) dirs
