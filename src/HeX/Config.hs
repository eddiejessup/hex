{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module HeX.Config
    ( module HeX.Config.Parameters
    , Config(..)
    , newConfig
    , ConfReaderT, ConfStateT
    , parIndentBox
    , fontInfoMap
    , modifyParams
    )
where

import           Control.Monad.State.Lazy       ( StateT, modify )
import           Control.Monad.Trans.Reader     ( ReaderT )
import qualified Data.HashMap.Strict           as HMap
import           Data.Maybe                     ( fromJust )
import           Path
import           System.Directory

import           TFM                            ( TexFont )

import qualified Data.Path                       as Pth
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL

import          HeX.Config.Parameters

type FontInfoMap = HMap.HashMap Int TexFont

data Config = Config
  { currentFontNr :: Maybe Int
  , fontInfoMap :: FontInfoMap
  , fontDirectories :: [Pth.AbsPathToDir]
  , params :: ParamConfig
  }

newConfig :: IO Config
newConfig = do
  cwd <- fromJust . parseAbsDir <$> getCurrentDirectory
  pure $
    Config
    { currentFontNr = Nothing
    , fontInfoMap = HMap.empty
    , fontDirectories = [cwd]
    , params = usableParamConfig }

type ConfStateT = StateT Config
type ConfReaderT = ReaderT Config

modifyParams :: Monad m => (ParamConfig -> ParamConfig) -> ConfStateT m ()
modifyParams f = modify (\c@Config{params=ps} -> c {params=f ps})

parIndentBox :: Config -> BL.BreakableHListElem
parIndentBox conf = BL.HVListElem $ BL.ListBox $
  B.Box { contents = B.HBoxContents []
        , desiredLength = B.To $ unLenParam $ parIndent $ params conf }
