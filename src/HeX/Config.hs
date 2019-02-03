{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HeX.Config
    ( module HeX.Config.Parameters
    , Config(..)
    , FontInfo(..)
    , readFontInfo
    , lookupFontInfo
    , currentFontInfo
    , currentFontMetrics
    , addFont
    , newConfig
    , ConfReaderT, ConfStateT
    , parIndentBox
    , modifyParams
    , setConfIntParam
    , setConfLenParam
    , setConfGlueParam
    -- , setConfMathGlueParam
    -- , setConfTokenListParam
    , setConfSpecialInt
    , setConfSpecialLen
    )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( StateT
                                                , liftIO
                                                , modify
                                                , MonadState
                                                , gets )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Control.Monad.Except           ( MonadError
                                                )
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!?) )
import           Path
import           System.Directory

import qualified TFM
import           TFM                            ( TexFont )

import           HeXPrelude
import           HeX.Type
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Parse.Token
import           HeX.Config.Parameters

data Config = Config
    { currentFontNr   :: Maybe Int
    , fontInfos       :: V.Vector FontInfo
    , fontDirectories :: [Path Abs Dir]
    , params          :: ParamConfig
    } deriving (Show)

newConfig :: IO Config
newConfig =
    do
    cwdRaw <- getCurrentDirectory
    cwd <- parseAbsDir cwdRaw
    pure Config
        { currentFontNr = Nothing
        , fontInfos = V.empty
        , fontDirectories = [cwd]
        , params          = usableParamConfig }

type ConfStateT = StateT Config
type ConfReaderT = ReaderT Config

-- Fonts.

data FontInfo = FontInfo
    { fontMetrics :: TexFont
    , hyphenChar
    , skewChar    :: IntVal
    } deriving (Show)

readFontInfo :: (MonadState Config m, MonadIO m) => Path Abs File -> m FontInfo
readFontInfo fontPath =
    do
    fontMetrics <- liftIO $ TFM.readTFMFancy fontPath
    hyphenChar <- unIntParam <$> gets (defaultHyphenChar . params)
    skewChar <- unIntParam <$> gets (defaultSkewChar . params)
    pure FontInfo{..}

lookupFontInfo :: (MonadState Config m, MonadError String m) => Int -> m FontInfo
lookupFontInfo fNr =
    do
    infos <- gets fontInfos
    liftMaybe "No such font number" $ infos !? fNr

currentFontInfo :: (MonadState Config m, MonadError String m) => m FontInfo
currentFontInfo = gets currentFontNr >>= liftMaybe "Font number isn't set" >>= lookupFontInfo

currentFontMetrics :: (MonadState Config m, MonadError String m) => m TexFont
currentFontMetrics = fontMetrics <$> currentFontInfo

addFont :: MonadState Config m => FontInfo -> m Int
addFont newInfo =
    do
    infos <- gets fontInfos
    let newInfos = V.snoc infos newInfo
    modify (\conf -> conf{fontInfos = newInfos})
    pure $ V.length newInfos - 1

-- Parameters.

modifyParams :: MonadState Config m => (ParamConfig -> ParamConfig) -> m ()
modifyParams f =
    do
    pConf <- gets params
    let modConf _params conf = conf{params=_params}
    modify $ modConf $ f pConf

liftSetParam
    :: MonadState Config m
    => (p -> v -> ParamConfig -> ParamConfig)
    -> p
    -> v
    -> m ()
liftSetParam g p v = modifyParams $ g p v

setConfIntParam :: MonadState Config m => IntegerParameter -> IntVal -> m ()
setConfIntParam = liftSetParam setIntParam

setConfLenParam :: MonadState Config m => LengthParameter -> LenVal -> m ()
setConfLenParam = liftSetParam setLenParam

setConfGlueParam :: MonadState Config m => GlueParameter -> BL.Glue -> m ()
setConfGlueParam = liftSetParam setGlueParam

-- setConfMathGlueParam :: MonadState Config m => MathGlueParameter -> BL.MathGlue -> m ()
-- setConfMathGlueParam = liftSetParam setMathGlueParam

-- setConfTokenListParam :: MonadState Config m => TokenListParameter -> [Token] -> m ()
-- setConfTokenListParam = liftSetParam setTokenListParam

setConfSpecialInt :: MonadState Config m => SpecialInteger -> IntVal -> m ()
setConfSpecialInt = liftSetParam setSpecialInt

setConfSpecialLen :: MonadState Config m => SpecialLength -> LenVal -> m ()
setConfSpecialLen = liftSetParam setSpecialLen

parIndentBox :: Config -> BL.BreakableHListElem
parIndentBox conf =
    BL.HVListElem $ BL.VListBaseElem $ B.ElemBox $ B.Box
        { contents      = B.HBoxContents []
        , desiredLength = B.To . unLenParam . parIndent . params $ conf
        }
