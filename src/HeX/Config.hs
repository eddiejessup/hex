{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module HeX.Config
    ( module HeX.Config.Parameters
    , Config(..)
    , currentFontInfo
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

import           Control.Monad.State.Lazy       ( StateT
                                                , modify
                                                , MonadState
                                                , gets )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Control.Monad.Except           ( MonadError
                                                )
import qualified Data.HashMap.Strict           as HMap
import           Data.Maybe                     ( fromJust )
import           Path
import           System.Directory

import           TFM                            ( TexFont )

import qualified Data.Path                     as Path

import           HeXPrelude
import           HeX.Type
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Parse.Token
import           HeX.Config.Parameters

type FontInfoMap = HMap.HashMap Int TexFont

data Config = Config
    { currentFontNr   :: Maybe Int
    , fontInfoMap     :: FontInfoMap
    , fontDirectories :: [Path.AbsPathToDir]
    , params          :: ParamConfig
    }

newConfig :: IO Config
newConfig =
    do
    cwd <- fromJust . parseAbsDir <$> getCurrentDirectory
    pure Config
        { currentFontNr   = Nothing
        , fontInfoMap     = HMap.empty
        , fontDirectories = [cwd]
        , params          = usableParamConfig }

type ConfStateT = StateT Config
type ConfReaderT = ReaderT Config

currentFontInfo :: (MonadState Config m, MonadError String m) => m TexFont
currentFontInfo =
    do
    -- Maybe font number isn't set.
    fNr <- gets currentFontNr >>= liftMaybe "Font number isn't set"
    -- Or maybe there's no font where there should be.
    -- TODO: I think I can make this case impossible, maybe by storing the
    -- current font info directly instead of a lookup.
    gets fontInfoMap >>= (liftMaybe "No such font number" . (HMap.lookup fNr))

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
