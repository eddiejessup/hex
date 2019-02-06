{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module HeX.Config.Config where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( StateT
                                                , liftIO
                                                , modify
                                                , MonadState
                                                , gets )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Control.Monad.Except           ( MonadError
                                                )

import           Data.Hashable
import qualified Data.HashMap.Strict           as HMap
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!?) )
import           Path
import           Safe                           ( toEnumMay )
import           System.Directory

import qualified TFM
import           TFM                            ( TexFont )

import           HeXPrelude
import           HeX.Type
import qualified HeX.Categorise                as Cat
import qualified HeX.Lex                       as Lex
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Parse.Token
import           HeX.Config.Parameters
import           HeX.Config.Codes

type RegisterMap v = HMap.HashMap EightBitInt v

data Config = Config
    { currentFontNr     :: Maybe Int
    , fontInfos         :: V.Vector FontInfo
    , searchDirectories :: [Path Abs Dir]
    , params            :: ParamConfig
    -- Char-code attribute maps.
    , catCodeMap        :: Cat.CharCatMap
    , mathCodeMap       :: Cat.CharCodeMap MathCode
    , lowercaseMap
    , uppercaseMap      :: Cat.CharCodeMap CaseChangeCode
    , spaceFactorMap    :: Cat.CharCodeMap SpaceFactorCode
    , delimiterCodeMap  :: Cat.CharCodeMap DelimiterCode
    -- Registers.
    , integerRegister   :: RegisterMap IntVal
    , lengthRegister    :: RegisterMap LenVal
    , glueRegister      :: RegisterMap BL.Glue
    -- , mathGlueRegister  :: RegisterMap MathGlue
    , tokenListRegister :: RegisterMap [Lex.Token]
    -- , boxRegister       :: RegisterMap (Maybe Box)
    } deriving (Show)

newConfig :: IO Config
newConfig =
    do
    cwdRaw <- getCurrentDirectory
    cwd <- parseAbsDir cwdRaw
    pure Config
        { currentFontNr     = Nothing
        , fontInfos         = V.empty
        , searchDirectories = [cwd]
        , params            = usableParamConfig
        , catCodeMap        = Cat.usableCharCatMap
        , mathCodeMap       = newMathCodeMap
        , lowercaseMap      = newLowercaseMap
        , uppercaseMap      = newUppercaseMap
        , spaceFactorMap    = newSpaceFactorMap
        , delimiterCodeMap  = newDelimiterCodeMap
        , integerRegister   = HMap.empty
        , lengthRegister    = HMap.empty
        , glueRegister      = HMap.empty
        -- , mathGlueRegister  = HMap.empty
        , tokenListRegister = HMap.empty
        -- , boxRegister       = HMap.empty
        }

fillMap :: (Hashable k, Enum k, Bounded k, Eq k) => v -> HMap.HashMap k v
fillMap v = HMap.fromList $ (, v) <$> [minBound..]

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

-- Codes.

updateCharCodeMap
    :: (MonadError String m, MonadState Config m)
    => CodeType
    -> Cat.CharCode
    -> IntVal
    -> m ()
updateCharCodeMap t c n =
    modify =<< case t of
        CategoryCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure (\cnf -> cnf{catCodeMap=insert v $ catCodeMap cnf})
        MathCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure (\cnf -> cnf{mathCodeMap=insert v $ mathCodeMap cnf})
        ChangeCaseCodeType dir ->
            do
            v <- liftMay $ toEnumMay n
            pure $ case dir of
                Upward -> (\cnf -> cnf{uppercaseMap=insert v $ uppercaseMap cnf})
                Downward -> (\cnf -> cnf{lowercaseMap=insert v $ lowercaseMap cnf})
        SpaceFactorCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure (\cnf -> cnf{spaceFactorMap=insert v $ spaceFactorMap cnf})
        DelimiterCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure (\cnf -> cnf{delimiterCodeMap=insert v $ delimiterCodeMap cnf})
  where
    insert = HMap.insert c
    liftMay f = liftMaybe ("Invalid target value for code type " ++ show t ++ ": " ++ show n) f

-- Registers.

setIntegerRegister :: MonadState Config m => EightBitInt -> IntVal -> m ()
setIntegerRegister idx v =
    modify (\c -> c{integerRegister=HMap.insert idx v $ integerRegister c})

setLengthRegister :: MonadState Config m => EightBitInt -> LenVal -> m ()
setLengthRegister idx v =
    modify (\c -> c{lengthRegister=HMap.insert idx v $ lengthRegister c})

setGlueRegister :: MonadState Config m => EightBitInt -> BL.Glue -> m ()
setGlueRegister idx v =
    modify (\c -> c{glueRegister=HMap.insert idx v $ glueRegister c})
