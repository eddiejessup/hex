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
import           Data.Char                    ( chr
                                              , toLower
                                              , toUpper
                                              )
import qualified Data.HashMap.Strict           as HMap
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!?) )
import           Path
import           System.Directory

import qualified TFM
import           TFM                            ( TexFont )

import           HeXPrelude
import qualified HeX.Categorise                as Cat
import           HeX.Type
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Parse.Token
import           HeX.Config.Parameters

data FamilyCharRef = FamilyCharRef { family :: IntVal, position :: Cat.CharCode }
    deriving (Show)

data DelimiterVar
    = PresentDelimiterVar FamilyCharRef
    | NullDelimiterVar
    deriving (Show)

data DelimiterSpec = DelimiterSpec { smallVar, largeVar :: DelimiterVar }
    deriving (Show)

data DelimiterCode
    = NotADelimiter
    | DelimiterSpecCode DelimiterSpec
    deriving (Show)

data MathClass
    = Ordinary        -- 0
    | LargeOperator   -- 1
    | BinaryRelation  -- 2
    | Relation        -- 3
    | Opening         -- 4
    | Closing         -- 5
    | Punctuation     -- 6
    | VariableFamily  -- 7
    deriving (Show)

data MathCode
    = NormalMathCode MathClass FamilyCharRef
    | ActiveMathCode
    deriving (Show)

data CaseChange
    = NoCaseChange
    | ChangeToCode Cat.CharCode
    deriving (Show)

data Config = Config
    { currentFontNr     :: Maybe Int
    , fontInfos         :: V.Vector FontInfo
    , searchDirectories :: [Path Abs Dir]
    , params            :: ParamConfig
    -- Char-code attribute maps.
    , catCodeMap        :: Cat.CharCatMap
    , mathCodeMap       :: Cat.CharCodeMap MathCode
    , lowercaseMap
    , uppercaseMap      :: Cat.CharCodeMap CaseChange
    , spaceFactorMap    :: Cat.CharCodeMap IntVal
    , delimiterCodeMap  :: Cat.CharCodeMap DelimiterCode
    } deriving (Show)

initialiseCharCodeMap :: (Cat.CharCode -> v) -> Cat.CharCodeMap v
initialiseCharCodeMap val = HMap.fromList $ ((\c -> (c, val c)) . chr) <$> [0 .. 127]

digits :: [Char]
digits = ['1'..'9']

lowerLetters :: [Char]
lowerLetters = ['a'..'z']

upperLetters :: [Char]
upperLetters = ['A'..'Z']

letters :: [Char]
letters = lowerLetters ++ upperLetters

-- The ten digits have \mathcode x = x + "7000.
-- The 52 letters have \mathcode x = x + "7100.
-- Otherwise,          \mathcode x = x
-- Put otherwise: letters are class 7, family 1; digits are class 7, family 0.
newMathCodeMap :: Cat.CharCodeMap MathCode
newMathCodeMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` digits  = NormalMathCode VariableFamily (FamilyCharRef 0 c)
        | c `elem` letters = NormalMathCode VariableFamily (FamilyCharRef 1 c)
        | otherwise        = NormalMathCode Ordinary       (FamilyCharRef 0 c)

-- By default, all \uccode and \lccode values are zero except that the
-- letters a to z and A to Z have \uccode values A to Z and \lccode values a to
-- z.
newLowercaseMap :: Cat.CharCodeMap CaseChange
newLowercaseMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` letters = ChangeToCode $ toLower c
        | otherwise        = NoCaseChange

newUppercaseMap :: Cat.CharCodeMap CaseChange
newUppercaseMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` letters = ChangeToCode $ toUpper c
        | otherwise        = NoCaseChange

-- By default, all characters have a space factor code of 1000, except that the
-- uppercase letters ‘A’ through ‘Z’ have code 999.
newSpaceFactorMap :: Cat.CharCodeMap IntVal
newSpaceFactorMap = initialiseCharCodeMap f
  where
    f c
        | c `elem` ['A' .. 'Z'] = 999
        | otherwise             = 1000

-- All delcodes are −1 until they are changed by a \delcode command.
newDelimiterCodeMap :: Cat.CharCodeMap DelimiterCode
newDelimiterCodeMap = initialiseCharCodeMap $ const NotADelimiter

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
        }

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
