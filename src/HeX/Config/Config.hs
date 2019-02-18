{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module HeX.Config.Config where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( liftIO
                                                , modify
                                                , MonadState
                                                , gets )
import           Control.Monad.Reader           ( MonadReader
                                                , asks )
import           Control.Monad.Except           ( MonadError
                                                )

import           Data.Hashable
import qualified Data.HashMap.Strict           as HMap
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!?) )
import           Path
import           Safe                           ( toEnumMay )
import           System.Directory
import           System.IO                      ( Handle
                                                , IOMode(..)
                                                , openFile
                                                )

import qualified TFM
import           TFM                            ( TexFont )

import           HeXPrelude
import           HeX.Type
import qualified HeX.Categorise                as Cat
import qualified HeX.Lex                       as Lex
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Parse.Token
import qualified HeX.Parse.AST                 as AST
import           HeX.Config.Parameters
import           HeX.Config.Codes
import           HeX.Parse.Resolve

type RegisterMap v = HMap.HashMap EightBitInt v

data ScopedConfig = ScopedConfig
    { csMap :: CSMap
    } deriving (Show)

newGlobalScopedConfig :: ScopedConfig
newGlobalScopedConfig = ScopedConfig
    { csMap = defaultCSMap
    }

newLocalScopedConfig :: ScopedConfig
newLocalScopedConfig = ScopedConfig
    { csMap = HMap.empty
    }

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
    , tokenListRegister :: RegisterMap BalancedText
    -- , boxRegister       :: RegisterMap (Maybe Box)
    -- File streams.
    , logStream         :: Handle
    , outFileStreams    :: HMap.HashMap FourBitInt Handle
    , scopedConfig      :: (ScopedConfig, [(AST.CommandTrigger, ScopedConfig)])
    } deriving (Show)

newConfig :: IO Config
newConfig =
    do
    cwdRaw <- getCurrentDirectory
    cwd <- parseAbsDir cwdRaw
    logHandle <- openFile "hex.log" WriteMode
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
        , logStream         = logHandle
        , outFileStreams    = HMap.empty
        , scopedConfig      = (newGlobalScopedConfig, [])
        }

fillMap :: (Hashable k, Enum k, Bounded k, Eq k) => v -> HMap.HashMap k v
fillMap v = HMap.fromList $ (, v) <$> [minBound..]

-- Fonts.

data FontInfo = FontInfo
    { fontMetrics :: TexFont
    , hyphenChar
    , skewChar    :: IntVal
    } deriving (Show)

readFontInfo :: (MonadReader Config m, MonadIO m) => Path Abs File -> m FontInfo
readFontInfo fontPath =
    do
    fontMetrics <- liftIO $ TFM.readTFMFancy fontPath
    hyphenChar <- unIntParam <$> asks (defaultHyphenChar . params)
    skewChar <- unIntParam <$> asks (defaultSkewChar . params)
    pure FontInfo{..}

lookupFontInfo :: (MonadReader Config m, MonadError String m) => Int -> m FontInfo
lookupFontInfo fNr =
    do
    infos <- asks fontInfos
    liftMaybe "No such font number" $ infos !? fNr

currentFontInfo :: (MonadReader Config m, MonadError String m) => m FontInfo
currentFontInfo = asks currentFontNr >>= liftMaybe "Font number isn't set" >>= lookupFontInfo

currentFontMetrics :: (MonadReader Config m, MonadError String m) => m TexFont
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

setConfTokenListParam :: MonadState Config m => TokenListParameter -> BalancedText -> m ()
setConfTokenListParam = liftSetParam setTokenListParam

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

-- Scopes.

modLocalScope :: (s, [(t, s)]) -> (s -> s) -> (s, [(t, s)])
modLocalScope (gS, (t, lS):tLS) f = (gS, (t, f lS):tLS)
modLocalScope (gS, []) f = (f gS, [])

insertControlSequence
    :: Config
    -> Lex.ControlSequenceLike
    -> ResolvedToken
    -> GlobalFlag
    -> Config
insertControlSequence conf cs t globalFlag =
    conf{scopedConfig = insertToScopes $ scopedConfig conf}
  where
    insertToScopes scopes@(g, locs) =
        case globalFlag of
            Global ->
                (insertCSToScope g, deleteCSFromScope <$> locs)
            Local ->
                modLocalScope scopes insertCSToScope

    insertCSToScope c@ScopedConfig{csMap = _csMap} =
        c{csMap = HMap.insert cs t _csMap}

    deleteCSFromScope (trig, c@ScopedConfig{csMap = _csMap}) =
        (trig, c{csMap = HMap.delete cs _csMap})


scopedLookup :: (k -> Maybe v) -> (k, [(a, k)]) -> Maybe v
scopedLookup f (g, []) = f g
scopedLookup f (g, (_, loc):locs) = case f loc of
    Nothing -> scopedLookup f (g, locs)
    Just v -> Just v

lookupCS :: Lex.ControlSequenceLike -> Config -> Maybe ResolvedToken
lookupCS cs = scopedLookupCS . scopedConfig
  where
    scopedLookupCS = scopedLookup ((HMap.lookup cs) . csMap)


lookupCSProper :: Lex.ControlSequence -> Config -> Maybe ResolvedToken
lookupCSProper cs = lookupCS (Lex.ControlSequenceProper cs)

pushScope :: AST.CommandTrigger -> Config -> Config
pushScope trig c@Config{scopedConfig = (g, locs)} =
    c{scopedConfig = (g, (trig, newLocalScopedConfig):locs)}

popScope :: AST.CommandTrigger -> Config -> Either String Config
popScope _       Config{scopedConfig = (_, [])} = Left "Cannot pop from global scope"
popScope trigA c@Config{scopedConfig = (g, (trigB, _):locs)}
    | trigA /= trigB = Left $ "Entry and exit scope triggers differ: " ++ show (trigA, trigB)
    | otherwise      = Right c{scopedConfig = (g, locs)}

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

setTokenListRegister :: MonadState Config m => EightBitInt -> BalancedText -> m ()
setTokenListRegister idx v =
    modify (\c -> c{tokenListRegister=HMap.insert idx v $ tokenListRegister c})
