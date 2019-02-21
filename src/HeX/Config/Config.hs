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
import           Data.Maybe                     ( fromMaybe )
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

data Scope = Scope
    { csMap                   :: CSMap
    -- Char-code attribute maps.
    , catCodeMap              :: Cat.CharCatMap
    , mathCodeMap             :: Cat.CharCodeMap MathCode
    , lowercaseMap
    , uppercaseMap            :: Cat.CharCodeMap CaseChangeCode
    , spaceFactorMap          :: Cat.CharCodeMap SpaceFactorCode
    , delimiterCodeMap        :: Cat.CharCodeMap DelimiterCode
    -- Parameters.
    , integerParameterMap     :: HMap.HashMap IntegerParameter IntVal
    , lengthParameterMap      :: HMap.HashMap LengthParameter LenVal
    , glueParameterMap        :: HMap.HashMap GlueParameter BL.Glue
    -- , mathGlueParameterMap :: HMap.HashMap MathGlueParameter MathGlue
    , tokenListParameterMap   :: HMap.HashMap TokenListParameter BalancedText
    } deriving (Show)

newGlobalScope :: Scope
newGlobalScope = Scope
    { csMap      = defaultCSMap

    , catCodeMap              = Cat.usableCharCatMap
    , mathCodeMap             = newMathCodeMap
    , lowercaseMap            = newLowercaseMap
    , uppercaseMap            = newUppercaseMap
    , spaceFactorMap          = newSpaceFactorMap
    , delimiterCodeMap        = newDelimiterCodeMap

    , integerParameterMap     = usableIntegerParameterMap
    , lengthParameterMap      = usableLengthParameterMap
    , glueParameterMap        = usableGlueParameterMap
    -- , mathGlueParameterMap = newMathGlueParameterMap
    , tokenListParameterMap   = newTokenListParameterMap
    }

newLocalScope :: Scope
newLocalScope = Scope
    { csMap                   = HMap.empty

    , catCodeMap              = HMap.empty
    , mathCodeMap             = HMap.empty
    , lowercaseMap            = HMap.empty
    , uppercaseMap            = HMap.empty
    , spaceFactorMap          = HMap.empty
    , delimiterCodeMap        = HMap.empty

    , integerParameterMap     = HMap.empty
    , lengthParameterMap      = HMap.empty
    , glueParameterMap        = HMap.empty
    -- , mathGlueParameterMap = HMap.empty
    , tokenListParameterMap   = HMap.empty
    }

data Config = Config
    { currentFontNr     :: Maybe Int
    , fontInfos         :: V.Vector FontInfo
    , searchDirectories :: [Path Abs Dir]
    -- Registers.
    , integerRegister   :: RegisterMap IntVal
    , lengthRegister    :: RegisterMap LenVal
    , glueRegister      :: RegisterMap BL.Glue
    -- , mathGlueRegister  :: RegisterMap MathGlue
    , tokenListRegister :: RegisterMap BalancedText
    -- , boxRegister       :: RegisterMap (Maybe Box)
    , specialIntegerMap       :: HMap.HashMap SpecialInteger IntVal
    , specialLengthMap        :: HMap.HashMap SpecialLength IntVal
    -- File streams.
    , logStream         :: Handle
    , outFileStreams    :: HMap.HashMap FourBitInt Handle
    , scopedConfig      :: (Scope, [(AST.CommandTrigger, Scope)])
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
        , integerRegister   = HMap.empty
        , lengthRegister    = HMap.empty
        , glueRegister      = HMap.empty
        -- , mathGlueRegister  = HMap.empty
        , tokenListRegister = HMap.empty
        -- , boxRegister       = HMap.empty
        , specialIntegerMap = newSpecialIntegerMap
        , specialLengthMap  = newSpecialLengthMap
        , logStream         = logHandle
        , outFileStreams    = HMap.empty
        , scopedConfig      = (newGlobalScope, [])
        }

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
    hyphenChar <- asks $ lookupIntegerParameter DefaultHyphenChar
    skewChar <- asks $ lookupIntegerParameter DefaultSkewChar
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

parIndentBox :: Config -> BL.BreakableHListElem
parIndentBox conf =
    BL.HVListElem $ BL.VListBaseElem $ B.ElemBox $ B.Box
        { contents      = B.HBoxContents []
        , desiredLength = B.To . (lookupLengthParameter ParIndent) $ conf
        }

-- Scopes.

modLocalScope :: (s, [(t, s)]) -> (s -> s) -> (s, [(t, s)])
modLocalScope (gS, (t, lS):tLS) f = (gS, (t, f lS):tLS)
modLocalScope (gS, []) f = (f gS, [])

insertKey
    :: (Eq k, Hashable k)
    => (Scope -> HMap.HashMap k v)
    -> (Scope -> HMap.HashMap k v -> Scope)
    -> k
    -> v
    -> GlobalFlag
    -> Config
    -> Config
insertKey getMap upD k v globalFlag conf =
    conf{scopedConfig = insertToScopes $ scopedConfig conf}
  where
    insertToScopes scopes@(g, locs) =
        case globalFlag of
            Global ->
                (insertKeyToScope g, deleteKeyFromScope <$> locs)
            Local ->
                modLocalScope scopes insertKeyToScope

    insertKeyToScope c =
        upD c $ HMap.insert k v $ getMap c

    deleteKeyFromScope (trig, c) =
        (trig, upD c $ HMap.delete k $ getMap c)

insertControlSequence
    :: Lex.ControlSequenceLike
    -> ResolvedToken
    -> GlobalFlag
    -> Config
    -> Config
insertControlSequence = insertKey csMap $ \c _map -> c{csMap = _map}

scopedLookup :: (k -> Maybe v) -> (k, [(a, k)]) -> Maybe v
scopedLookup f (g, []) = f g
scopedLookup f (g, (_, loc):locs) = case f loc of
    Nothing -> scopedLookup f (g, locs)
    Just v -> Just v

scopedMapLookup
    :: (Eq k, Hashable k)
    => (Scope -> HMap.HashMap k v) -> k -> Config -> Maybe v
scopedMapLookup getMap k = lkp . scopedConfig
  where
    lkp = scopedLookup ((HMap.lookup k) . getMap)

lookupCS :: Lex.ControlSequenceLike -> Config -> Maybe ResolvedToken
lookupCS = scopedMapLookup csMap

lookupCSProper :: Lex.ControlSequence -> Config -> Maybe ResolvedToken
lookupCSProper cs = lookupCS (Lex.ControlSequenceProper cs)

lookupCatCode :: Cat.CharCode -> Config -> Cat.CatCode
lookupCatCode t conf = Cat.catDefault $ scopedMapLookup catCodeMap t conf

lookupIntegerParameter :: IntegerParameter -> Config -> IntVal
lookupIntegerParameter p conf = fromMaybe 0 $ scopedMapLookup integerParameterMap p conf

lookupLengthParameter :: LengthParameter -> Config -> LenVal
lookupLengthParameter p conf = fromMaybe 0 $ scopedMapLookup lengthParameterMap p conf

lookupGlueParameter :: GlueParameter -> Config -> BL.Glue
lookupGlueParameter p conf = fromMaybe mempty $ scopedMapLookup glueParameterMap p conf

-- lookupMathGlueParameter :: MathGlueParameter -> Config -> MathGlue
-- lookupMathGlueParameter p conf = fromMaybe 0 $ scopedMapLookup mathGlueParameterMap p conf

lookupTokenListParameter :: TokenListParameter -> Config -> BalancedText
lookupTokenListParameter p conf = fromMaybe mempty $ scopedMapLookup tokenListParameterMap p conf

lookupSpecialInteger :: SpecialInteger -> Config -> IntVal
lookupSpecialInteger p conf = HMap.lookupDefault 0 p (specialIntegerMap conf)

lookupSpecialLength :: SpecialLength -> Config -> IntVal
lookupSpecialLength p conf = HMap.lookupDefault 0 p (specialLengthMap conf)

setIntegerParameter :: IntegerParameter -> IntVal -> GlobalFlag -> Config -> Config
setIntegerParameter = insertKey integerParameterMap $ \c _map -> c{integerParameterMap = _map}

setLengthParameter :: LengthParameter -> LenVal -> GlobalFlag -> Config -> Config
setLengthParameter = insertKey lengthParameterMap $ \c _map -> c{lengthParameterMap = _map}

setGlueParameter :: GlueParameter -> BL.Glue -> GlobalFlag -> Config -> Config
setGlueParameter = insertKey glueParameterMap $ \c _map -> c{glueParameterMap = _map}

-- setMathGlueParameter :: MathGlueParameter -> MathGlue -> GlobalFlag -> Config -> Config
-- setMathGlueParameter = insertKey mathGlueParameterMap $ \c _map -> c{mathGlueParameterMap = _map}

setTokenListParameter :: TokenListParameter -> BalancedText -> GlobalFlag -> Config -> Config
setTokenListParameter = insertKey tokenListParameterMap $ \c _map -> c{tokenListParameterMap = _map}

setSpecialInteger :: SpecialInteger -> IntVal -> Config -> Config
setSpecialInteger p v conf = conf{specialIntegerMap=HMap.insert p v $ specialIntegerMap conf}

setSpecialLength :: SpecialLength -> IntVal -> Config -> Config
setSpecialLength p v conf = conf{specialLengthMap=HMap.insert p v $ specialLengthMap conf}

pushScope :: AST.CommandTrigger -> Config -> Config
pushScope trig c@Config{scopedConfig = (g, locs)} =
    c{scopedConfig = (g, (trig, newLocalScope):locs)}

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
    -> GlobalFlag
    -> m ()
updateCharCodeMap t c n globalFlag =
    do
    insert <- case t of
        CategoryCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure $ insertKey catCodeMap (\cnf m -> cnf{catCodeMap = m}) c v
        MathCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure $ insertKey mathCodeMap (\cnf m -> cnf{mathCodeMap = m}) c v
        ChangeCaseCodeType dir ->
            do
            v <- liftMay $ toEnumMay n
            pure $ case dir of
                Upward -> insertKey uppercaseMap (\cnf m -> cnf{uppercaseMap = m}) c v
                Downward -> insertKey lowercaseMap (\cnf m -> cnf{lowercaseMap = m}) c v
        SpaceFactorCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure $ insertKey spaceFactorMap (\cnf m -> cnf{spaceFactorMap = m}) c v
        DelimiterCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure $ insertKey delimiterCodeMap (\cnf m -> cnf{delimiterCodeMap = m}) c v
    modify $ insert globalFlag
  where
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
