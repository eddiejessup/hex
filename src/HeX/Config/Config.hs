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
                                                , hClose
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

data Group
    = LocalStructureGroup AST.CommandTrigger
    | ExplicitBoxGroup
    deriving (Show)

type RegisterMap v = HMap.HashMap EightBitInt v

data Scope = Scope
    -- Fonts.
    { currentFontNr       :: Maybe Int
    , familyMemberFonts   :: HMap.HashMap (FontRange, Int) Int
    -- Control sequences.
    , csMap               :: CSMap
    -- Char-code attribute maps.
    , catCodes            :: Cat.CatCodes
    , mathCodes           :: Cat.CharCodeMap MathCode
    , lowercaseCodes
    , uppercaseCodes      :: Cat.CharCodeMap CaseChangeCode
    , spaceFactors        :: Cat.CharCodeMap SpaceFactorCode
    , delimiterCodes      :: Cat.CharCodeMap DelimiterCode
    -- Parameters.
    , integerParameters   :: HMap.HashMap IntegerParameter IntVal
    , lengthParameters    :: HMap.HashMap LengthParameter LenVal
    , glueParameters      :: HMap.HashMap GlueParameter BL.Glue
    , mathGlueParameters  :: HMap.HashMap MathGlueParameter BL.MathGlue
    , tokenListParameters :: HMap.HashMap TokenListParameter BalancedText
    -- Registers.
    , integerRegister     :: RegisterMap IntVal
    , lengthRegister      :: RegisterMap LenVal
    , glueRegister        :: RegisterMap BL.Glue
    , mathGlueRegister    :: RegisterMap BL.MathGlue
    , tokenListRegister   :: RegisterMap BalancedText
    -- , boxRegister         :: RegisterMap (Maybe Box)

    } deriving (Show)

newGlobalScope :: Scope
newGlobalScope = Scope
    { currentFontNr       = Nothing
    , familyMemberFonts   = HMap.empty

    , csMap               = defaultCSMap

    , catCodes            = Cat.usableCatCodes
    , mathCodes           = newMathCodes
    , lowercaseCodes      = newLowercaseCodes
    , uppercaseCodes      = newUppercaseCodes
    , spaceFactors        = newSpaceFactors
    , delimiterCodes      = newDelimiterCodes

    , integerParameters   = usableIntegerParameters
    , lengthParameters    = usableLengthParameters
    , glueParameters      = usableGlueParameters
    , mathGlueParameters  = newMathGlueParameters
    , tokenListParameters = newTokenListParameters

    , integerRegister     = HMap.empty
    , lengthRegister      = HMap.empty
    , glueRegister        = HMap.empty
    , mathGlueRegister    = HMap.empty
    , tokenListRegister   = HMap.empty
    -- , boxRegister         = HMap.empty
    }

newLocalScope :: Scope
newLocalScope = Scope
    { currentFontNr       = Nothing
    , familyMemberFonts   = HMap.empty

    , csMap               = HMap.empty

    , catCodes            = HMap.empty
    , mathCodes           = HMap.empty
    , lowercaseCodes      = HMap.empty
    , uppercaseCodes      = HMap.empty
    , spaceFactors        = HMap.empty
    , delimiterCodes      = HMap.empty

    , integerParameters   = HMap.empty
    , lengthParameters    = HMap.empty
    , glueParameters      = HMap.empty
    , mathGlueParameters  = HMap.empty
    , tokenListParameters = HMap.empty

    , integerRegister     = HMap.empty
    , lengthRegister      = HMap.empty
    , glueRegister        = HMap.empty
    , mathGlueRegister    = HMap.empty
    , tokenListRegister   = HMap.empty
    -- , boxRegister         = HMap.empty
    }

data Config = Config
    { fontInfos         :: V.Vector FontInfo
    , searchDirectories :: [Path Abs Dir]
    , specialIntegers :: HMap.HashMap SpecialInteger IntVal
    , specialLengths  :: HMap.HashMap SpecialLength IntVal
    -- File streams.
    , logStream         :: Handle
    , outFileStreams    :: HMap.HashMap FourBitInt Handle
    , scopedConfig      :: (Scope, [(Group, Scope)])
    , afterAssignmentToken :: Maybe Lex.Token
    } deriving (Show)

newConfig :: IO Config
newConfig =
    do
    cwdRaw <- getCurrentDirectory
    _searchDirectories <- mapM parseAbsDir [ "/usr/local/texlive/2018basic/texmf-dist/fonts/tfm/public/latex-fonts"
                                           , "/usr/local/texlive/2018basic/texmf-dist/fonts/tfm/public/cm"
                                           , "/usr/local/texlive/2018basic/texmf-dist/fonts/tfm/public/knuth-lib"
                                           , cwdRaw
                                           ]
    logHandle <- openFile "hex.log" WriteMode
    pure Config
        { fontInfos         = V.empty
        , searchDirectories = _searchDirectories
        , specialIntegers = newSpecialIntegers
        , specialLengths  = newSpecialLengths
        , logStream         = logHandle
        , outFileStreams    = HMap.empty
        , scopedConfig      = (newGlobalScope, [])
        , afterAssignmentToken = Nothing
        }

finaliseConfig :: Config -> IO ()
finaliseConfig config = do
    hClose $ logStream config

-- Fonts info (global).

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
    (!? fNr) <$> asks fontInfos >>= liftMaybe "No such font number"

addFont :: MonadState Config m => FontInfo -> m Int
addFont newInfo =
    do
    infos <- gets fontInfos
    let newInfos = V.snoc infos newInfo
    modify (\conf -> conf{fontInfos = newInfos})
    pure $ V.length newInfos - 1

-- Scopes.

modLocalScope :: (s, [(t, s)]) -> (s -> s) -> (s, [(t, s)])
modLocalScope (gS, (t, lS):tLS) f = (gS, (t, f lS):tLS)
modLocalScope (gS, []) f = (f gS, [])

pushGroup :: Group -> Config -> Config
pushGroup grp c@Config{scopedConfig = (g, locs)} =
    c{scopedConfig = (g, (grp, newLocalScope):locs)}

popGroup :: Config -> Maybe (Group, Config)
popGroup c@Config{scopedConfig = (g, locs)} =
    case locs of
        [] -> Nothing
        (grp, _):locs' -> Just (grp, c{scopedConfig = (g, locs')})

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
                (insertKeyToScope g, (\(t, sc) -> (t, deleteKeyFromScope sc)) <$> locs)
            Local ->
                modLocalScope scopes insertKeyToScope

    insertKeyToScope c =
        upD c $ HMap.insert k v $ getMap c

    deleteKeyFromScope c =
        upD c $ HMap.delete k $ getMap c

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

-- Font number (scoped).

lookupCurrentFontNr :: Config -> Maybe Int
lookupCurrentFontNr = scopedLookup currentFontNr . scopedConfig

mLookupCurrentFontNr :: (MonadReader Config m, MonadError String m) => m Int
mLookupCurrentFontNr = asks lookupCurrentFontNr >>= liftMaybe "Font number isn't set"

currentFontInfo :: (MonadReader Config m, MonadError String m) => m FontInfo
currentFontInfo = mLookupCurrentFontNr >>= lookupFontInfo

currentFontMetrics :: (MonadReader Config m, MonadError String m) => m TexFont
currentFontMetrics = fontMetrics <$> currentFontInfo

selectFontNr :: Int -> GlobalFlag -> Config -> Config
selectFontNr n globalFlag conf =
    conf{scopedConfig = insertToScopes $ scopedConfig conf}
  where
    insertToScopes scopes@(g, locs) =
        case globalFlag of
            Global ->
                (selectFontInScope g, (\(t, sc) -> (t, deselectFontInScope sc)) <$> locs)
            Local ->
                modLocalScope scopes selectFontInScope

    deselectFontInScope sc = sc{currentFontNr = Nothing}

    selectFontInScope sc = sc{currentFontNr = Just n}

setFamilyMemberFont
    :: (FontRange, Int) -> Int -> GlobalFlag -> Config -> Config
setFamilyMemberFont = insertKey familyMemberFonts $ \c _map -> c{familyMemberFonts = _map}

lookupFontFamilyMember :: (MonadReader Config m, MonadError String m) => (FontRange, Int) -> m Int
lookupFontFamilyMember k =
    asks (scopedMapLookup familyMemberFonts k)
        >>= liftMaybe ("Family member undefined: " ++ show k)

-- Control sequences.

lookupCS :: Lex.ControlSequenceLike -> Config -> Maybe ResolvedToken
lookupCS = scopedMapLookup csMap

lookupCSProper :: Lex.ControlSequence -> Config -> Maybe ResolvedToken
lookupCSProper cs = lookupCS (Lex.ControlSequenceProper cs)

setControlSequence
    :: Lex.ControlSequenceLike -> ResolvedToken -> GlobalFlag -> Config -> Config
setControlSequence = insertKey csMap $ \c _map -> c{csMap = _map}

-- Codes.

lookupCatCode :: Cat.CharCode -> Config -> Cat.CatCode
lookupCatCode t conf = Cat.catDefault $ scopedMapLookup catCodes t conf

lookupChangeCaseCode :: VDirection -> Cat.CharCode -> Config -> CaseChangeCode
lookupChangeCaseCode d t conf =
    let field = case d of
            Upward   -> uppercaseCodes
            Downward -> lowercaseCodes
    in fromMaybe NoCaseChange $ scopedMapLookup field t conf

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
            pure $ insertKey catCodes (\cnf m -> cnf{catCodes = m}) c v
        MathCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure $ insertKey mathCodes (\cnf m -> cnf{mathCodes = m}) c v
        ChangeCaseCodeType dir ->
            do
            v <- liftMay $ toEnumMay n
            pure $ case dir of
                Upward -> insertKey uppercaseCodes (\cnf m -> cnf{uppercaseCodes = m}) c v
                Downward -> insertKey lowercaseCodes (\cnf m -> cnf{lowercaseCodes = m}) c v
        SpaceFactorCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure $ insertKey spaceFactors (\cnf m -> cnf{spaceFactors = m}) c v
        DelimiterCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure $ insertKey delimiterCodes (\cnf m -> cnf{delimiterCodes = m}) c v
    modify $ insert globalFlag
  where
    liftMay f = liftMaybe ("Invalid target value for code type " ++ show t ++ ": " ++ show n) f

-- Parameters and special quantities.

lookupIntegerParameter :: IntegerParameter -> Config -> IntVal
lookupIntegerParameter p conf = fromMaybe 0 $ scopedMapLookup integerParameters p conf

lookupLengthParameter :: LengthParameter -> Config -> LenVal
lookupLengthParameter p conf = fromMaybe 0 $ scopedMapLookup lengthParameters p conf

lookupGlueParameter :: GlueParameter -> Config -> BL.Glue
lookupGlueParameter p conf = fromMaybe mempty $ scopedMapLookup glueParameters p conf

lookupMathGlueParameter :: MathGlueParameter -> Config -> BL.MathGlue
lookupMathGlueParameter p conf = fromMaybe mempty $ scopedMapLookup mathGlueParameters p conf

lookupTokenListParameter :: TokenListParameter -> Config -> BalancedText
lookupTokenListParameter p conf = fromMaybe mempty $ scopedMapLookup tokenListParameters p conf

lookupSpecialInteger :: SpecialInteger -> Config -> IntVal
lookupSpecialInteger p conf = HMap.lookupDefault 0 p (specialIntegers conf)

lookupSpecialLength :: SpecialLength -> Config -> IntVal
lookupSpecialLength p conf = HMap.lookupDefault 0 p (specialLengths conf)

setIntegerParameter :: IntegerParameter -> IntVal -> GlobalFlag -> Config -> Config
setIntegerParameter = insertKey integerParameters $ \c _map -> c{integerParameters = _map}

setLengthParameter :: LengthParameter -> LenVal -> GlobalFlag -> Config -> Config
setLengthParameter = insertKey lengthParameters $ \c _map -> c{lengthParameters = _map}

setGlueParameter :: GlueParameter -> BL.Glue -> GlobalFlag -> Config -> Config
setGlueParameter = insertKey glueParameters $ \c _map -> c{glueParameters = _map}

setMathGlueParameter :: MathGlueParameter -> BL.MathGlue -> GlobalFlag -> Config -> Config
setMathGlueParameter = insertKey mathGlueParameters $ \c _map -> c{mathGlueParameters = _map}

setTokenListParameter :: TokenListParameter -> BalancedText -> GlobalFlag -> Config -> Config
setTokenListParameter = insertKey tokenListParameters $ \c _map -> c{tokenListParameters = _map}

setSpecialInteger :: SpecialInteger -> IntVal -> Config -> Config
setSpecialInteger p v conf = conf{specialIntegers=HMap.insert p v $ specialIntegers conf}

setSpecialLength :: SpecialLength -> IntVal -> Config -> Config
setSpecialLength p v conf = conf{specialLengths=HMap.insert p v $ specialLengths conf}

parIndentBox :: Config -> BL.BreakableHListElem
parIndentBox conf =
    BL.HVListElem $ BL.VListBaseElem $ B.ElemBox $ B.Box
        { contents      = B.HBoxContents []
        , desiredLength = B.To . (lookupLengthParameter ParIndent) $ conf
        }

-- Registers.

lookupIntegerRegister :: EightBitInt -> Config -> IntVal
lookupIntegerRegister p conf = fromMaybe 0 $ scopedMapLookup integerRegister p conf

lookupLengthRegister :: EightBitInt -> Config -> LenVal
lookupLengthRegister p conf = fromMaybe 0 $ scopedMapLookup lengthRegister p conf

lookupGlueRegister :: EightBitInt -> Config -> BL.Glue
lookupGlueRegister p conf = fromMaybe mempty $ scopedMapLookup glueRegister p conf

lookupMathGlueRegister :: EightBitInt -> Config -> BL.MathGlue
lookupMathGlueRegister p conf = fromMaybe mempty $ scopedMapLookup mathGlueRegister p conf

lookupTokenListRegister :: EightBitInt -> Config -> BalancedText
lookupTokenListRegister p conf = fromMaybe mempty $ scopedMapLookup tokenListRegister p conf

setIntegerRegister :: EightBitInt -> IntVal -> GlobalFlag -> Config -> Config
setIntegerRegister = insertKey integerRegister $ \c _map -> c{integerRegister = _map}

setLengthRegister :: EightBitInt -> LenVal -> GlobalFlag -> Config -> Config
setLengthRegister = insertKey lengthRegister $ \c _map -> c{lengthRegister = _map}

setGlueRegister :: EightBitInt -> BL.Glue -> GlobalFlag -> Config -> Config
setGlueRegister = insertKey glueRegister $ \c _map -> c{glueRegister = _map}

setMathGlueRegister :: EightBitInt -> BL.MathGlue -> GlobalFlag -> Config -> Config
setMathGlueRegister = insertKey mathGlueRegister $ \c _map -> c{mathGlueRegister = _map}

setTokenListRegister :: EightBitInt -> BalancedText -> GlobalFlag -> Config -> Config
setTokenListRegister = insertKey tokenListRegister $ \c _map -> c{tokenListRegister = _map}
