module HeX.Config.Config where

import           HeXlude

import           Control.Monad.Except     ( MonadError, liftEither )
import           Control.Monad.IO.Class   ( MonadIO )
import           Control.Monad.Reader     ( MonadReader, asks )
import           Control.Monad.State.Lazy ( MonadState, gets, liftIO, modify )
import qualified Data.HashMap.Strict      as HMap
import           Data.Maybe               ( fromMaybe )
import qualified Data.IntMap.Strict       as IntMap
import           Data.IntMap.Strict       ( IntMap, (!?) )
import           Path
import           System.Directory
import           System.IO                ( Handle
                                          , IOMode(..)
                                          , hClose
                                          , openFile
                                          )

import           Safe                     ( toEnumMay )

import qualified TFM
import           TFM                      ( TexFont )

import qualified HeX.Box                  as B
import qualified HeX.BreakList            as BL
import qualified HeX.Categorise           as Cat
import           HeX.Config.Codes
import           HeX.Config.Parameters
import qualified HeX.Lex                  as Lex
import qualified HeX.Parse.AST            as AST
import           HeX.Parse.Resolve
import           HeX.Parse.Token

data Group = LocalStructureGroup AST.CommandTrigger | ExplicitBoxGroup
    deriving ( Show )

type RegisterMap v = HMap.HashMap EightBitInt v

data Scope =
    Scope { -- Fonts.
            currentFontNr :: Maybe Int
          , familyMemberFonts :: HMap.HashMap (FontRange, Int) Int
            -- Control sequences.
          , csMap :: CSMap
            -- Char-code attribute maps.
          , catCodes :: Cat.CatCodes
          , mathCodes :: Cat.CharCodeMap MathCode
          , lowercaseCodes, uppercaseCodes :: Cat.CharCodeMap CaseChangeCode
          , spaceFactors :: Cat.CharCodeMap SpaceFactorCode
          , delimiterCodes :: Cat.CharCodeMap DelimiterCode
            -- Parameters.
          , integerParameters :: HMap.HashMap IntegerParameter IntVal
          , lengthParameters :: HMap.HashMap LengthParameter LenVal
          , glueParameters :: HMap.HashMap GlueParameter BL.Glue
          , mathGlueParameters :: HMap.HashMap MathGlueParameter BL.MathGlue
          , tokenListParameters :: HMap.HashMap TokenListParameter BalancedText
            -- Registers.
          , integerRegister :: RegisterMap IntVal
          , lengthRegister :: RegisterMap LenVal
          , glueRegister :: RegisterMap BL.Glue
          , mathGlueRegister :: RegisterMap BL.MathGlue
          , tokenListRegister :: RegisterMap BalancedText
          , boxRegister :: RegisterMap B.Box
          }
    deriving ( Show )

newGlobalScope :: Scope
newGlobalScope =
    Scope { currentFontNr = Nothing
          , familyMemberFonts = HMap.empty
          , csMap = defaultCSMap
          , catCodes = Cat.usableCatCodes
          , mathCodes = newMathCodes
          , lowercaseCodes = newLowercaseCodes
          , uppercaseCodes = newUppercaseCodes
          , spaceFactors = newSpaceFactors
          , delimiterCodes = newDelimiterCodes
          , integerParameters = usableIntegerParameters
          , lengthParameters = usableLengthParameters
          , glueParameters = usableGlueParameters
          , mathGlueParameters = newMathGlueParameters
          , tokenListParameters = newTokenListParameters
          , integerRegister = HMap.empty
          , lengthRegister = HMap.empty
          , glueRegister = HMap.empty
          , mathGlueRegister = HMap.empty
          , tokenListRegister = HMap.empty
          , boxRegister = HMap.empty
          }

newLocalScope :: Scope
newLocalScope =
    Scope { currentFontNr = Nothing
          , familyMemberFonts = HMap.empty
          , csMap = HMap.empty
          , catCodes = HMap.empty
          , mathCodes = HMap.empty
          , lowercaseCodes = HMap.empty
          , uppercaseCodes = HMap.empty
          , spaceFactors = HMap.empty
          , delimiterCodes = HMap.empty
          , integerParameters = HMap.empty
          , lengthParameters = HMap.empty
          , glueParameters = HMap.empty
          , mathGlueParameters = HMap.empty
          , tokenListParameters = HMap.empty
          , integerRegister = HMap.empty
          , lengthRegister = HMap.empty
          , glueRegister = HMap.empty
          , mathGlueRegister = HMap.empty
          , tokenListRegister = HMap.empty
          , boxRegister = HMap.empty
          }

data Config =
    Config { fontInfos :: IntMap FontInfo
           , searchDirectories :: [Path Abs Dir]
           , specialIntegers :: HMap.HashMap SpecialInteger IntVal
           , specialLengths :: HMap.HashMap SpecialLength IntVal
             -- File streams.
           , logStream :: Handle
           , outFileStreams :: HMap.HashMap FourBitInt Handle
           , globalScope :: Scope
           , scopedConfig :: [(Group, Scope)]
           , afterAssignmentToken :: Maybe Lex.Token
           }
    deriving ( Show )

newConfig :: IO Config
newConfig = do
    cwdRaw <- getCurrentDirectory
    _searchDirectories
        <- mapM parseAbsDir
                [ "/usr/local/texlive/2018basic/texmf-dist/fonts/tfm/public/latex-fonts"
                , "/usr/local/texlive/2018basic/texmf-dist/fonts/tfm/public/cm"
                , "/usr/local/texlive/2018basic/texmf-dist/fonts/tfm/public/knuth-lib"
                , cwdRaw
                ]
    logHandle <- openFile "hex.log" WriteMode
    pure Config { fontInfos = IntMap.empty
                , searchDirectories = _searchDirectories
                , specialIntegers = newSpecialIntegers
                , specialLengths = newSpecialLengths
                , logStream = logHandle
                , outFileStreams = HMap.empty
                , globalScope = newGlobalScope
                , scopedConfig = []
                , afterAssignmentToken = Nothing
                }

finaliseConfig :: Config -> IO ()
finaliseConfig config = do
    hClose $ logStream config

-- Fonts info (global).

data FontInfo =
    FontInfo { fontMetrics :: TexFont, hyphenChar, skewChar :: IntVal }
    deriving ( Show )

readFontInfo :: (MonadReader Config m, MonadIO m, MonadError Text m)
             => Path Abs File
             -> m FontInfo
readFontInfo fontPath = do
    eithFontMetrics <- liftIO $ TFM.readTFMFancy fontPath
    fontMetrics <- liftEither eithFontMetrics
    hyphenChar <- asks $ lookupIntegerParameter DefaultHyphenChar
    skewChar <- asks $ lookupIntegerParameter DefaultSkewChar
    pure FontInfo { fontMetrics, hyphenChar, skewChar }

lookupFontInfo :: (MonadReader Config m, MonadError Text m)
               => Int
               -> m FontInfo
lookupFontInfo fNr = (!? fNr) <$> asks fontInfos
    >>= liftMaybe "No such font number"

addFont :: MonadState Config m => FontInfo -> m Int
addFont newInfo = do
    infos <- gets fontInfos
    let newKey = case IntMap.lookupMax infos of
            Nothing -> 0
            Just (i, _) -> succ i
        newInfos = IntMap.insert newKey newInfo infos
    modify (\conf -> conf { fontInfos = newInfos })
    pure newKey

modifyFont :: MonadState Config m => Int -> (FontInfo -> FontInfo) -> m ()
modifyFont fNr f = modify (\c@Config{fontInfos} ->
                           c { fontInfos = IntMap.adjust f fNr fontInfos })

-- Scopes.

modLocalScope :: Config -> (Scope -> Scope) -> Config
modLocalScope c@Config { globalScope, scopedConfig } f = case scopedConfig of
    [] -> c { globalScope = f globalScope }
    (t, lS) : tLS -> c { scopedConfig = (t, f lS) : tLS }

pushGroup :: Group -> Config -> Config
pushGroup grp c@Config { scopedConfig } =
    c { scopedConfig = (grp, newLocalScope) : scopedConfig }

popGroup :: Config -> Maybe (Group, Config)
popGroup c@Config { scopedConfig } = case scopedConfig of
    [] -> Nothing
    (grp, _) : scopedConfigRest -> Just (grp, c { scopedConfig = scopedConfigRest })

data KeyOperation v = InsertVal v | DeleteVal

modifyKey :: (Eq k, Hashable k)
          => (Scope -> HMap.HashMap k v)
          -> (Scope -> HMap.HashMap k v -> Scope)
          -> k
          -> KeyOperation v
          -> GlobalFlag
          -> Config
          -> Config
modifyKey getMap upD k keyOp globalFlag conf@Config { scopedConfig, globalScope } =
    case globalFlag of
        Global -> conf { globalScope = modOp globalScope
                       , scopedConfig = (\(t, sc) -> (t, deleteKeyFromScope sc)) <$> scopedConfig
                       }
        Local -> modLocalScope conf modOp
  where
    modOp = case keyOp of
        DeleteVal   -> deleteKeyFromScope
        InsertVal v -> insertKeyToScope v

    insertKeyToScope v c = upD c $ HMap.insert k v $ getMap c

    deleteKeyFromScope c = upD c $ HMap.delete k $ getMap c

insertKey :: (Eq k, Hashable k)
          => (Scope -> HMap.HashMap k v)
          -> (Scope -> HMap.HashMap k v -> Scope)
          -> k
          -> v
          -> GlobalFlag
          -> Config
          -> Config
insertKey getMap upD k v = modifyKey getMap upD k (InsertVal v)

deleteKey :: (Eq k, Hashable k)
          => (Scope -> HMap.HashMap k v)
          -> (Scope -> HMap.HashMap k v -> Scope)
          -> k
          -> GlobalFlag
          -> Config
          -> Config
deleteKey getMap upD k = modifyKey getMap upD k DeleteVal

scopedLookup :: (Scope -> Maybe v) -> Config -> Maybe v
scopedLookup f c@Config { globalScope, scopedConfig } = case scopedConfig of
    [] -> f globalScope
    (_, lS) : tLS -> case f lS of
        Nothing -> scopedLookup f c { scopedConfig = tLS }
        Just v  -> Just v

scopedMapLookup
    :: (Eq k, Hashable k)
    => (Scope -> HMap.HashMap k v)
    -> k
    -> Config
    -> Maybe v
scopedMapLookup getMap k = scopedLookup ((HMap.lookup k) . getMap)

-- Font number (scoped).
lookupCurrentFontNr :: Config -> Maybe Int
lookupCurrentFontNr = scopedLookup currentFontNr

mLookupCurrentFontNr :: (MonadReader Config m, MonadError Text m) => m Int
mLookupCurrentFontNr =
    asks lookupCurrentFontNr >>= liftMaybe "Font number isn't set"

currentFontInfo :: (MonadReader Config m, MonadError Text m) => m FontInfo
currentFontInfo = mLookupCurrentFontNr >>= lookupFontInfo

currentFontMetrics :: (MonadReader Config m, MonadError Text m) => m TexFont
currentFontMetrics = fontMetrics <$> currentFontInfo

selectFontNr :: Int -> GlobalFlag -> Config -> Config
selectFontNr n globalFlag c@Config { globalScope, scopedConfig } =
    case globalFlag of
        Global -> c { globalScope = selectFontInScope globalScope
                    , scopedConfig = (\(t, sc) -> (t, deselectFontInScope sc)) <$> scopedConfig
                    }
        Local -> modLocalScope c selectFontInScope
  where
    deselectFontInScope sc = sc { currentFontNr = Nothing }

    selectFontInScope sc = sc { currentFontNr = Just n }

setFamilyMemberFont
    :: (FontRange, Int)
    -> Int
    -> GlobalFlag
    -> Config
    -> Config
setFamilyMemberFont =
    insertKey familyMemberFonts $ \c _map -> c { familyMemberFonts = _map }

lookupFontFamilyMember
    :: (MonadReader Config m, MonadError Text m)
    => (FontRange, Int)
    -> m Int
lookupFontFamilyMember k = asks (scopedMapLookup familyMemberFonts k)
    >>= liftMaybe ("Family member undefined: " <> show k)

-- Control sequences.
lookupCS :: Lex.ControlSequenceLike -> Config -> Maybe ResolvedToken
lookupCS = scopedMapLookup csMap

lookupCSProper :: Lex.ControlSequence -> Config -> Maybe ResolvedToken
lookupCSProper cs = lookupCS (Lex.ControlSequenceProper cs)

setControlSequence
    :: Lex.ControlSequenceLike
    -> ResolvedToken
    -> GlobalFlag
    -> Config
    -> Config
setControlSequence = insertKey csMap $ \c _map -> c { csMap = _map }

-- Codes.
lookupCatCode :: Cat.CharCode -> Config -> Cat.CatCode
lookupCatCode t conf = Cat.catDefault $ scopedMapLookup catCodes t conf

lookupChangeCaseCode :: VDirection -> Cat.CharCode -> Config -> CaseChangeCode
lookupChangeCaseCode d t conf =
    let field = case d of
            Upward   -> uppercaseCodes
            Downward -> lowercaseCodes
    in
        fromMaybe NoCaseChange $ scopedMapLookup field t conf

updateCharCodeMap
    :: (MonadError Text m, MonadState Config m)
    => CodeType
    -> Cat.CharCode
    -> IntVal
    -> GlobalFlag
    -> m ()
updateCharCodeMap t c n globalFlag = do
    insert <- case t of
        CategoryCodeType       -> do
            v <- liftMay $ toEnumMay n
            pure $ insertKey catCodes (\cnf m -> cnf { catCodes = m }) c v
        MathCodeType           -> do
            v <- liftMay $ toEnumMay n
            pure $ insertKey mathCodes (\cnf m -> cnf { mathCodes = m }) c v
        ChangeCaseCodeType dir -> do
            v <- liftMay $ toEnumMay n
            pure $
                case dir of
                    Upward   -> insertKey uppercaseCodes
                                          (\cnf m -> cnf { uppercaseCodes = m })
                                          c
                                          v
                    Downward -> insertKey lowercaseCodes
                                          (\cnf m -> cnf { lowercaseCodes = m })
                                          c
                                          v
        SpaceFactorCodeType    -> do
            v <- liftMay $ toEnumMay n
            pure $
                insertKey spaceFactors (\cnf m -> cnf { spaceFactors = m }) c v
        DelimiterCodeType      -> do
            v <- liftMay $ toEnumMay n
            pure $
                insertKey delimiterCodes
                          (\cnf m -> cnf { delimiterCodes = m })
                          c
                          v
    modify $ insert globalFlag
  where
    liftMay :: MonadError Text m => Maybe a -> m a
    liftMay = liftMaybe ("Invalid target value for code type "
                         <> show t
                         <> ": "
                         <> show n)

-- Parameters and special quantities.
lookupIntegerParameter :: IntegerParameter -> Config -> IntVal
lookupIntegerParameter p conf =
    fromMaybe 0 $ scopedMapLookup integerParameters p conf

lookupLengthParameter :: LengthParameter -> Config -> LenVal
lookupLengthParameter p conf =
    fromMaybe 0 $ scopedMapLookup lengthParameters p conf

lookupGlueParameter :: GlueParameter -> Config -> BL.Glue
lookupGlueParameter p conf =
    fromMaybe mempty $ scopedMapLookup glueParameters p conf

lookupMathGlueParameter :: MathGlueParameter -> Config -> BL.MathGlue
lookupMathGlueParameter p conf =
    fromMaybe mempty $ scopedMapLookup mathGlueParameters p conf

lookupTokenListParameter :: TokenListParameter -> Config -> BalancedText
lookupTokenListParameter p conf =
    fromMaybe mempty $ scopedMapLookup tokenListParameters p conf

lookupSpecialInteger :: SpecialInteger -> Config -> IntVal
lookupSpecialInteger p conf = HMap.lookupDefault 0 p (specialIntegers conf)

lookupSpecialLength :: SpecialLength -> Config -> IntVal
lookupSpecialLength p conf = HMap.lookupDefault 0 p (specialLengths conf)

setIntegerParameter
    :: IntegerParameter
    -> IntVal
    -> GlobalFlag
    -> Config
    -> Config
setIntegerParameter =
    insertKey integerParameters $ \c _map -> c { integerParameters = _map }

setLengthParameter
    :: LengthParameter
    -> LenVal
    -> GlobalFlag
    -> Config
    -> Config
setLengthParameter =
    insertKey lengthParameters $ \c _map -> c { lengthParameters = _map }

setGlueParameter :: GlueParameter -> BL.Glue -> GlobalFlag -> Config -> Config
setGlueParameter =
    insertKey glueParameters $ \c _map -> c { glueParameters = _map }

setMathGlueParameter
    :: MathGlueParameter
    -> BL.MathGlue
    -> GlobalFlag
    -> Config
    -> Config
setMathGlueParameter =
    insertKey mathGlueParameters $ \c _map -> c { mathGlueParameters = _map }

setTokenListParameter
    :: TokenListParameter
    -> BalancedText
    -> GlobalFlag
    -> Config
    -> Config
setTokenListParameter =
    insertKey tokenListParameters $ \c _map -> c { tokenListParameters = _map }

setSpecialInteger :: SpecialInteger -> IntVal -> Config -> Config
setSpecialInteger p v conf =
    conf { specialIntegers = HMap.insert p v $ specialIntegers conf }

setSpecialLength :: SpecialLength -> IntVal -> Config -> Config
setSpecialLength p v conf =
    conf { specialLengths = HMap.insert p v $ specialLengths conf }

parIndentBox :: Config -> BL.BreakableHListElem
parIndentBox conf = BL.HVListElem $
    BL.VListBaseElem $
    B.ElemBox $
    B.Box { B.contents      = B.HBoxContents []
          , B.desiredLength = B.To . (lookupLengthParameter ParIndent) $ conf
          }

-- Registers.
lookupIntegerRegister :: EightBitInt -> Config -> IntVal
lookupIntegerRegister p conf =
    fromMaybe 0 $ scopedMapLookup integerRegister p conf

lookupLengthRegister :: EightBitInt -> Config -> LenVal
lookupLengthRegister p conf =
    fromMaybe 0 $ scopedMapLookup lengthRegister p conf

lookupGlueRegister :: EightBitInt -> Config -> BL.Glue
lookupGlueRegister p conf =
    fromMaybe mempty $ scopedMapLookup glueRegister p conf

lookupMathGlueRegister :: EightBitInt -> Config -> BL.MathGlue
lookupMathGlueRegister p conf =
    fromMaybe mempty $ scopedMapLookup mathGlueRegister p conf

lookupTokenListRegister :: EightBitInt -> Config -> BalancedText
lookupTokenListRegister p conf =
    fromMaybe mempty $ scopedMapLookup tokenListRegister p conf

lookupBoxRegister :: EightBitInt -> Config -> Maybe B.Box
lookupBoxRegister p conf = scopedMapLookup boxRegister p conf

setIntegerRegister :: EightBitInt -> IntVal -> GlobalFlag -> Config -> Config
setIntegerRegister =
    insertKey integerRegister $ \c _map -> c { integerRegister = _map }

setLengthRegister :: EightBitInt -> LenVal -> GlobalFlag -> Config -> Config
setLengthRegister =
    insertKey lengthRegister $ \c _map -> c { lengthRegister = _map }

setGlueRegister :: EightBitInt -> BL.Glue -> GlobalFlag -> Config -> Config
setGlueRegister = insertKey glueRegister $ \c _map -> c { glueRegister = _map }

setMathGlueRegister
    :: EightBitInt
    -> BL.MathGlue
    -> GlobalFlag
    -> Config
    -> Config
setMathGlueRegister =
    insertKey mathGlueRegister $ \c _map -> c { mathGlueRegister = _map }

setTokenListRegister
    :: EightBitInt
    -> BalancedText
    -> GlobalFlag
    -> Config
    -> Config
setTokenListRegister =
    insertKey tokenListRegister $ \c _map -> c { tokenListRegister = _map }

setBoxRegister :: EightBitInt -> B.Box -> GlobalFlag -> Config -> Config
setBoxRegister = insertKey boxRegister $ \c _map -> c { boxRegister = _map }

delBoxRegister :: EightBitInt -> GlobalFlag -> Config -> Config
delBoxRegister = deleteKey boxRegister $ \c _map -> c { boxRegister = _map }
