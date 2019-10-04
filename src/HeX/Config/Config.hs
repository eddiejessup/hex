module HeX.Config.Config where

import           HeXlude

import           Control.Monad.IO.Class   (MonadIO)
import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State.Lazy (MonadState, gets, modify)
import qualified Data.HashMap.Strict      as HMap
import           Data.IntMap.Strict       (IntMap, (!?))
import qualified Data.IntMap.Strict       as IntMap
import           Data.Maybe               (fromMaybe)
import qualified Data.Path                as D.Path
import           Path                     (Abs, Dir, File, Path, Rel,
                                           parseAbsDir)
import qualified Path.IO
import           System.Directory
import           System.IO                (Handle, IOMode (..), hClose,
                                           openFile)

import           Safe                     (toEnumMay)

import           TFM                      (TexFont)
import qualified TFM

import qualified HeX.Box                  as B
import qualified HeX.BreakList            as BL
import qualified HeX.Categorise           as Cat
import           HeX.Config.Codes
import           HeX.Config.Parameters
import qualified HeX.Lex                  as Lex
import qualified HeX.Parse.AST            as AST
import           HeX.Parse.Resolve
import           HeX.Parse.Token
import           HeX.Quantity

data Group
    = ScopeGroup Scope ScopeGroup
    | NonScopeGroup
    deriving ( Show )

data ScopeGroup
    = LocalStructureGroup AST.CommandTrigger
    | ExplicitBoxGroup
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
          , texIntParameters :: HMap.HashMap TeXIntParameter TeXIntVal
          , lengthParameters :: HMap.HashMap LengthParameter TeXLength
          , glueParameters :: HMap.HashMap GlueParameter (BL.Glue TeXLength)
          , mathGlueParameters :: HMap.HashMap MathGlueParameter (BL.Glue MathLength)
          , tokenListParameters :: HMap.HashMap TokenListParameter BalancedText
            -- Registers.
          , texIntRegister :: RegisterMap TeXIntVal
          , lengthRegister :: RegisterMap TeXLength
          , glueRegister :: RegisterMap (BL.Glue TeXLength)
          , mathGlueRegister :: RegisterMap (BL.Glue MathLength)
          , tokenListRegister :: RegisterMap BalancedText
          , boxRegister :: RegisterMap (B.Box B.BoxContents)
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
          , texIntParameters = usableTeXIntParameters
          , lengthParameters = usableLengthParameters
          , glueParameters = usableGlueParameters
          , mathGlueParameters = newMathGlueParameters
          , tokenListParameters = newTokenListParameters
          , texIntRegister = HMap.empty
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
          , texIntParameters = HMap.empty
          , lengthParameters = HMap.empty
          , glueParameters = HMap.empty
          , mathGlueParameters = HMap.empty
          , tokenListParameters = HMap.empty
          , texIntRegister = HMap.empty
          , lengthRegister = HMap.empty
          , glueRegister = HMap.empty
          , mathGlueRegister = HMap.empty
          , tokenListRegister = HMap.empty
          , boxRegister = HMap.empty
          }

data Config =
    Config { fontInfos            :: IntMap FontInfo
           , searchDirectories    :: [Path Abs Dir]
           , specialTeXInts       :: HMap.HashMap SpecialTeXInt TeXIntVal
           , specialLengths       :: HMap.HashMap SpecialLength TeXLength
             -- File streams.
           , logStream            :: Handle
           , outFileStreams       :: HMap.HashMap FourBitInt Handle
           , afterAssignmentToken :: Maybe Lex.Token
           , globalScope          :: Scope
           , groups               :: [Group]
           }

newtype ConfigError = ConfigError Text
    deriving (Show)

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
                , specialTeXInts = newSpecialTeXInts
                , specialLengths = newSpecialLengths
                , logStream = logHandle
                , outFileStreams = HMap.empty
                , afterAssignmentToken = Nothing
                , globalScope = newGlobalScope
                , groups = []
                }

finaliseConfig :: Config -> IO ()
finaliseConfig config =
    hClose $ logStream config

-- Unscoped.

data FindFilePolicy
    = NoImplicitExtension
    | WithImplicitExtension Text

findFilePath
    :: ( MonadReader Config m
       , MonadIO m
       , MonadErrorAnyOf e m '[ConfigError, D.Path.PathError]
       )
    => FindFilePolicy
    -> [Path Abs Dir]
    -> Path Rel File
    -> m (Path Abs File)
findFilePath findPolicy extraPaths p =
    do
    dirs <- asks searchDirectories <&> (<> extraPaths)
    getTgtPath
        >>= Path.IO.findFile dirs
        >>= liftMaybe (throw $ ConfigError $ "Could not find file: " <> show p)
  where
    getTgtPath = case findPolicy of
        NoImplicitExtension ->
            pure p
        WithImplicitExtension ext ->
            D.Path.setFileExtension p ext

-- Font info.

data FontInfo =
    FontInfo { fontMetrics :: TexFont, hyphenChar, skewChar :: TeXIntVal }

readFontInfo
    :: ( MonadReader Config m
       , MonadIO m
       , MonadErrorAnyOf e m '[TFM.TFMError]
       )
    => Path Abs File
    -> m FontInfo
readFontInfo fontPath = do
    fontMetrics <- TFM.readTFMFancy fontPath
    hyphenChar <- asks $ lookupTeXIntParameter DefaultHyphenChar
    skewChar <- asks $ lookupTeXIntParameter DefaultSkewChar
    pure FontInfo { fontMetrics, hyphenChar, skewChar }

lookupFontInfo
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[ConfigError]
       )
    => Int
    -> m FontInfo
lookupFontInfo fNr = (!? fNr) <$> asks fontInfos
    >>= liftMaybe (throw (ConfigError "No such font number"))

addFont :: MonadState Config m => FontInfo -> m Int
addFont newInfo = do
    infos <- gets fontInfos
    let newKey = case IntMap.lookupMax infos of
            Nothing     -> 0
            Just (i, _) -> succ i
        newInfos = IntMap.insert newKey newInfo infos
    modify (\conf -> conf { fontInfos = newInfos })
    pure newKey

modifyFont :: MonadState Config m => Int -> (FontInfo -> FontInfo) -> m ()
modifyFont fNr f = modify (\c@Config{fontInfos} ->
                           c { fontInfos = IntMap.adjust f fNr fontInfos })

-- Special quantities.

lookupSpecialTeXInt :: SpecialTeXInt -> Config -> TeXIntVal
lookupSpecialTeXInt p c = HMap.lookupDefault 0 p (specialTeXInts c)

lookupSpecialLength :: SpecialLength -> Config -> TeXLength
lookupSpecialLength p c = HMap.lookupDefault (TeXLength 0) p (specialLengths c)

setSpecialTeXInt :: SpecialTeXInt -> TeXIntVal -> Config -> Config
setSpecialTeXInt p v c =
    c{ specialTeXInts = HMap.insert p v $ specialTeXInts c }

setSpecialLength :: SpecialLength -> TeXLength -> Config -> Config
setSpecialLength p v c =
    c{ specialLengths = HMap.insert p v $ specialLengths c }

-- Scoped.

modLocalScope :: Config -> (Scope -> Scope) -> Config
modLocalScope c@Config{ globalScope, groups } f =
    case groups of
        [] -> c{ globalScope = f globalScope }
        ScopeGroup scope scopeGroupType : outerGroups ->
            c{ groups = ScopeGroup (f scope) scopeGroupType : outerGroups }
        nonScopeGroup : outerGroups ->
            let
                -- Get result if this non-scoped group hadn't existed.
                cWithoutGroup@Config{ groups = groupsWithoutGroup }
                    = modLocalScope c{ groups = outerGroups } f
            in
                -- Append non-scope group to that result.
                cWithoutGroup{ groups = nonScopeGroup:groupsWithoutGroup }

pushGroup :: Group -> Config -> Config
pushGroup group c@Config{ groups } =
    c{ groups = group : groups }

popGroup :: Config -> Maybe (Group, Config)
popGroup c@Config{ groups } =
    case groups of
        [] ->
            Nothing
        group : outerGroups ->
            Just (group, c{ groups = outerGroups })

data KeyOperation v = InsertVal v | DeleteVal

modGroupScope :: (Scope -> Scope) -> Group -> Group
modGroupScope f = \case
    ScopeGroup scope scopeGroupType ->
        ScopeGroup (f scope) scopeGroupType
    nonScopeGroup ->
        nonScopeGroup

modifyKey :: (Eq k, Hashable k)
          => (Scope -> HMap.HashMap k v)
          -> (Scope -> HMap.HashMap k v -> Scope)
          -> k
          -> KeyOperation v
          -> GlobalFlag
          -> Config
          -> Config
modifyKey getMap upD k keyOp globalFlag c@Config{ groups, globalScope } =
    case globalFlag of
        Global ->
            c{ globalScope = modOp globalScope
             , groups = modGroupScope deleteKeyFromScope <$> groups
             }
        Local ->
            modLocalScope c modOp
  where
    modOp = case keyOp of
        DeleteVal   -> deleteKeyFromScope
        InsertVal v -> insertKeyToScope v

    insertKeyToScope v scope = upD scope $ HMap.insert k v $ getMap scope
    deleteKeyFromScope scope = upD scope $ HMap.delete k $ getMap scope

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
scopedLookup f c@Config{ globalScope, groups } =
    case groups of
        [] -> f globalScope
        ScopeGroup scope _ : outerGroups ->
            case f scope of
                Nothing -> scopedLookup f c{ groups = outerGroups }
                Just v  -> Just v
        _ : outerGroups ->
            scopedLookup f c{ groups = outerGroups }

scopedMapLookup
    :: (Eq k, Hashable k)
    => (Scope -> HMap.HashMap k v)
    -> k
    -> Config
    -> Maybe v
scopedMapLookup getMap k = scopedLookup (HMap.lookup k . getMap)

-- Font number (scoped).
lookupCurrentFontNr :: Config -> Maybe Int
lookupCurrentFontNr = scopedLookup currentFontNr

mLookupCurrentFontNr
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[ConfigError]
       )
    => m Int
mLookupCurrentFontNr =
    asks lookupCurrentFontNr >>= liftMaybe (throw $ ConfigError "Font number isn't set")

selectFontNr :: Int -> GlobalFlag -> Config -> Config
selectFontNr n globalFlag c@Config{ globalScope, groups } =
    case globalFlag of
        Global ->
            c{ globalScope = selectFontInScope globalScope
             , groups = modGroupScope deselectFontInScope <$> groups
             }
        Local ->
            modLocalScope c selectFontInScope
  where
    deselectFontInScope scope = scope{ currentFontNr = Nothing }
    selectFontInScope scope = scope{ currentFontNr = Just n }

setFamilyMemberFont
    :: (FontRange, Int)
    -> Int
    -> GlobalFlag
    -> Config
    -> Config
setFamilyMemberFont =
    insertKey familyMemberFonts $ \c _map -> c { familyMemberFonts = _map }

lookupFontFamilyMember
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[ConfigError]
       )
    => (FontRange, Int)
    -> m Int
lookupFontFamilyMember k = asks (scopedMapLookup familyMemberFonts k)
    >>= liftMaybe (throw $ ConfigError $ "Family member undefined: " <> show k)

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
    :: ( MonadErrorAnyOf e m '[ConfigError]
       , MonadState Config m
       )
    => CodeType
    -> Cat.CharCode
    -> TeXIntVal
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
            pure $ case dir of
                Upward ->
                    insertKey uppercaseCodes (\cnf m -> cnf { uppercaseCodes = m }) c v
                Downward ->
                    insertKey lowercaseCodes (\cnf m -> cnf { lowercaseCodes = m }) c v
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
    liftMay
        :: ( MonadErrorAnyOf e m '[ConfigError]
           )
        => Maybe a
        -> m a
    liftMay = liftMaybe (throw $ ConfigError $
                         "Invalid target value for code type "
                         <> show t
                         <> ": "
                         <> show n)

-- Parameters and special quantities.
lookupTeXIntParameter :: TeXIntParameter -> Config -> TeXIntVal
lookupTeXIntParameter p conf =
    fromMaybe 0 $ scopedMapLookup texIntParameters p conf

lookupLengthParameter :: LengthParameter -> Config -> TeXLength
lookupLengthParameter p conf =
    fromMaybe 0 $ scopedMapLookup lengthParameters p conf

lookupGlueParameter :: GlueParameter -> Config -> (BL.Glue TeXLength)
lookupGlueParameter p conf =
    fromMaybe mempty $ scopedMapLookup glueParameters p conf

lookupMathGlueParameter :: MathGlueParameter -> Config -> (BL.Glue MathLength)
lookupMathGlueParameter p conf =
    fromMaybe mempty $ scopedMapLookup mathGlueParameters p conf

lookupTokenListParameter :: TokenListParameter -> Config -> BalancedText
lookupTokenListParameter p conf =
    fromMaybe mempty $ scopedMapLookup tokenListParameters p conf

setTeXIntParameter
    :: TeXIntParameter
    -> TeXIntVal
    -> GlobalFlag
    -> Config
    -> Config
setTeXIntParameter =
    insertKey texIntParameters $ \c _map -> c { texIntParameters = _map }

setLengthParameter
    :: LengthParameter
    -> TeXLength
    -> GlobalFlag
    -> Config
    -> Config
setLengthParameter =
    insertKey lengthParameters $ \c _map -> c { lengthParameters = _map }

setGlueParameter :: GlueParameter -> (BL.Glue TeXLength) -> GlobalFlag -> Config -> Config
setGlueParameter =
    insertKey glueParameters $ \c _map -> c { glueParameters = _map }

setMathGlueParameter
    :: MathGlueParameter
    -> (BL.Glue MathLength)
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

parIndentBox :: Config -> BL.HListElem
parIndentBox conf = BL.HVListElem $
    BL.VListBaseElem $
    B.ElemBox $ B.Box
        { B.contents  = B.HBoxContents mempty
        , B.boxWidth  = lookupLengthParameter ParIndent conf
        , B.boxHeight = 0
        , B.boxDepth  = 0
        }

-- Registers.
lookupTeXIntRegister :: EightBitInt -> Config -> TeXIntVal
lookupTeXIntRegister p conf =
    fromMaybe 0 $ scopedMapLookup texIntRegister p conf

lookupLengthRegister :: EightBitInt -> Config -> TeXLength
lookupLengthRegister p conf =
    fromMaybe 0 $ scopedMapLookup lengthRegister p conf

lookupGlueRegister :: EightBitInt -> Config -> (BL.Glue TeXLength)
lookupGlueRegister p conf =
    fromMaybe mempty $ scopedMapLookup glueRegister p conf

lookupMathGlueRegister :: EightBitInt -> Config -> (BL.Glue MathLength)
lookupMathGlueRegister p conf =
    fromMaybe mempty $ scopedMapLookup mathGlueRegister p conf

lookupTokenListRegister :: EightBitInt -> Config -> BalancedText
lookupTokenListRegister p conf =
    fromMaybe mempty $ scopedMapLookup tokenListRegister p conf

lookupBoxRegister :: EightBitInt -> Config -> Maybe (B.Box B.BoxContents)
lookupBoxRegister = scopedMapLookup boxRegister

setTeXIntRegister :: EightBitInt -> TeXIntVal -> GlobalFlag -> Config -> Config
setTeXIntRegister =
    insertKey texIntRegister $ \c _map -> c { texIntRegister = _map }

setLengthRegister :: EightBitInt -> TeXLength -> GlobalFlag -> Config -> Config
setLengthRegister =
    insertKey lengthRegister $ \c _map -> c { lengthRegister = _map }

setGlueRegister :: EightBitInt -> (BL.Glue TeXLength) -> GlobalFlag -> Config -> Config
setGlueRegister = insertKey glueRegister $ \c _map -> c { glueRegister = _map }

setMathGlueRegister
    :: EightBitInt
    -> (BL.Glue MathLength)
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

setBoxRegister :: EightBitInt -> B.Box B.BoxContents -> GlobalFlag -> Config -> Config
setBoxRegister = insertKey boxRegister $ \c _map -> c { boxRegister = _map }

delBoxRegister :: EightBitInt -> GlobalFlag -> Config -> Config
delBoxRegister = deleteKey boxRegister $ \c _map -> c { boxRegister = _map }

setBoxRegisterNullable :: EightBitInt
                            -> GlobalFlag -> Maybe (B.Box B.BoxContents) -> Config -> Config
setBoxRegisterNullable idx global = \case
    -- If the fetched box is null, delete the left-hand
    -- register's contents. Otherwise set the register to
    -- the fetched box's contents.
    Nothing -> delBoxRegister idx global
    Just b -> setBoxRegister idx b global

-- Scoped, but with unscoped references.

currentFontInfo
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[ConfigError]
       )
    => m FontInfo
currentFontInfo = mLookupCurrentFontNr >>= lookupFontInfo

currentFontMetrics
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[ConfigError]
       )
    => m TexFont
currentFontMetrics = fontMetrics <$> currentFontInfo
