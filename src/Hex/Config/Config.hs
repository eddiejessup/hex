{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}

module Hex.Config.Config where

import           Hexlude

import           Control.Monad.Catch      (MonadThrow)
import qualified Data.Containers          as D.C
import qualified Data.Generics.Product    as G.P

import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.Map.Strict          ((!?))
import qualified Data.Map.Strict          as Map
import qualified Data.Path                as D.Path
import           Path                     (Abs, Dir, File, Path, Rel,
                                           parseAbsDir)
import qualified Path.IO
import           System.Directory
import           System.IO                (hClose)

import           TFM                      (TexFont)
import qualified TFM

import qualified Hex.Box                  as B
import qualified Hex.BreakList            as BL
import qualified Hex.Config.Codes         as Code
import           Hex.Config.Parameters
import qualified Hex.Lex                  as Lex
import qualified Hex.Parse.AST            as AST
import           Hex.Resolve
import           Hex.Quantity
import qualified System.Log.FastLogger as Log

data Group
    = ScopeGroup Scope ScopeGroup
    | NonScopeGroup
    deriving stock (Show)

renderGroupType :: Group -> Text
renderGroupType = \case
    ScopeGroup _ (LocalStructureGroup trigger) ->
        "Scope/LocalStructure/" <> show trigger
    ScopeGroup _ ExplicitBoxGroup ->
        "Scope/ExplicitBox"
    NonScopeGroup ->
        "NonScope"

data ScopeGroup
    = LocalStructureGroup AST.CommandTrigger
    | ExplicitBoxGroup
    deriving stock (Show)

type RegisterMap v = Map EightBitInt v

data Scope =
    Scope { -- Fonts.
            currentFontNr :: Maybe TeXInt
          , familyMemberFonts :: HashMap (FontRange, TeXInt) TeXInt
            -- Control sequences.
          , csMap :: CSMap
            -- Char-code attribute maps.
          , catCodes :: Code.CatCodes
          , mathCodes :: Code.CharCodeMap Code.MathCode
          , lowercaseCodes :: Code.CharCodeMap Code.CaseChangeCode
          , uppercaseCodes :: Code.CharCodeMap Code.CaseChangeCode
          , spaceFactors :: Code.CharCodeMap Code.SpaceFactorCode
          , delimiterCodes :: Code.CharCodeMap Code.DelimiterCode
            -- Parameters.
          , texIntParameters :: HashMap TeXIntParameter TeXInt
          , lengthParameters :: HashMap LengthParameter Length
          , glueParameters :: HashMap GlueParameter (BL.Glue Length)
          , mathGlueParameters :: HashMap MathGlueParameter (BL.Glue MathLength)
          , tokenListParameters :: HashMap TokenListParameter BalancedText
            -- Registers.
          , texIntRegister :: RegisterMap TeXInt
          , lengthRegister :: RegisterMap Length
          , glueRegister :: RegisterMap (BL.Glue Length)
          , mathGlueRegister :: RegisterMap (BL.Glue MathLength)
          , tokenListRegister :: RegisterMap BalancedText
          , boxRegister :: RegisterMap (B.Box B.BoxContents)
          }
    deriving stock (Show, Generic)

newGlobalScope :: Scope
newGlobalScope =
    Scope { currentFontNr = Nothing
          , familyMemberFonts = mempty
          , csMap = defaultCSMap
          , catCodes = Code.usableCatCodes
          , mathCodes = Code.newMathCodes
          , lowercaseCodes = Code.newLowercaseCodes
          , uppercaseCodes = Code.newUppercaseCodes
          , spaceFactors = Code.newSpaceFactors
          , delimiterCodes = Code.newDelimiterCodes
          , texIntParameters = usableTeXIntParameters
          , lengthParameters = usableLengthParameters
          , glueParameters = usableGlueParameters
          , mathGlueParameters = newMathGlueParameters
          , tokenListParameters = newTokenListParameters
          , texIntRegister = mempty
          , lengthRegister = mempty
          , glueRegister = mempty
          , mathGlueRegister = mempty
          , tokenListRegister = mempty
          , boxRegister = mempty
          }

newLocalScope :: Scope
newLocalScope =
    Scope { currentFontNr = Nothing
          , familyMemberFonts = mempty
          , csMap = mempty
          , catCodes = mempty
          , mathCodes = mempty
          , lowercaseCodes = mempty
          , uppercaseCodes = mempty
          , spaceFactors = mempty
          , delimiterCodes = mempty
          , texIntParameters = mempty
          , lengthParameters = mempty
          , glueParameters = mempty
          , mathGlueParameters = mempty
          , tokenListParameters = mempty
          , texIntRegister = mempty
          , lengthRegister = mempty
          , glueRegister = mempty
          , mathGlueRegister = mempty
          , tokenListRegister = mempty
          , boxRegister = mempty
          }

data Config =
    Config { fontInfos            :: Map TeXInt FontInfo
           , searchDirectories    :: [Path Abs Dir]
           , specialTeXInts       :: HashMap SpecialTeXInt TeXInt
           , specialLengths       :: HashMap SpecialLength Length
             -- File streams.
           , logStream            :: Handle
           , outFileStreams       :: Map FourBitInt Handle
           , internalLoggerSet    :: Log.LoggerSet
             -- / File streams.
           , afterAssignmentToken :: Maybe Lex.Token
           , globalScope          :: Scope
           , groups               :: [Group]
           }
    deriving stock (Generic)

newtype ConfigError = ConfigError Text
    deriving stock (Show)

newConfig :: (MonadIO m, MonadThrow m) => [FilePath] -> m Config
newConfig extraSearchDirs = do
    cwdRaw <- liftIO getCurrentDirectory
    searchDirectories <- mapM parseAbsDir ([cwdRaw] <> extraSearchDirs)
    logHandle <- liftIO $ openFile "hex.log" WriteMode

    internalLoggerSet <- liftIO $ Log.newFileLoggerSet Log.defaultBufSize "hex_internal.log"

    pure Config { fontInfos = Map.empty
                , searchDirectories
                , specialTeXInts = newSpecialTeXInts
                , specialLengths = newSpecialLengths
                , logStream = logHandle
                , outFileStreams = mempty
                , afterAssignmentToken = Nothing
                , globalScope = newGlobalScope
                , groups = []
                , internalLoggerSet
                }

finaliseConfig :: MonadIO m => Config -> m ()
finaliseConfig config =
    liftIO $ hClose $ logStream config

-- Unscoped.

data FindFilePolicy
    = NoImplicitExtension
    | WithImplicitExtension Text

findFilePath
    :: ( MonadState st m
       , HasType Config st
       , MonadIO m
       , MonadError e m
       , AsType ConfigError e
       , AsType D.Path.PathError e
       )
    => FindFilePolicy
    -> [Path Abs Dir]
    -> Path Rel File
    -> m (Path Abs File)
findFilePath findPolicy extraPaths p =
    do
    dirs <- use $ typed @Config % field @"searchDirectories" % to (<> extraPaths)
    getTgtPath
        >>= Path.IO.findFile dirs
        >>= note (injectTyped $ ConfigError $ "Could not find file: " <> show p)
  where
    getTgtPath = case findPolicy of
        NoImplicitExtension ->
            pure p
        WithImplicitExtension ext ->
            D.Path.setFileExtension p ext

-- Font info.

data FontInfo =
    FontInfo { fontMetrics :: TexFont, hyphenChar, skewChar :: TeXInt }

readFontInfo
    :: ( MonadState st m
       , HasType Config st
       , MonadIO m
       , MonadError e m
       , AsType TFM.TFMError e
       )
    => Path Abs File
    -> m FontInfo
readFontInfo fontPath = do
    fontMetrics <- TFM.readTFMFancy fontPath
    hyphenChar <- use $ typed @Config % to (lookupTeXIntParameter DefaultHyphenChar)
    skewChar <- use $ typed @Config % to (lookupTeXIntParameter DefaultSkewChar)
    pure FontInfo { fontMetrics, hyphenChar, skewChar }

lookupFontInfo
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType ConfigError e
       )
    => TeXInt
    -> m FontInfo
lookupFontInfo fNr = do
    mayInfo <- use $ typed @Config % field @"fontInfos" % to (!? fNr)
    note (injectTyped (ConfigError "No such font number")) mayInfo

addFont :: (MonadState st m, HasType Config st) => FontInfo -> m TeXInt
addFont newInfo = do
    infos <- use $ typed @Config % field @"fontInfos"
    let newKey = case Map.lookupMax infos of
            Nothing     -> 0
            Just (i, _) -> succ i
        newInfos = Map.insert newKey newInfo infos
    assign' (typed @Config % field @"fontInfos") newInfos
    pure newKey

modifyFont :: (MonadState st m, HasType Config st) => TeXInt -> (FontInfo -> FontInfo) -> m ()
modifyFont fNr f =
    modifying' (typed @Config % field @"fontInfos") (Map.adjust f fNr)

-- Special quantities.

lookupSpecialTeXInt :: SpecialTeXInt -> Config -> TeXInt
lookupSpecialTeXInt p c = HashMap.lookupDefault 0 p (specialTeXInts c)

lookupSpecialLength :: SpecialLength -> Config -> Length
lookupSpecialLength p c = HashMap.lookupDefault (Length 0) p (specialLengths c)

setSpecialTeXInt :: SpecialTeXInt -> TeXInt -> Config -> Config
setSpecialTeXInt p v c =
    c{ specialTeXInts = HashMap.insert p v $ specialTeXInts c }

setSpecialLength :: SpecialLength -> Length -> Config -> Config
setSpecialLength p v c =
    c{ specialLengths = HashMap.insert p v $ specialLengths c }

-- Scoped.

localScopeL :: Lens' Config Scope
localScopeL = lens getter setter
  where
    getter :: Config -> Scope
    getter Config{ globalScope, groups } = fromMaybe globalScope $ asum $ toScope <$> groups
      where
        toScope = \case
            ScopeGroup scope _ ->
                Just scope
            _ ->
                Nothing

    setter :: Config -> Scope -> Config
    setter c@Config{ globalScope, groups } sc =
        let (newGlob, newGroups) = go [] groups
        in c
            & G.P.field @"globalScope" .~ newGlob
            & G.P.field @"groups" .~ newGroups
      where
        go befGroups aftGroups  = case aftGroups of
                [] ->
                    (sc, befGroups)
                ScopeGroup _ scopeGroupType : restAftGroups ->
                    (globalScope, befGroups ++ (ScopeGroup sc scopeGroupType : restAftGroups))
                nonScopeGroup : restAftGroups ->
                    go (befGroups ++ [nonScopeGroup]) restAftGroups

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

modifyKey :: D.C.IsMap map
          => Lens' Scope map
          -> D.C.ContainerKey map
          -> KeyOperation (D.C.MapValue map)
          -> ScopeFlag
          -> Config
          -> Config
modifyKey mapLens k keyOp scopeFlag c =
    case scopeFlag of
        Global ->
            c   & G.P.field @"globalScope" %~ modOp
                & G.P.field @"groups" %~ fmap (modGroupScope deleteKeyFromScope)
        Local ->
            c & localScopeL %~ modOp
  where
    modOp = case keyOp of
        DeleteVal   -> deleteKeyFromScope
        InsertVal v -> insertKeyToScope v

    insertKeyToScope v = mapLens %~ D.C.insertMap k v

    deleteKeyFromScope = mapLens %~ D.C.deleteMap k

insertKey :: D.C.IsMap map
          => Lens' Scope map
          -> D.C.ContainerKey map
          -> D.C.MapValue map
          -> ScopeFlag
          -> Config
          -> Config
insertKey mapLens k v = modifyKey mapLens k (InsertVal v)

deleteKey :: D.C.IsMap map
          => Lens' Scope map
          -> D.C.ContainerKey map
          -> ScopeFlag
          -> Config
          -> Config
deleteKey mapLens k = modifyKey mapLens k DeleteVal

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
    :: D.C.IsMap map
    => (Scope -> map)
    -> D.C.ContainerKey map
    -> Config
    -> Maybe (D.C.MapValue map)
scopedMapLookup getMap k = scopedLookup (D.C.lookup k . getMap)

-- Font number (scoped).
lookupCurrentFontNr :: Config -> Maybe TeXInt
lookupCurrentFontNr = scopedLookup currentFontNr

mLookupCurrentFontNr
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType ConfigError e
       )
    => m TeXInt
mLookupCurrentFontNr = do
    mayFNr <- use $ typed @Config % to lookupCurrentFontNr
    note (injectTyped $ ConfigError "Font number isn't set") mayFNr

selectFontNr :: TeXInt -> ScopeFlag -> Config -> Config
selectFontNr n scopeFlag c@Config{ globalScope, groups } =
    case scopeFlag of
        Global ->
            c{ globalScope = selectFontInScope globalScope
             , groups = modGroupScope deselectFontInScope <$> groups
             }
        Local ->
            c & localScopeL %~ selectFontInScope
  where
    deselectFontInScope scope = scope{ currentFontNr = Nothing }
    selectFontInScope scope = scope{ currentFontNr = Just n }

setFamilyMemberFont
    :: (FontRange, TeXInt)
    -> TeXInt
    -> ScopeFlag
    -> Config
    -> Config
setFamilyMemberFont = insertKey (G.P.field @"familyMemberFonts")

lookupFontFamilyMember
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType ConfigError e
       )
    => (FontRange, TeXInt)
    -> m TeXInt
lookupFontFamilyMember k = do
    mayMember <- use $ typed @Config % to (scopedMapLookup familyMemberFonts k)
    note (injectTyped $ ConfigError $ "Family member undefined: " <> show k) mayMember

-- Control sequences.
lookupCS :: Lex.ControlSequenceLike -> Config -> Maybe ResolvedToken
lookupCS = scopedMapLookup csMap

lookupCSProper :: Lex.ControlSequence -> Config -> Maybe ResolvedToken
lookupCSProper cs = lookupCS (Lex.ControlSequenceProper cs)

setControlSequence
    :: Lex.ControlSequenceLike
    -> ResolvedToken
    -> ScopeFlag
    -> Config
    -> Config
setControlSequence = insertKey (G.P.field @"csMap")

-- Codes.
lookupCatCode :: Code.CharCode -> Config -> Code.CatCode
lookupCatCode t conf = fromMaybe Code.Invalid $ scopedMapLookup catCodes t conf

lookupChangeCaseCode :: VDirection -> Code.CharCode -> Config -> Code.CaseChangeCode
lookupChangeCaseCode d t conf =
    let codes = case d of
            Upward   -> uppercaseCodes
            Downward -> lowercaseCodes
    in
        fromMaybe Code.NoCaseChange $ scopedMapLookup codes t conf

updateCharCodeMap
    :: ( MonadError e m
       , AsType ConfigError e

       , MonadState st m
       , HasType Config st
       )
    => CodeType
    -> Code.CharCode
    -> TeXInt
    -> ScopeFlag
    -> m ()
updateCharCodeMap t c n scopeFlag = do
    insert <- case t of
        CategoryCodeType       ->
            noteConfigError (Code.fromTeXInt n) <&> insertKey (G.P.field @"catCodes") c
        MathCodeType           ->
            noteConfigError (Code.fromTeXInt n) <&> insertKey (G.P.field @"mathCodes") c
        ChangeCaseCodeType dir -> do
            v <- noteConfigError $ Code.fromTeXInt n
            pure $ case dir of
                Upward ->
                    insertKey (G.P.field @"uppercaseCodes") c v
                Downward ->
                    insertKey (G.P.field @"lowercaseCodes") c v
        SpaceFactorCodeType    ->
            noteConfigError $ Code.fromTeXInt n <&> insertKey (G.P.field @"spaceFactors") c
        DelimiterCodeType      ->
            noteConfigError $ Code.fromTeXInt n <&> insertKey (G.P.field @"delimiterCodes") c
    modifying' (typed @Config) (insert scopeFlag)
  where
    noteConfigError :: (MonadError e m, AsType ConfigError e) => Maybe a -> m a
    noteConfigError =
        note (injectTyped $ ConfigError $ "Invalid target value for code type "
              <> show t <> ": " <> show n)

-- Parameters and special quantities.
lookupTeXIntParameter :: TeXIntParameter -> Config -> TeXInt
lookupTeXIntParameter p conf =
    fromMaybe 0 $ scopedMapLookup texIntParameters p conf

lookupLengthParameter :: LengthParameter -> Config -> Length
lookupLengthParameter p conf =
    fromMaybe 0 $ scopedMapLookup lengthParameters p conf

lookupGlueParameter :: GlueParameter -> Config -> BL.Glue Length
lookupGlueParameter p conf =
    fromMaybe mempty $ scopedMapLookup glueParameters p conf

lookupMathGlueParameter :: MathGlueParameter -> Config -> BL.Glue MathLength
lookupMathGlueParameter p conf =
    fromMaybe mempty $ scopedMapLookup mathGlueParameters p conf

lookupTokenListParameter :: TokenListParameter -> Config -> BalancedText
lookupTokenListParameter p conf =
    fromMaybe mempty $ scopedMapLookup tokenListParameters p conf

setTeXIntParameter
    :: TeXIntParameter
    -> TeXInt
    -> ScopeFlag
    -> Config
    -> Config
setTeXIntParameter = insertKey (G.P.field @"texIntParameters")

setLengthParameter
    :: LengthParameter
    -> Length
    -> ScopeFlag
    -> Config
    -> Config
setLengthParameter = insertKey (G.P.field @"lengthParameters")

setGlueParameter :: GlueParameter -> BL.Glue Length -> ScopeFlag -> Config -> Config
setGlueParameter = insertKey (G.P.field @"glueParameters")

setMathGlueParameter
    :: MathGlueParameter
    -> BL.Glue MathLength
    -> ScopeFlag
    -> Config
    -> Config
setMathGlueParameter = insertKey (G.P.field @"mathGlueParameters")

setTokenListParameter
    :: TokenListParameter
    -> BalancedText
    -> ScopeFlag
    -> Config
    -> Config
setTokenListParameter = insertKey (G.P.field @"tokenListParameters")

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
lookupTeXIntRegister :: EightBitInt -> Config -> TeXInt
lookupTeXIntRegister p conf =
    fromMaybe 0 $ scopedMapLookup texIntRegister p conf

lookupLengthRegister :: EightBitInt -> Config -> Length
lookupLengthRegister p conf =
    fromMaybe 0 $ scopedMapLookup lengthRegister p conf

lookupGlueRegister :: EightBitInt -> Config -> BL.Glue Length
lookupGlueRegister p conf =
    fromMaybe mempty $ scopedMapLookup glueRegister p conf

lookupMathGlueRegister :: EightBitInt -> Config -> BL.Glue MathLength
lookupMathGlueRegister p conf =
    fromMaybe mempty $ scopedMapLookup mathGlueRegister p conf

lookupTokenListRegister :: EightBitInt -> Config -> BalancedText
lookupTokenListRegister p conf =
    fromMaybe mempty $ scopedMapLookup tokenListRegister p conf

lookupBoxRegister :: EightBitInt -> Config -> Maybe (B.Box B.BoxContents)
lookupBoxRegister = scopedMapLookup boxRegister

setTeXIntRegister :: EightBitInt -> TeXInt -> ScopeFlag -> Config -> Config
setTeXIntRegister =
    insertKey (G.P.field @"texIntRegister")

setLengthRegister :: EightBitInt -> Length -> ScopeFlag -> Config -> Config
setLengthRegister =
    insertKey (G.P.field @"lengthRegister")

setGlueRegister :: EightBitInt -> BL.Glue Length -> ScopeFlag -> Config -> Config
setGlueRegister = insertKey (G.P.field @"glueRegister")

setMathGlueRegister
    :: EightBitInt
    -> BL.Glue MathLength
    -> ScopeFlag
    -> Config
    -> Config
setMathGlueRegister = insertKey (G.P.field @"mathGlueRegister")

setTokenListRegister
    :: EightBitInt
    -> BalancedText
    -> ScopeFlag
    -> Config
    -> Config
setTokenListRegister =
    insertKey (G.P.field @"tokenListRegister")

setBoxRegister :: EightBitInt -> B.Box B.BoxContents -> ScopeFlag -> Config -> Config
setBoxRegister = insertKey (G.P.field @"boxRegister")

delBoxRegister :: EightBitInt -> ScopeFlag -> Config -> Config
delBoxRegister = deleteKey (G.P.field @"boxRegister")

setBoxRegisterNullable :: EightBitInt
                            -> ScopeFlag -> Maybe (B.Box B.BoxContents) -> Config -> Config
setBoxRegisterNullable idx global = \case
    -- If the fetched box is null, delete the left-hand
    -- register's contents. Otherwise set the register to
    -- the fetched box's contents.
    Nothing -> delBoxRegister idx global
    Just b -> setBoxRegister idx b global

-- Scoped, but with unscoped references.

currentFontInfo
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType ConfigError e
       )
    => m FontInfo
currentFontInfo = mLookupCurrentFontNr >>= lookupFontInfo

currentFontMetrics
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType ConfigError e
       )
    => m TexFont
currentFontMetrics = fontMetrics <$> currentFontInfo
