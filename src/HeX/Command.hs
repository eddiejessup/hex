module HeX.Command where

import           HeXlude

import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , throwError
                                                , withExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT
                                                , get
                                                , liftIO
                                                , modify
                                                )
import qualified Data.Text                     as Text
import qualified Data.HashMap.Strict           as HMap
import           Data.Path                      ( findFilePath )
import           Path                           ( Path
                                                , Rel
                                                , File
                                                )
import qualified Path

import qualified TFM
import           TFM                            ( TexFont(..) )

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Categorise                 ( CharCode )
import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Lex                       as Lex
import qualified HeX.Parse                     as HP

-- Horizontal mode.

characterBox
    :: (MonadReader Config m, MonadError Text m) => CharCode -> m B.Character
characterBox char = do
    fontMetrics <- currentFontMetrics
    let toSP = TFM.designScaleSP fontMetrics
    TFM.Character { TFM.width, TFM.height, TFM.depth } <-
        liftMaybe "No such character"
            $ (HMap.lookup char $ characters fontMetrics)
    pure B.Character { B.char       = char
                     , B.charWidth  = toSP width
                     , B.charHeight = toSP height
                     , B.charDepth  = toSP depth
                     }

spaceGlue :: (MonadReader Config m, MonadError Text m) => m BL.Glue
spaceGlue = do
    fontMetrics@TexFont { spacing, spaceStretch, spaceShrink } <-
        currentFontMetrics
    let toSP   = TFM.designScaleSP fontMetrics
        toFlex = toSP >>> fromIntegral >>> BL.finiteFlex
    pure BL.Glue { BL.dimen   = toSP spacing
                 , BL.stretch = toFlex spaceStretch
                 , BL.shrink  = toFlex spaceShrink
                 }

-- Mode independent.

loadFont
    :: (MonadState Config m, MonadIO m, MonadError Text m)
    => Path Rel File
    -> HP.FontSpecification
    -> m B.FontDefinition
loadFont relPath fontSpec = do
    fontInfo_ <- readOnState $ readRelPath relPath
    let designSizeSP = TFM.designSizeSP $ fontMetrics fontInfo_
    scaleRatio <- readOnState $ evaluateFontSpecification designSizeSP fontSpec
    fontName   <- extractFontName relPath
    liftIO
        $  putStrLn
        $  "Loading font: "
        <> showT fontName
        <> ", with design size: "
        <> showT designSizeSP
        <> ", with scale ratio: "
        <> showT scaleRatio
    fNr <- addFont fontInfo_
    pure B.FontDefinition { B.fontNr           = fNr
        -- TODO: Improve mapping of name and path.
                          , B.fontPath         = relPath
                          , B.fontName         = fontName
                          , B.fontInfo         = fontMetrics fontInfo_
                          , B.scaleFactorRatio = scaleRatio
                          }
  where
    readRelPath p =
        findFilePath p
            <$> asks searchDirectories
            >>= liftThrow ("Could not find font: " <> showT p)
            >>= readFontInfo

    stripExtension p =
        liftThrow "Could not strip font extension" $ Path.setFileExtension "" p

    fileName = Path.filename >>> Path.toFilePath >>> toS

    extractFontName p = stripExtension p <&> fileName

selectFont :: MonadState Config m => Int -> HP.GlobalFlag -> m B.FontSelection
selectFont n globalFlag = do
    modify $ selectFontNr n globalFlag
    pure $ B.FontSelection n

readOnState :: MonadState r m => ReaderT r m b -> m b
readOnState f = get >>= runReaderT f

readOnConfState
    :: (HP.InhibitableStream s, MonadState s m)
    => ReaderT Config (StateT Config m) a
    -> m a
readOnConfState f = HP.runConfState $ readOnState f

modConfState
    :: (MonadState s m, HP.InhibitableStream s) => (Config -> Config) -> m ()
modConfState x = HP.runConfState $ modify $ x

setIntegerVariable
    :: (MonadState Config m, MonadError Text m)
    => HP.IntegerVariable
    -> HP.GlobalFlag
    -> IntVal
    -> m ()
setIntegerVariable v globalFlag tgt = case v of
    HP.ParamVar p -> modify $ setIntegerParameter p tgt globalFlag
    HP.RegisterVar iRaw ->
        readOnState (evaluateEightBitInt iRaw)
            >>= (\i -> modify $ setIntegerRegister i tgt globalFlag)

setLengthVariable
    :: (MonadState Config m, MonadError Text m)
    => HP.LengthVariable
    -> HP.GlobalFlag
    -> LenVal
    -> m ()
setLengthVariable v globalFlag tgt = case v of
    HP.ParamVar p -> modify $ setLengthParameter p tgt globalFlag
    HP.RegisterVar iRaw ->
        readOnState (evaluateEightBitInt iRaw)
            >>= (\i -> modify $ setLengthRegister i tgt globalFlag)

setGlueVariable
    :: (MonadState Config m, MonadError Text m)
    => HP.GlueVariable
    -> HP.GlobalFlag
    -> BL.Glue
    -> m ()
setGlueVariable v globalFlag tgt = case v of
    HP.ParamVar p -> modify $ setGlueParameter p tgt globalFlag
    HP.RegisterVar iRaw ->
        readOnState (evaluateEightBitInt iRaw)
            >>= (\i -> modify $ setGlueRegister i tgt globalFlag)

setMathGlueVariable
    :: (MonadState Config m, MonadError Text m)
    => HP.MathGlueVariable
    -> HP.GlobalFlag
    -> BL.MathGlue
    -> m ()
setMathGlueVariable v globalFlag tgt = case v of
    HP.ParamVar p -> modify $ setMathGlueParameter p tgt globalFlag
    HP.RegisterVar iRaw ->
        readOnState (evaluateEightBitInt iRaw)
            >>= (\i -> modify $ setMathGlueRegister i tgt globalFlag)

setTokenListVariable
    :: (MonadState Config m, MonadError Text m)
    => HP.TokenListVariable
    -> HP.GlobalFlag
    -> HP.BalancedText
    -> m ()
setTokenListVariable v globalFlag tgt = case v of
    HP.ParamVar p -> modify $ setTokenListParameter p tgt globalFlag
    HP.RegisterVar iRaw ->
        readOnState (evaluateEightBitInt iRaw)
            >>= (\i -> modify $ setTokenListRegister i tgt globalFlag)

showLexTok :: Lex.Token -> Text
showLexTok = \case
    Lex.CharCatToken Lex.CharCat { Lex.char, Lex.cat = Lex.Letter } ->
        Text.singleton char
    Lex.CharCatToken Lex.CharCat { Lex.char, Lex.cat = Lex.Other } ->
        Text.singleton char
    Lex.CharCatToken Lex.CharCat { Lex.char, Lex.cat = Lex.Space } ->
        Text.singleton char
    Lex.CharCatToken         cc                       -> showT cc
    Lex.ControlSequenceToken (Lex.ControlSequence cs) -> "\\" <> toS cs

showPrimTok :: HP.PrimitiveToken -> Text
showPrimTok = \case
    HP.UnexpandedTok   t   -> showLexTok t
    HP.SubParserError  err -> err
    HP.ResolutionError cs  -> "Unknown control sequence: " <> showT cs
    pt                     -> showT pt

showBalancedText :: HP.BalancedText -> Text
showBalancedText (HP.BalancedText txt) = Text.concat $ showLexTok <$> txt

showExpandedBalancedText :: HP.ExpandedBalancedText -> Text
showExpandedBalancedText (HP.ExpandedBalancedText txt) =
    Text.concat $ showPrimTok <$> txt

getStream :: HMap.HashMap FourBitInt Handle -> IntVal -> Maybe Handle
getStream strms n = do
    fourBitn <- newFourBitInt n
    HMap.lookup fourBitn strms

data BuildError s
  = ParseError s
  | ConfigError Text

liftConfigError :: Monad m => ExceptT Text m a -> ExceptBuildT s m a
liftConfigError = withExceptT ConfigError

liftConfState
    :: (HP.InhibitableStream s, MonadState s m)
    => StateT Config (ExceptT Text m) a
    -> ExceptBuildT s m a
liftConfState x = liftConfigError $ HP.runConfState x

liftReadOnConfState
    :: (HP.InhibitableStream s, MonadState s m)
    => ReaderT Config (StateT Config (ExceptT Text m)) a
    -> ExceptBuildT s m a
liftReadOnConfState x = liftConfigError $ readOnConfState x

throwConfigError :: MonadError (BuildError s) m => Text -> m a
throwConfigError s = throwError $ ConfigError s

liftMaybeConfigError :: MonadError (BuildError s) m => Text -> Maybe a -> m a
liftMaybeConfigError s = liftMaybe (ConfigError s)

type ExceptBuildT s m a = ExceptT (BuildError (HP.ParseErrorBundle s)) m a
