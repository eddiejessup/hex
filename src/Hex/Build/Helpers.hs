module Hex.Build.Helpers where

import           Hexlude

import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.Path           as D.Path

import           TFM                 (TexFont (..))
import qualified TFM

import qualified Hex.Box             as B
import           Hex.BreakList       (HListElem, VListElem)
import qualified Hex.BreakList       as BL
import qualified Hex.Config.Codes    as Code
import           Hex.Config
import           Hex.Evaluate
import qualified Hex.Lex             as Lex
import           Hex.Quantity
import qualified Hex.Parse.AST as AST
import qualified Hex.Resolve as Tok
import qualified Hex.Parse.TokenParser.Class as P
import Hex.Parse.CommandParser.Command (parseCommand)

glueToElem
    :: MonadEvaluate m AST.Glue
    => AST.Glue
    -> m BL.VListElem
glueToElem g =
    BL.ListGlue <$> astEval g

ruleToElem
    :: MonadEvaluate m AST.Length
    => AST.Rule
    -> m Length
    -> m Length
    -> m Length
    -> m BL.VListElem
ruleToElem AST.Rule { AST.width, AST.height, AST.depth } defaultW defaultH defaultD =
    do
    rule <- B.Rule
                <$> maybe defaultW astEval width
                <*> maybe defaultH astEval height
                <*> maybe defaultD astEval depth
    pure $ BL.VListBaseElem $ B.ElemRule rule

-- Assume we are adding a non-rule box of height h to the vertical list.
-- Let \prevdepth = p, \lineskiplimit = l, \baselineskip = (b plus y minus z).
-- Add interline glue, above the new box, of:
-- If p ≤ −1000 pt:
--    No glue.
-- Otherwise, if b−p−h ≥ l:
--    (b−p−h) plus y minus z
-- Otherwise:
--    \lineskip
-- Then set \prevdepth to the depth of the new box.
addVListElem
    :: (MonadState st m, HasType Config st)
    => BL.VList
    -> BL.VListElem
    -> m BL.VList
addVListElem (BL.VList accSeq) = \case
    e@(BL.VListBaseElem (B.ElemBox b)) ->
        do
        _prevDepth <- uses (typed @Config) $ lookupSpecialLength Tok.PrevDepth
        BL.Glue blineLength blineStretch blineShrink <- uses (typed @Config) $ lookupGlueParameter Tok.BaselineSkip
        skipLimit <- uses (typed @Config) $ lookupLengthParameter Tok.LineSkipLimit
        skip <- uses (typed @Config) $ lookupGlueParameter Tok.LineSkip
        modifying' (typed @Config) $ setSpecialLength Tok.PrevDepth (naturalDepth e)
        pure $ BL.VList $ if _prevDepth <= -oneKPt
            then
                accSeq :|> e
            else
                let proposedBaselineLength = blineLength - _prevDepth - naturalHeight b
                -- Intuition: set the distance between baselines to \baselineskip, but no
                -- closer than \lineskiplimit [theBaselineLengthMin], in which case
                -- \lineskip [theMinBaselineGlue] is used.
                    glue = BL.ListGlue $ if proposedBaselineLength >= skipLimit
                        then BL.Glue proposedBaselineLength blineStretch blineShrink
                        else skip
                in (accSeq :|> glue) :|> e
    e ->
        pure (BL.VList (accSeq :|> e))

hModeCharacterElem
    :: ( MonadError e m
       , AsType ConfigError e

       , MonadEvaluate m AST.CharCodeRef

       , MonadState st m -- Read-only
       , HasType Config st
       )
    => AST.CharCodeRef
    -> m HListElem
hModeCharacterElem c =
    astEval c
        >>= characterBox
        <&> B.ElemCharacter
        <&> BL.HListHBaseElem

hModeSpaceElem
    :: ( MonadError e m
       , AsType ConfigError e

       , MonadState st m -- Read-only
       , HasType Config st
       )
    => m HListElem
hModeSpaceElem =
    BL.HVListElem . BL.ListGlue <$> spaceGlue

hModeRuleElem
    :: MonadEvaluate m AST.Length
    => AST.Rule
    -> m HListElem
hModeRuleElem rule =
    BL.HVListElem <$> ruleToElem rule defaultWidth defaultHeight defaultDepth
  where
    defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
    defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
    defaultDepth = pure 0

hModeStartParagraph
    :: ( MonadError e m
       , MonadState st m -- Read-only
       , HasType Config st
       )
    => Tok.IndentFlag
    -> m (Maybe HListElem)
hModeStartParagraph = \case
    Tok.DoNotIndent ->
        pure Nothing
    -- \indent: An empty box of width \parindent is appended to the current
    -- list, and the space factor is set to 1000.
    -- TODO: Space factor.
    Tok.Indent ->
        Just <$> uses (typed @Config) parIndentBox

vModeRuleElem
    :: ( MonadEvaluate m AST.Length
       , MonadState st m -- Read-only
       , HasType Config st
       )
    => AST.Rule
    -> m VListElem
vModeRuleElem rule =
    ruleToElem rule defaultWidth defaultHeight defaultDepth
  where
    defaultWidth = use $ typed @Config % to (lookupLengthParameter Tok.HSize)
    defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
    defaultDepth = pure 0

-- Horizontal mode commands.

characterBox
    :: ( MonadState st m -- Read-only
       , HasType Config st

       , MonadError e m
       , AsType ConfigError e
       )
    => CharCode
    -> m B.Character
characterBox char = do
    fontMetrics <- use (typed @Config) >>= currentFontMetrics
    let toSP = TFM.designScaleSP fontMetrics
    TFM.Character { TFM.width, TFM.height, TFM.depth } <-
        note (injectTyped $ ConfigError "No such character")
        (IntMap.lookup (fromIntegral $ Code.codeWord char) (characters fontMetrics))
    pure B.Character { B.char       = char
                     , B.charWidth  = toSP width
                     , B.charHeight = toSP height
                     , B.charDepth  = toSP depth
                     }

spaceGlue
    :: ( MonadState st m -- Read-only
       , HasType Config st

       , MonadError e m
       , AsType ConfigError e
       )
    => m (BL.Glue Length)
spaceGlue = do
    fontMetrics@TexFont { spacing, spaceStretch, spaceShrink } <- use (typed @Config) >>= currentFontMetrics
    let toSP   = TFM.designScaleSP fontMetrics
        toFlex = toSP >>> fromIntegral >>> BL.finiteFlex
    pure BL.Glue { BL.dimen   = toSP spacing
                 , BL.stretch = toFlex spaceStretch
                 , BL.shrink  = toFlex spaceShrink
                 }

-- Mode independent.

loadFont
    :: ( MonadState st m
       , HasType Config st
       , MonadIO m
       , MonadError e m
       , AsType D.Path.PathError e
       , AsType ConfigError e
       , AsType TFM.TFMError e
       , MonadEvaluate m AST.Length
       , MonadEvaluate m AST.TeXInt
       )
    => AST.TeXFilePath
    -> AST.FontSpecification
    -> m B.FontDefinition
loadFont (AST.TeXFilePath path) fontSpec = do
    fontName <- D.Path.fileNameText path
    info@FontInfo{ fontMetrics } <- findFilePath (WithImplicitExtension "tfm") [] path >>= readFontInfo
    let designSizeSP = TFM.designSizeSP fontMetrics
    scaleRatio <- evaluateFontSpecification designSizeSP fontSpec
    -- liftIO
    --     putText
    --     "Loading font: "
    --     <> show path
    --     <> ", with design size: "
    --     <> show designSizeSP
    --     <> ", with scale ratio: "
    --     <> show scaleRatio
    fNr <- addFont info
    -- TODO: Improve mapping of name and path.
    pure B.FontDefinition { B.fontDefChecksum    = TFM.checksum fontMetrics
                          , B.fontDefDesignSize  = designSizeSP
                          , B.fontDefDesignScale = TFM.designScaleSP fontMetrics scaleRatio
                          , B.fontNr             = fNr
                          , B.fontPath           = path
                          , B.fontName           = fontName
                          }

selectFont :: (MonadState st m, HasType Config st) => TeXInt -> Tok.ScopeFlag -> m ()
selectFont n scopeFlag = modifying' (typed @Config) $ selectFontNr n scopeFlag

getFileStream :: Map.Map FourBitInt Handle -> TeXInt -> Maybe Handle
getFileStream strms (TeXInt n) = do
    fourBitN <- newFourBitInt n
    Map.lookup fourBitN strms

fetchBox
  :: ( MonadState st m
     , HasType Config st
     , MonadEvaluate m AST.EightBitTeXInt
     )
  => Tok.BoxFetchMode
  -> AST.EightBitTeXInt
  -> m (Maybe (B.Box B.BoxContents))
fetchBox fetchMode idx = do
  eIdx <- astEval idx
  fetchedMaybeBox <- use $ typed @Config % to (lookupBoxRegister eIdx)
  case fetchMode of
    Tok.Lookup -> pure ()
    Tok.Pop -> modifying' (typed @Config) $ delBoxRegister eIdx Tok.Local
  pure fetchedMaybeBox

-- Showing things.

showLexTok :: Lex.Token -> [CharCode]
showLexTok = \case
    Lex.CharCatToken Lex.CharCat { Lex.char } ->
        [char]
    Lex.ControlSequenceToken (Lex.ControlSequence bs) ->
        Code.CharCode_ '\\' : (Code.CharCode <$> BS.unpack bs)

showPrimTok :: Tok.PrimitiveToken -> [CharCode]
showPrimTok = \case
    Tok.UnresolvedTok t -> showLexTok t
    pt                 -> unsafeCodesFromChars (show pt)

showBalancedText :: Tok.BalancedText -> [CharCode]
showBalancedText (Tok.BalancedText lexToks) =
    concat $ toList . showLexTok <$> lexToks

showExpandedBalancedText :: Tok.ExpandedBalancedText -> [CharCode]
showExpandedBalancedText (Tok.ExpandedBalancedText primToks) =
    concat $ toList . showPrimTok <$> primToks

-- Looping.

runLoop :: Monad m => (m (Maybe b)) -> m b
runLoop f = go
  where
    go = f >>= \case
        Nothing ->
          go
        Just b ->
          pure b

runCommandLoop
  :: ( MonadState st m
     , HasType Config st

     , P.MonadTokenParse m

     , MonadError e m
     , AsType ConfigError e
     , AsType EvaluationError e

     , MonadSlog m
     )
  => (AST.Command -> m (Maybe b))
  -> m b
runCommandLoop runCommand = runLoop parseAndRunCommand
  where
    parseAndRunCommand = do
      command <- parseCommand
      sLogStampedJSON "Parsed command"
        [ ("command", toJSON command)
        ]
      undefined -- checkPointStream
      runCommand command
      undefined -- commitStream
