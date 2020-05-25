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
import qualified Hex.Parse           as HP
import           Hex.Quantity

glueToElem
    :: ( MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e

       , MonadState st m -- Read-only
       , HasType Config st
       )
    => HP.Glue
    -> m BL.VListElem
glueToElem g =
    BL.ListGlue <$> texEvaluate g

ruleToElem
    :: ( MonadState st m -- Read-only
       , HasType Config st
       , MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e
       )
    => HP.Rule
    -> m Length
    -> m Length
    -> m Length
    -> m BL.VListElem
ruleToElem HP.Rule { HP.width, HP.height, HP.depth } defaultW defaultH defaultD =
    do
    rule <- B.Rule
                <$> maybe defaultW texEvaluate width
                <*> maybe defaultH texEvaluate height
                <*> maybe defaultD texEvaluate depth
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
        _prevDepth <- gets $ lookupSpecialLength HP.PrevDepth . getTyped @Config
        BL.Glue blineLength blineStretch blineShrink <- gets $ lookupGlueParameter HP.BaselineSkip . getTyped @Config
        skipLimit <- gets $ lookupLengthParameter HP.LineSkipLimit . getTyped @Config
        skip <- gets $ lookupGlueParameter HP.LineSkip . getTyped @Config
        modify $ typed @Config %~ setSpecialLength HP.PrevDepth (naturalDepth e)
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
       , AsType EvaluationError e
       , AsType ConfigError e

       , MonadState st m -- Read-only
       , HasType Config st
       )
    => HP.CharCodeRef
    -> m HListElem
hModeCharacterElem c =
    texEvaluate c
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
    :: ( MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e

       , MonadState st m -- Read-only
       , HasType Config st
       )
    => HP.Rule
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
    => HP.IndentFlag
    -> m (Maybe HListElem)
hModeStartParagraph = \case
    HP.DoNotIndent ->
        pure Nothing
    -- \indent: An empty box of width \parindent is appended to the current
    -- list, and the space factor is set to 1000.
    -- TODO: Space factor.
    HP.Indent ->
        Just <$> gets (view $ typed @Config % to parIndentBox)

vModeRuleElem
    :: ( MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e
       , MonadState st m -- Read-only
       , HasType Config st
       )
    => HP.Rule
    -> m VListElem
vModeRuleElem rule =
    ruleToElem rule defaultWidth defaultHeight defaultDepth
  where
    defaultWidth = gets $ view $ typed @Config % to (lookupLengthParameter HP.HSize)
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
    fontMetrics <- currentFontMetrics
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
    fontMetrics@TexFont { spacing, spaceStretch, spaceShrink } <- currentFontMetrics
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
       , AsType EvaluationError e
       )
    => HP.TeXFilePath
    -> HP.FontSpecification
    -> m B.FontDefinition
loadFont (HP.TeXFilePath path) fontSpec = do
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

selectFont :: (MonadState st m, HasType Config st) => TeXInt -> HP.ScopeFlag -> m ()
selectFont n scopeFlag = modify $ typed @Config %~ selectFontNr n scopeFlag

getFileStream :: Map.Map FourBitInt Handle -> TeXInt -> Maybe Handle
getFileStream strms (TeXInt n) = do
    fourBitN <- newFourBitInt n
    Map.lookup fourBitN strms

fetchBox
  :: ( MonadState st m
     , HasType Config st
     , MonadError e m
     , AsType ConfigError e
     , AsType EvaluationError e
     )
  => HP.BoxFetchMode
  -> HP.EightBitTeXInt
  -> m (Maybe (B.Box B.BoxContents))
fetchBox fetchMode idx = do
  eIdx <- texEvaluate idx
  fetchedMaybeBox <- gets $ view $ typed @Config % to (lookupBoxRegister eIdx)
  case fetchMode of
    HP.Lookup -> pure ()
    HP.Pop -> modify $ typed @Config %~ delBoxRegister eIdx HP.Local
  pure fetchedMaybeBox

-- Showing things.

showLexTok :: Lex.Token -> [CharCode]
showLexTok = \case
    Lex.CharCatToken Lex.CharCat { Lex.char } ->
        [char]
    Lex.ControlSequenceToken (Lex.ControlSequence bs) ->
        Code.CharCode_ '\\' : (Code.CharCode <$> BS.unpack bs)

showPrimTok :: HP.PrimitiveToken -> [CharCode]
showPrimTok = \case
    HP.UnresolvedTok t -> showLexTok t
    pt                 -> unsafeCodesFromChars (show pt)

showBalancedText :: HP.BalancedText -> [CharCode]
showBalancedText (HP.BalancedText lexToks) =
    concat $ toList . showLexTok <$> lexToks

showExpandedBalancedText :: HP.ExpandedBalancedText -> [CharCode]
showExpandedBalancedText (HP.ExpandedBalancedText primToks) =
    concat $ toList . showPrimTok <$> primToks

-- Looping.

runLoop :: Monad m => (s -> a -> m (s, a, Maybe b)) -> s -> a -> m (s, a, b)
runLoop f = go
  where
    go s state_ = do
      (newS, newState, recRes) <- f s state_
      case recRes of
        Nothing ->
          go newS newState
        Just b ->
          pure (newS, newState, b)

runCommandLoop
  :: ( MonadState st m
     , HasType Config st

     , HP.MonadTeXParse (HP.TeXParseT s m)

     , MonadError e m
     , HP.AsTeXParseErrors e
     , AsType HP.ParseError e

     , MonadSlog m
     )
  => (s -> s -> a -> HP.Command -> m (s, a, Maybe b))
  -> s
  -> a
  -> m (s, a, b)
runCommandLoop runCommand = runLoop parseAndRunCommand
  where
    parseAndRunCommand oldS elemList = do
      (newS, command) <- HP.runTeXParseTEmbedded HP.parseCommand oldS
      sLogStampedJSON "Parsed command"
        [ ("command", toJSON command)
        ]
      runCommand oldS newS elemList command
