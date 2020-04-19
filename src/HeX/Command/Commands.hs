module Hex.Command.Commands where

import           Hexlude

import qualified Data.Map.Strict     as Map
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.Path           as D.Path
import qualified Data.Sequence       as Seq

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

       , MonadReader st m
       , HasType Config st
       )
    => HP.Glue
    -> m BL.VListElem
glueToElem g =
    BL.ListGlue <$> texEvaluate g

ruleToElem
    :: ( MonadReader st m
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


hModeAddHGlue
    :: ( MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e

       , MonadReader st m
       , HasType Config st
       )
    => HP.Glue
    -> m HListElem
hModeAddHGlue g =
    BL.HVListElem <$> glueToElem g

hModeAddCharacter
    :: ( MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e

       , MonadReader st m
       , HasType Config st
       )
    => HP.CharCodeRef
    -> m HListElem
hModeAddCharacter c =
    texEvaluate c
        >>= characterBox
        <&> B.ElemCharacter
        <&> BL.HListHBaseElem

hModeAddSpace
    :: ( MonadError e m
       , AsType ConfigError e

       , MonadReader st m
       , HasType Config st
       )
    => m HListElem
hModeAddSpace =
    BL.HVListElem . BL.ListGlue <$> spaceGlue

hModeAddRule
    :: ( MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e

       , MonadReader st m
       , HasType Config st
       )
    => HP.Rule
    -> m HListElem
hModeAddRule rule =
    BL.HVListElem <$> ruleToElem rule defaultWidth defaultHeight defaultDepth
  where
    defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
    defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
    defaultDepth = pure 0

hModeStartParagraph
    :: ( MonadError e m
       , MonadReader st m
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
        Just <$> (asks $ view $ typed @Config . to parIndentBox)

vModeAddVGlue
    :: ( MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e
       , MonadReader st m
       , HasType Config st
       )
    => HP.Glue
    -> m VListElem
vModeAddVGlue = glueToElem

vModeAddRule
    :: ( MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e
       , MonadReader st m
       , HasType Config st
       )
    => HP.Rule
    -> m VListElem
vModeAddRule rule =
    ruleToElem rule defaultWidth defaultHeight defaultDepth
  where
    defaultWidth = asks $ view $ typed @Config . to (lookupLengthParameter HP.HSize)
    defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
    defaultDepth = pure 0

-- Horizontal mode commands.

characterBox
    :: ( MonadReader st m
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
        $ IntMap.lookup (fromIntegral $ Code.codeWord char) (characters fontMetrics)
    pure B.Character { B.char       = char
                     , B.charWidth  = toSP width
                     , B.charHeight = toSP height
                     , B.charDepth  = toSP depth
                     }

spaceGlue
    :: ( MonadReader st m
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
    info@FontInfo{ fontMetrics } <- readOnState $ findFilePath (WithImplicitExtension "tfm") [] path >>= readFontInfo
    let designSizeSP = TFM.designSizeSP fontMetrics
    scaleRatio <- readOnState $ evaluateFontSpecification designSizeSP fontSpec
    -- liftIO
    --     $  putText
    --     $  "Loading font: "
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

selectFont :: (MonadState st m, HasType Config st) => TeXInt -> HP.GlobalFlag -> m ()
selectFont n globalFlag = modify $ typed @Config %~ selectFontNr n globalFlag

getFileStream :: Map.Map FourBitInt Handle -> TeXInt -> Maybe Handle
getFileStream strms (TeXInt n) = do
    fourBitN <- newFourBitInt n
    Map.lookup fourBitN strms

-- Showing things.

showLexTok :: Lex.Token -> Seq CharCode
showLexTok = \case
    Lex.CharCatToken Lex.CharCat { Lex.char } ->
        singleton char
    Lex.ControlSequenceToken Lex.ControlSequence { Lex.csChars } ->
        singleton (Code.CharCode_ '\\') <> csChars

showPrimTok :: HP.PrimitiveToken -> Seq CharCode
showPrimTok = \case
    HP.UnresolvedTok t -> showLexTok t
    pt                 -> Seq.fromList $ unsafeCodesFromChars (show pt)

showBalancedText :: HP.BalancedText -> Seq CharCode
showBalancedText (HP.BalancedText lexToks) =
    Seq.fromList $ concat $ toList . showLexTok <$> lexToks

showExpandedBalancedText :: HP.ExpandedBalancedText -> [CharCode]
showExpandedBalancedText (HP.ExpandedBalancedText primToks) =
    concat $ toList . showPrimTok <$> primToks
