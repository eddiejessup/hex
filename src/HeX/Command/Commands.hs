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
import           Hex.Command.Common
import           Hex.Config
import           Hex.Evaluate
import qualified Hex.Lex             as Lex
import qualified Hex.Parse           as HP
import           Hex.Quantity

glueToElem
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       , MonadState s m
       )
    => HP.Glue
    -> m BL.VListElem
glueToElem g =
    BL.ListGlue <$> evalOnConfState g

ruleToElem
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
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
    :: MonadState Config m
    => BL.VList
    -> BL.VListElem
    -> m BL.VList
addVListElem (BL.VList accSeq) = \case
    e@(BL.VListBaseElem (B.ElemBox b)) ->
        do
        _prevDepth <- gets $ lookupSpecialLength HP.PrevDepth
        BL.Glue blineLength blineStretch blineShrink <- gets $ lookupGlueParameter HP.BaselineSkip
        skipLimit <- gets $ lookupLengthParameter HP.LineSkipLimit
        skip <- gets $ lookupGlueParameter HP.LineSkip
        modify $ setSpecialLength HP.PrevDepth $ naturalDepth e
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
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       , MonadState s m
       )
    => HP.Glue
    -> m HListElem
hModeAddHGlue g =
    BL.HVListElem <$> glueToElem g

hModeAddCharacter
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       , MonadState s m
       )
    => HP.CharCodeRef
    -> m HListElem
hModeAddCharacter c =
    readOnConfState $
        texEvaluate c
        >>= characterBox
        <&> B.ElemCharacter
        <&> BL.HListHBaseElem

hModeAddSpace
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m '[ConfigError]
       , MonadState s m
       )
    => m HListElem
hModeAddSpace =
    BL.HVListElem . BL.ListGlue <$> readOnConfState spaceGlue

hModeAddRule
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       , MonadState s m
       )
    => HP.Rule
    -> m HListElem
hModeAddRule rule =
    BL.HVListElem <$> readOnConfState (ruleToElem rule defaultWidth defaultHeight defaultDepth)
  where
    defaultWidth = pure (toScaledPointApprox (0.4 :: Rational) Point)
    defaultHeight = pure (toScaledPointApprox (10 :: Int) Point)
    defaultDepth = pure 0

hModeStartParagraph
    :: ( HP.TeXStream s
       , MonadErrorVariant e m
       , MonadState s m
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
        Just <$> readOnConfState (asks parIndentBox)

vModeAddVGlue
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       , MonadState s m
       )
    => HP.Glue
    -> m VListElem
vModeAddVGlue = glueToElem

vModeAddRule
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
       , MonadState s m
       )
    => HP.Rule
    -> m VListElem
vModeAddRule rule =
    readOnConfState $ ruleToElem rule defaultWidth defaultHeight defaultDepth
  where
    defaultWidth = gets $ lookupLengthParameter HP.HSize
    defaultHeight = pure $ toScaledPointApprox (0.4 :: Rational) Point
    defaultDepth = pure 0

-- Horizontal mode commands.

characterBox
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[ConfigError]
       )
    => CharCode
    -> m B.Character
characterBox char = do
    fontMetrics <- currentFontMetrics
    let toSP = TFM.designScaleSP fontMetrics
    TFM.Character { TFM.width, TFM.height, TFM.depth } <-
        note (throw $ ConfigError "No such character")
        $ IntMap.lookup (fromIntegral $ Code.codeWord char) (characters fontMetrics)
    pure B.Character { B.char       = char
                     , B.charWidth  = toSP width
                     , B.charHeight = toSP height
                     , B.charDepth  = toSP depth
                     }

spaceGlue
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[ConfigError]
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
    :: ( MonadState Config m
       , MonadIO m
       , MonadErrorAnyOf e m
           '[ D.Path.PathError
            , ConfigError
            , TFM.TFMError
            , EvaluationError
            ]
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

selectFont :: MonadState Config m => TeXInt -> HP.GlobalFlag -> m ()
selectFont n globalFlag = modify $ selectFontNr n globalFlag

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
    HP.UnexpandedTok t -> showLexTok t
    pt                 -> Seq.fromList $ unsafeCodesFromChars (show pt)

showBalancedText :: HP.BalancedText -> Seq CharCode
showBalancedText (HP.BalancedText lexToks) =
    Seq.fromList $ concat $ toList . showLexTok <$> lexToks

showExpandedBalancedText :: HP.ExpandedBalancedText -> [CharCode]
showExpandedBalancedText (HP.ExpandedBalancedText primToks) =
    concat $ toList . showPrimTok <$> primToks
