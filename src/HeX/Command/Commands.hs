module HeX.Command.Commands where

import           HeXlude

import qualified Data.HashMap.Strict           as HMap
import qualified Data.Text                     as Text
import           Path                           ( Path
                                                , Rel
                                                , File
                                                )
import qualified Path

import           Data.Path                      ( findFilePath )
import qualified TFM
import           TFM                            ( TexFont(..) )

import qualified HeX.Box          as B
import           HeX.BreakList                  ( HListElem, VListElem )
import qualified HeX.BreakList    as BL
import           HeX.Categorise                 ( CharCode )
import           HeX.Command.Common
import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Lex                       as Lex
import qualified HeX.Parse        as HP
import qualified HeX.Unit         as Unit

glueToElem :: HP.InhibitableStream s => HP.Glue -> ExceptMonadBuild s BL.VListElem
glueToElem g =
    do
    eG <- liftEvalOnConfState g
    pure $ BL.ListGlue eG

ruleToElem
    :: HP.InhibitableStream s
    => HP.Rule
    -> ReaderT Config (StateT Config (ExceptT Text (MonadBuild s))) LenVal
    -> ReaderT Config (StateT Config (ExceptT Text (MonadBuild s))) LenVal
    -> ReaderT Config (StateT Config (ExceptT Text (MonadBuild s))) LenVal
    -> ExceptMonadBuild s BL.VListElem
ruleToElem HP.Rule { HP.width, HP.height, HP.depth } defaultW defaultH defaultD =
    do
    rule <- liftReadOnConfState $ B.Rule
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
    => BackwardDirected Seq BL.VListElem
    -> BL.VListElem
    -> m (BackwardDirected Seq BL.VListElem)
addVListElem acc e = case e of
    (BL.VListBaseElem (B.ElemBox b)) -> addVListBox b
    _                                -> pure (e <<| acc)
  where
    addVListBox :: MonadState Config m => B.Box -> m (BackwardDirected Seq BL.VListElem)
    addVListBox b =
        do
        _prevDepth <- gets $ lookupSpecialLength HP.PrevDepth
        BL.Glue blineLength blineStretch blineShrink <- gets $ lookupGlueParameter HP.BaselineSkip
        skipLimit <- gets $ lookupLengthParameter HP.LineSkipLimit
        skip <- gets $ lookupGlueParameter HP.LineSkip
        modify $ setSpecialLength HP.PrevDepth $ naturalDepth e
        pure $ if _prevDepth <= -Unit.oneKPt
            then
                e <<| acc
            else
                let proposedBaselineLength = blineLength - _prevDepth - naturalHeight b
                -- Intuition: set the distance between baselines to \baselineskip, but no
                -- closer than \lineskiplimit [theBaselineLengthMin], in which case
                -- \lineskip [theMinBaselineGlue] is used.
                    glue = BL.ListGlue $ if proposedBaselineLength >= skipLimit
                        then BL.Glue proposedBaselineLength blineStretch blineShrink
                        else skip
                in e <<| glue <<| acc

hModeAddHGlue :: HP.InhibitableStream s => HP.Glue -> ExceptMonadBuild s HListElem
hModeAddHGlue g =
    BL.HVListElem <$> glueToElem g

hModeAddCharacter :: HP.InhibitableStream s => HP.CharCodeRef -> ExceptMonadBuild s HListElem
hModeAddCharacter c =
    liftConfigError $
        do
        charCode <- readOnConfState $ texEvaluate c
        (BL.HListHBaseElem . B.ElemCharacter) <$> (readOnConfState $ characterBox charCode)

hModeAddSpace :: HP.InhibitableStream s => ExceptMonadBuild s HListElem
hModeAddSpace =
    liftConfigError $ (BL.HVListElem . BL.ListGlue) <$> readOnConfState spaceGlue

hModeAddRule :: HP.InhibitableStream s => HP.Rule -> ExceptMonadBuild s HListElem
hModeAddRule rule =
    do
    let
        defaultWidth = pure (Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point)
        defaultHeight = pure (Unit.toScaledPointApprox (10 :: Int) Unit.Point)
        defaultDepth = pure 0
    ruleElem <- ruleToElem rule defaultWidth defaultHeight defaultDepth
    pure $ BL.HVListElem ruleElem

hModeStartParagraph :: HP.InhibitableStream s => HP.IndentFlag -> ExceptMonadBuild s (Maybe HListElem)
hModeStartParagraph = \case
    HP.DoNotIndent ->
        pure Nothing
    -- \indent: An empty box of width \parindent is appended to the current
    -- list, and the space factor is set to 1000.
    -- TODO: Space factor.
    HP.Indent ->
        Just <$> (liftConfigError $ readOnConfState $ asks parIndentBox)

vModeAddVGlue :: HP.InhibitableStream s => HP.Glue -> ExceptMonadBuild s VListElem
vModeAddVGlue g =
    glueToElem g

vModeAddRule :: HP.InhibitableStream s => HP.Rule -> ExceptMonadBuild s VListElem
vModeAddRule rule =
    do
    let
        defaultWidth = gets $ lookupLengthParameter HP.HSize
        defaultHeight = pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
        defaultDepth = pure 0
    ruleToElem rule defaultWidth defaultHeight defaultDepth

-- Horizontal mode commands.

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

selectFont :: MonadState Config m => Int -> HP.GlobalFlag -> m ()
selectFont n globalFlag = modify $ selectFontNr n globalFlag

getFileStream :: HMap.HashMap FourBitInt Handle -> IntVal -> Maybe Handle
getFileStream strms n = do
    fourBitn <- newFourBitInt n
    HMap.lookup fourBitn strms

-- Showing things.

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
