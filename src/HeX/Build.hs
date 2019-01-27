{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HeX.Build where

import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( MonadState
                                                , gets
                                                , liftIO
                                                , modify
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , liftEither
                                                , MonadError
                                                , throwError
                                                , withExceptT
                                                )
import qualified Data.HashMap.Strict           as HMap
import           Data.Either.Combinators        ( mapLeft )
import           Path
import           Safe                           ( headMay
                                                , toEnumMay )
import qualified Text.Megaparsec               as PS

import           Data.Adjacent                  ( Adj(..) )
import           Data.Path                      ( findFilePath )

import qualified TFM
import           TFM                            ( TexFont(..) )
import qualified TFM.Character                 as TFMC

import           HeX.Concept
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.BreakList.Line             ( setParagraph )
import           HeX.BreakList.Page             ( PageBreakJudgment(..)
                                                , pageBreakJudgment
                                                , setPage
                                                )
import           HeX.Config
import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex
import           HeX.Evaluate
import qualified HeX.Parse                     as HP
import qualified HeX.Unit                      as Unit

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e Nothing  = throwError e
liftMaybe _ (Just a) = pure a

currentFontInfo :: (MonadState Config m, MonadError String m) => m TexFont
currentFontInfo =
    do
    -- Maybe font number isn't set.
    fNr <- gets currentFontNr >>= liftMaybe "Font number isn't set"
    -- Or maybe there's no font where there should be.
    -- TODO: I think I can make this case impossible, maybe by storing the
    -- current font info directly instead of a lookup.
    gets fontInfoMap >>= (liftMaybe "No such font number" . (HMap.lookup fNr))

defineFont
    :: (MonadState Config m, MonadIO m)
    => Lex.ControlSequenceLike
    -> Path Rel File
    -> m B.FontDefinition
defineFont _ fPath =
    do
    theFontDirectories <- gets fontDirectories
    maybeFontDef <- liftIO $ runMaybeT $ ioFontDef theFontDirectories
    case maybeFontDef of
        Just fontDef@B.FontDefinition {fontInfo = font} ->
            do
            modify
              (\conf -> conf {fontInfoMap = HMap.insert fNr font $ fontInfoMap conf})
            pure fontDef
        Nothing ->
            fail "Could not define font"
  where
    -- TODO: Look up font number from control sequence.
    fNr = HP.theFontNr
    ioFontDef fontDirs =
        do
        fontPath <- findFilePath fPath fontDirs
        font <- liftIO $ TFM.readTFMFancy fontPath
        nonExtName <- Path.setFileExtension "" fPath
        let fontName = Path.toFilePath $ Path.filename nonExtName
        pure B.FontDefinition
            { fontNr           = fNr
            -- TODO: Improve mapping of name and path.
            , fontPath         = fontName
            , fontName         = fontName
            , fontInfo         = font
            , scaleFactorRatio = 1.0
            }

selectFont :: MonadState Config m => Int -> m B.FontSelection
selectFont n =
    do
    modify (\conf -> conf {currentFontNr = Just n})
    pure B.FontSelection {fontNr = n}

characterBox :: (MonadState Config m, MonadError String m) => CharCode -> m B.Character
characterBox char =
    do
    font <- currentFontInfo
    let toSP = TFM.designScaleSP font
    TFMC.Character {width = w, height = h, depth = d} <-
        liftMaybe "No such character" $ (HMap.lookup char $ characters font)
    pure
        B.Character {char = char, width = toSP w, height = toSP h, depth = toSP d}

spaceGlue :: (MonadState Config m, MonadError String m) => m BL.Glue
spaceGlue =
    do
    font@TexFont {spacing = d, spaceStretch = str, spaceShrink = shr} <- currentFontInfo
    let toSP = TFM.designScaleSP font
        toFlex = BL.finiteFlex . fromIntegral . toSP
    pure BL.Glue {dimen = toSP d, stretch = toFlex str, shrink = toFlex shr}

defineMacro :: HP.ExpandedStream -> HP.MacroAssignment -> HP.ExpandedStream
defineMacro es@HP.ExpandedStream{csMap = csMap} (HP.MacroAssignment name macro False False) =
    es{HP.csMap = csMap'}
  where
    newMacro = HP.SyntaxCommandHead $ HP.MacroTok macro
    csMap' = HMap.insert name newMacro csMap
defineMacro _ _
  = error "Not implemented: long and outer macros"

doAssignment
    :: (MonadState Config m, MonadIO m)
    => HP.VariableAssignment
    -> ExceptT String m ()
doAssignment = \case
    HP.IntegerVariableAssignment v n ->
        do
        let en = evaluateNumber n
        case v of
            (HP.ParamVar p)    -> setConfIntParam p en
            (HP.RegisterVar _) -> error "int registers not implemented"
    HP.LengthVariableAssignment v d ->
        do
        _mag <- gets (mag . params)
        let ed = evaluateLength _mag d
        case v of
            (HP.ParamVar p)    -> setConfLenParam p ed
            (HP.RegisterVar _) -> error "length registers not implemented"
    HP.GlueVariableAssignment v g ->
        do
        _mag <- gets (mag . params)
        let eg = evaluateGlue _mag g
        case v of
            (HP.ParamVar p)    -> setConfGlueParam p eg
            (HP.RegisterVar _) -> error "glue registers not implemented"
    HP.MathGlueVariableAssignment v g ->
        error "math-glue assignment not implemented"
    HP.TokenListVariableAssignmentVar v g ->
        error "token-list-to-variable assignment not implemented"
    HP.TokenListVariableAssignmentText v g ->
        error "token-list-to-text assignment not implemented"

handleModeIndep
    :: (MonadState Config m, MonadIO m)
    => HP.ExpandedStream
    -> HP.ModeIndependentCommand
    -> ExceptT String m ([BL.BreakableVListElem], HP.ExpandedStream)
handleModeIndep newStream com =
    let
        modAccum acc = pure (acc, newStream)
        modStream mStream = pure ([], mStream)
        continueUnchanged = pure ([], newStream)
    in  case com of
        HP.Relax ->
            continueUnchanged
        HP.IgnoreSpaces ->
            continueUnchanged
        HP.AddPenalty n ->
            modAccum [BL.ListPenalty $ evaluatePenalty n]
        HP.AddKern ln ->
            do
            _mag <- gets (mag . params)
            modAccum [BL.VListBaseElem $ B.ElemKern $ evaluateKern _mag ln]
        HP.AddGlue g ->
            do
            _mag <- gets (mag . params)
            modAccum [BL.ListGlue (evaluateGlue _mag g)]
        HP.Assign HP.Assignment{global = _, body = _body} ->
            case _body of
                HP.SetVariable ass ->
                    do
                    doAssignment ass
                    continueUnchanged
                HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                    let eIdx = evaluateNumber idx
                        eVal = evaluateNumber val
                    in  case codeType of
                            HP.CategoryCode ->
                                do
                                idxChar <- liftMaybe ("Invalid character code index: " ++ show eIdx) (toEnumMay eIdx)
                                valCat <- liftMaybe ("Invalid category code value: " ++ show eVal) (toEnumMay eVal)
                                modStream newStream{HP.ccMap=HMap.insert idxChar valCat $ HP.ccMap newStream}
                            _ ->
                                error $ "Code type not implemented: " ++ show codeType

                HP.SelectFont fNr ->
                    do
                    fontSel <- BL.VListBaseElem . B.ElemFontSelection <$> selectFont fNr
                    modAccum [fontSel]
                HP.DefineMacro m ->
                    modStream $ defineMacro newStream m
                HP.DefineFont cs HP.NaturalFont fPath ->
                    do
                    fontDef <- BL.VListBaseElem . B.ElemFontDefinition <$> defineFont cs fPath
                    modAccum [fontDef]
processHCommand
    :: (MonadState Config m, MonadIO m)
    => HP.ExpandedStream
    -> HP.ExpandedStream
    -> [BL.BreakableHListElem]
    -> HP.HModeCommand
    -> ExceptT String m ([BL.BreakableHListElem], HP.ExpandedStream, Bool)
processHCommand oldStream newStream acc com =
    let
        modAccum newAcc = pure (newAcc, newStream, True)
        modStream mStream = pure (acc, mStream, True)
        continueUnchanged = pure (acc, newStream, True)
    in  case com of
        HP.LeaveHMode ->
            do
            -- Inner mode: forbidden. TODO.
            -- Outer mode: insert the rol sequence "\par" into the input. The control
            -- sequence's current meaning will be used, which might no longer be the \par
            -- primitive.
            -- (Note that we pass oldStream, not newStream.)
            let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
            modStream $ HP.insertLexToken oldStream parToken
        HP.AddCharacter c ->
            do
            let charCode = evaluateCharCodeRef c
            hCharBox <- BL.HListHBaseElem . B.ElemCharacter <$> characterBox charCode
            modAccum $ hCharBox : acc
        HP.HAllModesCommand aCom ->
            case aCom of
                -- \indent: An empty box of width \parindent is appended to the current
                -- list, and the space factor is set to 1000.
                -- TODO: Space factor.
                HP.StartParagraph HP.DoNotIndent ->
                    continueUnchanged
                HP.StartParagraph HP.Indent ->
                    do
                    indentBox <- gets parIndentBox
                    modAccum (indentBox : acc)
                -- \par: end the current paragraph.
                HP.EndParagraph ->
                    pure (acc, newStream, False)
                HP.AddSpace ->
                    do
                    hGlue <- (BL.HVListElem . BL.ListGlue) <$> spaceGlue
                    modAccum $ hGlue : acc
                HP.AddRule HP.Rule{..} ->
                    do
                    _mag <- gets (mag . params)
                    let evalW = case width of
                            Nothing -> Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                            Just ln -> evaluateLength _mag ln
                        evalH = case height of
                            Nothing -> Unit.toScaledPointApprox (10 :: Int) Unit.Point
                            Just ln -> evaluateLength _mag ln
                        evalD = case depth of
                            Nothing -> 0
                            Just ln -> evaluateLength _mag ln
                        rule = B.Rule { width = evalW, height = evalH, depth = evalD }
                    modAccum $ (BL.HVListElem $ BL.VListBaseElem $ B.ElemRule rule) : acc
                HP.ModeIndependentCommand mcom ->
                    do
                    (extraAcc, mStream) <- handleModeIndep newStream mcom
                    pure ((BL.HVListElem <$> extraAcc) ++ acc, mStream, True)

data BuildError
  = ParseError (HP.ParseErrorBundle HP.ExpandedStream)
  | ConfigError String
  deriving (Show)

extractParagraph
    :: (MonadState Config m, MonadIO m)
    => HP.IndentFlag
    -> HP.ExpandedStream
    -> ExceptT BuildError m ([BL.BreakableHListElem], HP.ExpandedStream)
extractParagraph indentFlag stream =
    do
    indentBox <- gets parIndentBox
    extractParagraphInner [indentBox | indentFlag == HP.Indent] stream
  where
    -- We build a paragraph list in reverse order.
    extractParagraphInner
        :: (MonadState Config m, MonadIO m)
        => [BL.BreakableHListElem]
        -> HP.ExpandedStream
        -> ExceptT BuildError m ([BL.BreakableHListElem], HP.ExpandedStream)
    extractParagraphInner acc oldStream =
        do
        (PS.State {stateInput = newStream}, com) <-
            liftEither $ ParseError `mapLeft` HP.extractHModeCommand oldStream
        (procAcc, procStream, continue) <-
            withExceptT ConfigError $ processHCommand oldStream newStream acc com
        if continue
            then extractParagraphInner procAcc procStream
            else pure (procAcc, procStream)

extractParagraphLineBoxes
    :: (MonadState Config m, MonadIO m)
    => HP.IndentFlag
    -> HP.ExpandedStream
    -> ExceptT BuildError m ([[B.HBoxElem]], HP.ExpandedStream)
extractParagraphLineBoxes indentFlag stream =
    do
    (hList, stream') <- extractParagraph indentFlag stream
    desiredW <- gets (hSize . params)
    lineTol <- gets (tolerance . params)
    linePen <- gets (linePenalty . params)
    pure (setParagraph desiredW lineTol linePen hList, stream')

data CurrentPage = CurrentPage
    { items :: [BL.BreakableVListElem]
    , bestPointAndCost :: Maybe (Int, Int)
    }

runPageBuilder
    :: MonadState Config m
    => CurrentPage
    -> [BL.BreakableVListElem]
    -> m [B.Page]
runPageBuilder (CurrentPage cur _) [] =
    do
    desiredH <- gets (vSize . params)
    pure [setPage desiredH $ reverse cur]
runPageBuilder (CurrentPage cur _bestPointAndCost) (x:xs)
    -- If the current vlist has no boxes, we discard a discardable item.
    | not $ any BL.isBox cur =
        let nextXs = if BL.isDiscardable x then cur else x:cur
        in runPageBuilder (CurrentPage nextXs _bestPointAndCost) xs
    -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
    -- the cost c of breaking at this point.
    | BL.isDiscardable x =
        do
        desiredH <- gets (vSize . params)
        case BL.toBreakItem Adj{pre = headMay cur, val = x, post = headMay xs} of
            -- If we can't break here, just add it to the list and continue.
            Nothing ->
                usualContinue
            Just brk ->
                case (pageBreakJudgment cur brk desiredH, _bestPointAndCost) of
                    (DoNotBreak, _) ->
                        usualContinue
                    -- I don't think this condition will ever be satisfied, but if we
                    -- decide to break before any valid break-point has been considered,
                    -- just carry on.
                    (BreakPageAtBest, Nothing) ->
                        usualContinue
                    -- If c = ∞, we break at the best breakpoint so far.
                    -- The current vlist material following that best breakpoint is
                    -- returned to the recent contributions, to consider again.
                    (BreakPageAtBest, Just (iBest, _)) ->
                        -- the `reverse` will put both of these into reading order.
                        let (curNewPage, toReturn) = splitAt iBest $ reverse cur
                            newPage = setPage desiredH curNewPage
                        -- xs is also in reading order
                        -- We didn't actually split at x: x was just what made us compute
                        -- cost and notice we'd gone too far. So add it to the left-overs
                        -- to return.
                        in  (newPage :) <$> runPageBuilder newCurrentPage (toReturn ++ (x:xs))
                    -- If p ≤ −10000, we know the best breakpoint is this one, so break
                    -- here.
                    (BreakPageHere, _) ->
                        -- the `reverse` will put this into reading order.
                        let newPage = setPage desiredH $ reverse cur
                        in  (newPage :) <$> runPageBuilder newCurrentPage xs
                    -- If the resulting cost <= the smallest cost seen so far, remember
                    -- the current breakpoint as the best so far.
                    (TrackCost cHere, _) ->
                        let thisPointAndCost = Just (length cur, cHere)
                            newBestPointAndCost = case _bestPointAndCost of
                                Nothing ->
                                    thisPointAndCost
                                Just (_, cBest) ->
                                    if cHere > cBest then _bestPointAndCost else thisPointAndCost
                        in  runPageBuilder (CurrentPage (x:cur) newBestPointAndCost) xs
    -- If we can't break here, just add it to the list and continue.
    | otherwise = usualContinue
  where
    usualNextPage = CurrentPage (x:cur) _bestPointAndCost
    usualContinue = runPageBuilder usualNextPage xs

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
    => [BL.BreakableVListElem]
    -> BL.BreakableVListElem
    -> m [BL.BreakableVListElem]
addVListElem acc e = case e of
    (BL.VListBaseElem (B.ElemBox b)) -> addVListBox b
    _                                -> pure $ e : acc
  where
    addVListBox :: MonadState Config m => B.Box -> m [BL.BreakableVListElem]
    addVListBox b =
        do
        _prevDepth <- gets (unLenParam . prevDepth . params)
        BL.Glue blineLength blineStretch blineShrink <- gets (unGlueParam . baselineSkip . params)
        skipLimit <- gets (unLenParam . lineSkipLimit . params)
        skip <- gets (unGlueParam . lineSkip . params)
        setConfSpecialLen HP.PrevDepth $ naturalDepth e
        pure $ if _prevDepth <= -Unit.oneKPt
            then e : acc
            else
                let proposedBaselineLength = blineLength - _prevDepth - naturalHeight b
                -- Intuition: set the distance between baselines to \baselineskip, but no
                -- closer than \lineskiplimit [theBaselineLengthMin], in which case
                -- \lineskip [theMinBaselineGlue] is used.
                    glue = BL.ListGlue $ if proposedBaselineLength >= skipLimit
                        then BL.Glue proposedBaselineLength blineStretch blineShrink
                        else skip
                in  e : glue : acc

addVListElems
    :: MonadState Config m
    => [BL.BreakableVListElem]
    -> [BL.BreakableVListElem]
    -> m [BL.BreakableVListElem]
addVListElems = foldM addVListElem

processVCommand
    :: (MonadState Config m, MonadIO m)
    => HP.ExpandedStream
    -> HP.ExpandedStream
    -> [B.Page]
    -> CurrentPage
    -> [BL.BreakableVListElem]
    -> HP.VModeCommand
    -> ExceptT BuildError m ([B.Page], CurrentPage, [BL.BreakableVListElem], HP.ExpandedStream, Bool)
processVCommand oldStream newStream pages curPage acc com =
    let continueSamePage newAcc mStream  = pure (pages, curPage, newAcc, mStream, True)
        modAccum newAcc = continueSamePage newAcc newStream
        continueUnchanged = continueSamePage acc newStream

        addParagraphToPage indentFlag =
            do
            -- If the command shifts to horizontal mode, run '\indent', and re-read the
            -- stream as if the commands just seen hadn't been read.
            -- (Note that we pass "oldStream", not "newStream".)
            -- Paraboxes are returned in reading order.
            (lineBoxes, mStream) <- extractParagraphLineBoxes indentFlag oldStream
            desiredW <- gets (unLenParam . hSize . params)
            let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
            newAcc <- addVListElems acc $ BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes
            continueSamePage newAcc mStream
    in  case com of
            -- End recursion.
            HP.End ->
                do
                lastPages <- runPageBuilder curPage (reverse acc)
                let pagesFinal = pages ++ lastPages
                pure (pagesFinal, curPage, acc, newStream, False)
            HP.EnterHMode ->
                addParagraphToPage HP.Indent
            HP.VAllModesCommand aCom ->
                case aCom of
                    HP.StartParagraph indentFlag ->
                        addParagraphToPage indentFlag
                    -- \par does nothing in vertical mode.
                    HP.EndParagraph ->
                        continueUnchanged
                    -- <space token> has no effect in vertical modes.
                    HP.AddSpace ->
                        continueUnchanged
                    HP.AddRule HP.Rule{..} ->
                        do
                        _mag <- gets (mag . params)
                        evalW <- case width of
                            Nothing -> gets (unLenParam . hSize . params)
                            Just ln -> pure $ evaluateLength _mag ln
                        let evalH = case height of
                                -- TODO.
                                Nothing -> Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                                Just ln -> evaluateLength _mag ln
                            evalD = case depth of
                                Nothing -> 0
                                Just ln -> evaluateLength _mag ln
                            rule = B.Rule{width = evalW, height = evalH, depth = evalD}
                        modAccum $ (BL.VListBaseElem $ B.ElemRule rule) : acc
                    HP.ModeIndependentCommand mcom ->
                        do
                        (extraAcc, mStream) <- withExceptT ConfigError $ handleModeIndep newStream mcom
                        continueSamePage (extraAcc ++ acc) mStream

newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage [] Nothing

extractPages
    :: (MonadState Config m, MonadIO m)
    => HP.ExpandedStream
    -> ExceptT BuildError m ([B.Page], HP.ExpandedStream)
extractPages = extractPagesInner [] newCurrentPage []
  where
    extractPagesInner
        :: (MonadState Config m, MonadIO m)
        => [B.Page]
        -> CurrentPage
        -> [BL.BreakableVListElem]
        -> HP.ExpandedStream
        -> ExceptT BuildError m ([B.Page], HP.ExpandedStream)
    extractPagesInner pages curPage acc oldStream =
        do
        (PS.State {stateInput = newStream}, com) <-
            liftEither $ ParseError `mapLeft` HP.extractVModeCommand oldStream
        (procPages, procCurPage, procAcc, procStream, continue) <-
            processVCommand oldStream newStream pages curPage acc com
        if continue
            then extractPagesInner procPages procCurPage procAcc procStream
            else pure (procPages, procStream)
