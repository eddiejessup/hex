module HeX.Build where

import HeXlude

import           Control.Monad                  ( foldM
                                                , when )
import           Control.Monad.Except           ( liftEither )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Either.Combinators        ( mapLeft )
import           Safe                           ( toEnumMay )
import qualified Text.Megaparsec               as PS

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Config
import qualified HeX.Lex                       as Lex
import           HeX.Evaluate
import qualified HeX.Parse                     as HP
import qualified HeX.Unit                      as Unit
import           HeX.Command                    ( ExceptBuildT
                                                , liftReadOnConfState
                                                , readOnConfState
                                                , modConfState
                                                , throwConfigError
                                                , liftConfState
                                                , readOnState
                                                , liftMaybeConfigError
                                                , liftConfigError
                                                , BuildError(..) )
import qualified HeX.Command                   as Com

constructBox
    :: (HP.InhibitableStream s, MonadState s m, MonadIO m)
    => HP.Box -> ExceptBuildT s m (Maybe B.Box)
constructBox = \case
    HP.ExplicitBox spec boxType ->
        do
        eSpec <- liftReadOnConfState $ evaluateBoxSpecification spec
        modConfState $ pushGroup ExplicitBoxGroup
        b <- case boxType of
            HP.ExplicitHBox ->
                (B.HBoxContents . BL.setHList BL.NaturallyGood . reverse) <$> extractHList HP.DoNotIndent True
            HP.ExplicitVBox alignType ->
                ((\els -> B.VBoxContents els alignType) . BL.setVList BL.NaturallyGood . reverse) <$> extractVList True
        pure $ Just $ B.Box b eSpec
    HP.FetchedRegisterBox fetchMode idx ->
        do
        eIdx <- liftReadOnConfState $ evaluateEightBitInt idx
        box <- readOnConfState $ asks $ lookupBoxRegister eIdx
        case fetchMode of
            HP.Lookup -> pure ()
            HP.Pop -> HP.runConfState $ modify $ delBoxRegister eIdx HP.Local
        pure box

handleModeIndep
    :: (HP.InhibitableStream s, MonadState s m, MonadIO m)
    => HP.ModeIndependentCommand -> ExceptBuildT s m (Maybe [BL.BreakableVListElem])
handleModeIndep = \case
    HP.ChangeScope (HP.Sign True) trig ->
        do
        modConfState $ pushGroup $ LocalStructureGroup trig
        noElems
    HP.ChangeScope (HP.Sign False) trig ->
        HP.runConfState $
            gets popGroup >>= \case
                Nothing -> throwConfigError "No group to leave"
                Just (grp, conf') ->
                    do
                    put conf'
                    case grp of
                        LocalStructureGroup trigConf ->
                            do
                            when (trigConf /= trig) $ throwConfigError $ "Entry and exit group triggers differ: " <> showT (trig, trigConf)
                            retElems []
                        ExplicitBoxGroup ->
                            pure Nothing
    HP.Message HP.Out eTxt ->
        do
        liftIO $ putStrLn $ Com.showExpandedBalancedText eTxt
        noElems
    HP.Relax ->
        noElems
    HP.IgnoreSpaces ->
        noElems
    -- Re-insert the ⟨token⟩ into the input just after running the next
    -- assignment command. Later \afterassignment commands override earlier
    -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
    -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
    -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
    -- inserted by \everyhbox or \everyvbox.
    HP.SetAfterAssignmentToken lt ->
        do
        modConfState $ \conf -> conf{afterAssignmentToken = Just lt}
        noElems
    HP.AddPenalty n ->
        do
        p <- liftReadOnConfState $ evaluatePenalty n
        retElems [BL.ListPenalty p]
    HP.AddKern ln ->
        do
        k <- liftReadOnConfState $ evaluateKern ln
        retElems [BL.VListBaseElem $ B.ElemKern k]
    HP.AddGlue g ->
        do
        eG <- liftReadOnConfState $ evaluateGlue g
        retElems [BL.ListGlue eG]
    HP.Assign HP.Assignment { HP.global, HP.body } ->
        do
        assignElems <- case body of
            HP.DefineControlSequence cs tgt ->
                do
                (acc, newCSTok) <- liftReadOnConfState $ case tgt of
                    HP.MacroTarget macro ->
                        pure ([], HP.syntaxTok $ HP.MacroTok macro)
                    -- TODO: If a \let target is an active character, should we
                    -- treat it as a control sequence, or a char-cat pair?
                    HP.LetTarget (Lex.CharCatToken tgtCC) ->
                        pure ([], HP.primTok $ HP.LetCharCat tgtCC)
                    HP.LetTarget (Lex.ControlSequenceToken tgtCS) ->
                        do
                        resTok <- fromMaybe (HP.PrimitiveToken HP.RelaxTok) <$> asks (lookupCSProper tgtCS)
                        pure ([], resTok)
                    HP.ShortDefineTarget q n ->
                        do
                        en <- evaluateNumber n
                        pure ([], HP.primTok $ HP.IntRefTok q en)
                    HP.FontTarget fontSpec fPath ->
                        do
                        fontDef@B.FontDefinition { B.fontNr } <- Com.loadFont fPath fontSpec
                        let fontRefTok = HP.primTok $ HP.FontRefToken fontNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure ([boxElem], fontRefTok)
                liftIO $ putStrLn $ "Setting CS " <> showT cs <> " to token: " <> showT newCSTok <> (if global == HP.Global then " globally" else " locally")
                modConfState $ setControlSequence cs newCSTok global
                pure acc
            HP.SetVariable ass ->
                do
                liftConfState $ case ass of
                    HP.IntegerVariableAssignment v tgt ->
                        readOnState (evaluateNumber tgt) >>= Com.setIntegerVariable v global
                    HP.LengthVariableAssignment v tgt  ->
                        readOnState (evaluateLength tgt) >>= Com.setLengthVariable v global
                    HP.GlueVariableAssignment v tgt    ->
                        readOnState (evaluateGlue tgt) >>= Com.setGlueVariable v global
                    HP.MathGlueVariableAssignment v tgt  ->
                        readOnState (evaluateMathGlue tgt) >>= Com.setMathGlueVariable v global
                    HP.TokenListVariableAssignment v tgt ->
                        do
                        eTgt <- readOnState $ case tgt of
                            HP.TokenListAssignmentVar tgtVar   -> evaluateTokenListVariable tgtVar
                            HP.TokenListAssignmentText tgtText -> pure tgtText
                        Com.setTokenListVariable v global eTgt
                    HP.SpecialIntegerVariableAssignment v tgt ->
                        readOnState (evaluateNumber tgt) >>= (\en -> modify $ setSpecialInteger v en)
                    HP.SpecialLengthVariableAssignment v tgt ->
                        readOnState (evaluateLength tgt) >>= (\en -> modify $ setSpecialLength v en)
                pure []
            HP.ModifyVariable modCommand ->
                do
                liftConfState $ case modCommand of
                    HP.AdvanceIntegerVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ (+) <$> evaluateIntegerVariable var <*> evaluateNumber plusVal
                        Com.setIntegerVariable var global newVarVal
                    HP.AdvanceLengthVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ (+) <$> evaluateLengthVariable var <*> evaluateLength plusVal
                        Com.setLengthVariable var global newVarVal
                    HP.AdvanceGlueVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ mappend <$> evaluateGlueVariable var <*> evaluateGlue plusVal
                        Com.setGlueVariable var global newVarVal
                    HP.AdvanceMathGlueVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ mappend <$> evaluateMathGlueVariable var <*> evaluateMathGlue plusVal
                        Com.setMathGlueVariable var global newVarVal
                    -- Division of a positive integer by a positive integer
                    -- discards the remainder, and the sign of the result
                    -- changes if you change the sign of either operand.
                    HP.ScaleVariable vDir numVar scaleVal ->
                        do
                        eScaleVal <- readOnState $ evaluateNumber scaleVal
                        case numVar of
                            HP.IntegerNumericVariable var ->
                                do
                                eVar <- readOnState $ evaluateIntegerVariable var
                                let op = case vDir of
                                        Upward -> (*)
                                        Downward -> quot
                                Com.setIntegerVariable var global $ op eVar eScaleVal
                            HP.LengthNumericVariable var ->
                                do
                                eVar <- readOnState $ evaluateLengthVariable var
                                let op = case vDir of
                                        Upward -> (*)
                                        Downward -> quot
                                Com.setLengthVariable var global $ op eVar eScaleVal
                            HP.GlueNumericVariable var ->
                                do
                                eVar <- readOnState $ evaluateGlueVariable var
                                let op = case vDir of
                                        Upward -> BL.multiplyGlue
                                        Downward -> BL.divGlue
                                Com.setGlueVariable var global $ op eVar eScaleVal
                pure []
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- liftReadOnConfState $ evaluateNumber idx
                eVal <- liftReadOnConfState $ evaluateNumber val
                liftIO $ putStrLn $ "Evaluated code table index " <> showT idx <> " to " <> showT eIdx
                liftIO $ putStrLn $ "Evaluated code table value " <> showT val <> " to " <> showT eVal
                idxChar <- liftMaybeConfigError ("Invalid character code index: " <> showT eIdx) (toEnumMay eIdx)
                liftIO $ putStrLn $ "Setting " <> showT codeType <> "@" <> showT eIdx <> " (" <> showT idxChar <> ") to " <> showT eVal
                liftConfState $ updateCharCodeMap codeType idxChar eVal global
                pure []
            HP.SelectFont fNr ->
                do
                fontSel <- HP.runConfState $ Com.selectFont fNr global
                pure [BL.VListBaseElem $ B.ElemFontSelection fontSel]
            HP.SetFamilyMember fm fontRef ->
                do
                eFm <- liftReadOnConfState $ evaluateFamilyMember fm
                fNr <- liftReadOnConfState $ evaluateFontRef fontRef
                modConfState $ setFamilyMemberFont eFm fNr global
                pure []
            HP.SetBoxRegister idx box ->
                do
                eIdx <- liftReadOnConfState $ evaluateEightBitInt idx
                mayBox <- constructBox box
                modConfState $ case mayBox of
                    Nothing -> delBoxRegister eIdx global
                    Just b -> setBoxRegister eIdx b global
                pure []
        HP.runConfState (gets afterAssignmentToken) >>= \case
            Nothing -> pure ()
            Just lt ->
                do
                modify $ \s -> HP.insertLexToken s lt
                modConfState $ \c -> c{afterAssignmentToken = Nothing}
        retElems assignElems
    HP.WriteToStream n (HP.ImmediateWriteText eTxt) ->
        liftReadOnConfState $ do
        en <- evaluateNumber n
        fStreams <- asks outFileStreams
        let txtStr = Com.showExpandedBalancedText eTxt
        -- Write to:
        -- if stream number corresponds to existing, open file:
        --     file
        -- otherwise:
        --     log
        --     unless stream number is negative: terminal
        case Com.getStream fStreams en of
            Just fStream ->
                 liftIO $ hPutStrLn fStream txtStr
            Nothing ->
                do
                -- Write to terminal.
                when (en >= 0) $ liftIO $ putStrLn txtStr
                -- Write to log
                logHandle <- asks logStream
                liftIO $ hPutStrLn logHandle txtStr
        retElems []
    HP.AddBox HP.NaturalPlacement box ->
        do
        mayBox <- constructBox box
        retElems $ case mayBox of
            Nothing -> []
            Just b  -> [(BL.VListBaseElem . B.ElemBox) b]

  where
    retElems els = pure $ Just els

    noElems = retElems []

processHCommand
    :: (HP.InhibitableStream s, MonadState s m, MonadIO m)
    => s
    -> [BL.BreakableHListElem]
    -> Bool  -- restricted?
    -> HP.HModeCommand
    -> ExceptBuildT s m ([BL.BreakableHListElem], Bool)
processHCommand oldStream acc inRestricted = \case
    HP.LeaveHMode ->
        -- Inner mode: forbidden. TODO.
        if inRestricted
            then throwConfigError "Should not see vertical command in restricted horizontal mode"
            -- Outer mode: insert the rol sequence "\par" into the input. The control
            -- sequence's current meaning will be used, which might no longer be the \par
            -- primitive.
            -- (Note that we use oldStream.)
            else
                do
                let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
                put $ HP.insertLexToken oldStream parToken
                continueUnchanged
    HP.AddCharacter c ->
        liftConfigError $ do
        charCode <- readOnConfState $ evaluateCharCodeRef c
        hCharBox <- (BL.HListHBaseElem . B.ElemCharacter) <$> (readOnConfState $ Com.characterBox charCode)
        modAccum $ hCharBox : acc
    HP.HAllModesCommand aCom -> case aCom of
        -- \indent: An empty box of width \parindent is appended to the current
        -- list, and the space factor is set to 1000.
        -- TODO: Space factor.
        HP.StartParagraph HP.DoNotIndent ->
            continueUnchanged
        HP.StartParagraph HP.Indent ->
            liftConfigError $ do
            indentBox <- readOnConfState $ asks parIndentBox
            modAccum (indentBox : acc)
        -- \par: Restricted: does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            if inRestricted
                then continueUnchanged
                else pure (acc, False)
        HP.AddSpace ->
            liftConfigError $ do
            hGlue <- (BL.HVListElem . BL.ListGlue) <$> readOnConfState Com.spaceGlue
            modAccum $ hGlue : acc
        HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
            liftReadOnConfState $ do
            evalW <- case width of
                Nothing -> pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> evaluateLength ln
            evalH <- case height of
                Nothing -> pure $ Unit.toScaledPointApprox (10 :: Int) Unit.Point
                Just ln -> evaluateLength ln
            evalD <- case depth of
                Nothing -> pure 0
                Just ln -> evaluateLength ln
            let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
            modAccum $ (BL.HVListElem $ BL.VListBaseElem $ B.ElemRule rule) : acc
        HP.ModeIndependentCommand mcom ->
            handleModeIndep mcom >>= \case
                Nothing ->
                    pure (acc, False)
                Just extraAcc ->
                    modAccum $ (BL.HVListElem <$> extraAcc) <> acc
  where
    modAccum newAcc = pure (newAcc, True)

    continueUnchanged = pure (acc, True)

extractHList
    :: (HP.InhibitableStream s, MonadState s m, MonadIO m)
    => HP.IndentFlag
    -> Bool
    -> ExceptBuildT s m [BL.BreakableHListElem]
extractHList indentFlag inRestricted =
    do
    indentBox <- readOnConfState $ asks parIndentBox
    extractParagraphInner [indentBox | indentFlag == HP.Indent]
  where
    -- We build a paragraph list in reverse order.
    extractParagraphInner acc =
        do
        oldStream <- get
        (PS.State { PS.stateInput = newStream }, com) <- liftEither $ ParseError `mapLeft` HP.extractHModeCommand oldStream
        put newStream
        -- liftIO $ putStrLn $ (if inRestricted then "Restricted" else "Unrestricted") <> " horizontal mode, processing command: " <> showT com
        (procAcc, continue) <- processHCommand oldStream acc inRestricted com
        if continue
            then extractParagraphInner procAcc
            else pure procAcc

extractBreakAndSetHList
    :: (HP.InhibitableStream s, MonadState s m, MonadIO m)
    => HP.IndentFlag
    -> ExceptBuildT s m [[B.HBoxElem]]
extractBreakAndSetHList indentFlag =
    do
    hList <- extractHList indentFlag False
    readOnConfState $ do
        desiredW <- asks $ LenParamVal . lookupLengthParameter HP.HSize
        lineTol <- asks $ IntParamVal . lookupIntegerParameter HP.Tolerance
        linePen <- asks $ IntParamVal . lookupIntegerParameter HP.LinePenalty
        liftEither $ ConfigError `mapLeft` BL.breakAndSetParagraph desiredW lineTol linePen hList

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
        _prevDepth <- gets $ lookupSpecialLength HP.PrevDepth
        BL.Glue blineLength blineStretch blineShrink <- gets $ lookupGlueParameter HP.BaselineSkip
        skipLimit <- gets $ lookupLengthParameter HP.LineSkipLimit
        skip <- gets $ lookupGlueParameter HP.LineSkip
        modify $ setSpecialLength HP.PrevDepth $ naturalDepth e
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

processVCommand
    :: (HP.InhibitableStream s, MonadState s m, MonadIO m)
    => s
    -> [BL.BreakableVListElem]
    -> Bool -- internal?
    -> HP.VModeCommand
    -> ExceptBuildT s m ([BL.BreakableVListElem], Bool)
processVCommand oldStream acc inInternal = \case
    -- End recursion.
    HP.End ->
        if inInternal
            then throwConfigError "End not allowed in internal vertical mode"
        else
            do
            readOnConfState $ asks scopedConfig >>= \case
                (_, []) -> pure ()
                _ -> throwConfigError "Cannot end: not in global scope"
            gets HP.getConditionBodyState >>= \case
                Nothing -> pure ()
                Just _condState -> throwConfigError $ "Cannot end: in condition block: " <> showT _condState
            readOnConfState (asks finaliseConfig) >>= liftIO
            pure (acc, False)
    HP.EnterHMode ->
        addParagraphToPage HP.Indent
    HP.VAllModesCommand aCom -> case aCom of
        HP.StartParagraph indentFlag ->
            addParagraphToPage indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            continueUnchanged
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            continueUnchanged
        HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
            do
            evalW <- case width of
                Nothing -> readOnConfState $ gets $ lookupLengthParameter HP.HSize
                Just ln -> liftReadOnConfState $ evaluateLength ln
            evalH <- case height of
                -- TODO.
                Nothing -> readOnConfState $ pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> liftReadOnConfState $ evaluateLength ln
            evalD <- case depth of
                Nothing -> readOnConfState $ pure 0
                Just ln -> liftReadOnConfState $ evaluateLength ln
            let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
            modAccum $ (BL.VListBaseElem $ B.ElemRule rule) : acc
        HP.ModeIndependentCommand mcom ->
            handleModeIndep mcom >>= \case
                Nothing ->
                    pure (acc, False)
                Just extraAcc ->
                    modAccum $ extraAcc <> acc
  where
    modAccum newAcc = pure (newAcc, True)

    continueUnchanged = pure (acc, True)

    addParagraphToPage indentFlag =
        do
        -- If the command shifts to horizontal mode, run '\indent', and re-read the
        -- stream as if the commands just seen hadn't been read.
        -- (Note that we set "oldStream", not "newStream".)
        -- Paraboxes are returned in reading order.
        put oldStream
        lineBoxes <- extractBreakAndSetHList indentFlag
        desiredW <- readOnConfState $ asks $ lookupLengthParameter HP.HSize
        let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
        newAcc <- HP.runConfState $ foldM addVListElem acc $ BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes
        -- Continue iff not in internal mode.
        pure (newAcc, not inInternal)

extractVList
    :: (HP.InhibitableStream s, MonadState s m, MonadIO m)
    => Bool
    -> ExceptBuildT s m [BL.BreakableVListElem]
extractVList inInternal =
    extractVListInner []
  where
    extractVListInner acc =
        do
        oldStream <- get
        (PS.State { PS.stateInput = newStream }, com) <- liftEither $ ParseError `mapLeft` HP.extractVModeCommand oldStream
        put newStream
        -- liftIO $ putStrLn $ (if inInternal then "Internal" else "Outer") <> " vertical mode, processing command: " <> showT com
        (procAcc, continue) <- processVCommand oldStream acc inInternal com
        if continue
            then extractVListInner procAcc
            else pure procAcc

extractBreakAndSetVList
    :: (HP.InhibitableStream s, MonadState s m, MonadIO m)
    => ExceptBuildT s m [B.Page]
extractBreakAndSetVList = do
    vList <- extractVList False
    liftIO $ print $ length vList
    desiredH <- HP.runConfState $ gets $ LenParamVal . lookupLengthParameter HP.VSize
    pure $ BL.runPageBuilder desiredH BL.newCurrentPage $ reverse vList
