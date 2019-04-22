module HeX.Build where

import HeXlude

import           Control.Monad                  ( foldM
                                                , when )
import           Control.Monad.Except           ( liftEither )
import           Data.Either.Combinators        ( mapLeft )
import           Safe                           ( toEnumMay )
import qualified Text.Megaparsec               as P

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.BuildHelp
import           HeX.Config
import qualified HeX.Lex                       as Lex
import           HeX.Evaluate
import qualified HeX.Parse                     as HP
import qualified HeX.Unit                      as Unit
import qualified HeX.Command                   as Com
import qualified HeX.Variable                  as Var

data ModeIndependentResult
    = AddElems [BL.BreakableVListElem]
    | EnterBoxMode HP.ExplicitBox
    | FinishBoxMode B.DesiredLength

handleModeIndependentCommand
    :: HP.InhibitableStream s
    => Axis
    -> HP.ModeIndependentCommand -> ExceptBuildVM s ModeIndependentResult
handleModeIndependentCommand mode = \case
    HP.Message stdOutStream eTxt ->
        do
        let _handle = case stdOutStream of
                HP.StdOut -> stdout
                HP.StdErr -> stderr
        liftIO $ hPutStrLn _handle $ Com.showExpandedBalancedText eTxt
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
        p <- liftEvalOnConfState n
        pure $ AddElems [BL.ListPenalty $ BL.Penalty p]
    HP.AddKern ln ->
        do
        k <- liftEvalOnConfState ln
        pure $ AddElems [BL.VListBaseElem $ B.ElemKern $ B.Kern k]
    HP.AddGlue (HP.ModedGlue cmdMode g) ->
        do
        checkMode cmdMode
        eG <- liftEvalOnConfState g
        pure $ AddElems [BL.ListGlue eG]
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
                        en <- texEvaluate n
                        pure ([], HP.primTok $ HP.IntRefTok q en)
                    HP.FontTarget fontSpec fPath ->
                        do
                        fontDef@B.FontDefinition { B.fontNr } <- Com.loadFont fPath fontSpec
                        let fontRefTok = HP.primTok $ HP.FontRefToken fontNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure ([boxElem], fontRefTok)
                liftIO $ putText $ "Setting CS " <> showT cs <> " to token: " <> showT newCSTok <> (if global == HP.Global then " globally" else " locally")
                modConfState $ setControlSequence cs newCSTok global
                pure acc
            HP.SetVariable ass ->
                do
                liftConfState $ case ass of
                    HP.IntegerVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.LengthVariableAssignment v tgt  ->
                        Var.setValueFromAST v global tgt
                    HP.GlueVariableAssignment v tgt    ->
                        Var.setValueFromAST v global tgt
                    HP.MathGlueVariableAssignment v tgt  ->
                        Var.setValueFromAST v global tgt
                    HP.TokenListVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.SpecialIntegerVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.SpecialLengthVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                pure []
            HP.ModifyVariable modCommand ->
                do
                liftConfState $ case modCommand of
                    HP.AdvanceIntegerVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceLengthVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceGlueVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceMathGlueVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.ScaleVariable vDir numVar scaleVal ->
                        case numVar of
                            HP.IntegerNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                            HP.LengthNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                            HP.GlueNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                pure []
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- liftEvalOnConfState idx
                eVal <- liftEvalOnConfState val
                liftIO $ putText $ "Evaluated code table index " <> showT idx <> " to " <> showT eIdx
                liftIO $ putText $ "Evaluated code table value " <> showT val <> " to " <> showT eVal
                idxChar <- liftMaybeConfigError ("Invalid character code index: " <> showT eIdx) (toEnumMay eIdx)
                liftIO $ putText $ "Setting " <> showT codeType <> "@" <> showT eIdx <> " (" <> showT idxChar <> ") to " <> showT eVal
                liftConfState $ updateCharCodeMap codeType idxChar eVal global
                pure []
            HP.SelectFont fNr ->
                do
                HP.runConfState $ Com.selectFont fNr global
                pure [BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr]
            HP.SetFamilyMember fm fontRef ->
                do
                eFm <- liftEvalOnConfState fm
                fNr <- liftEvalOnConfState fontRef
                modConfState $ setFamilyMemberFont eFm fNr global
                pure []
            -- HP.SetBoxRegister idx box ->
            --     do
            --     eIdx <- liftEvalOnConfState idx
            --     mayBox <- constructBox box
            --     modConfState $ case mayBox of
            --         Nothing -> delBoxRegister eIdx global
            --         Just b -> setBoxRegister eIdx b global
            --     pure []
            HP.SetFontChar (HP.FontCharRef fontChar fontRef) charRef ->
                do
                fNr <- liftEvalOnConfState fontRef
                eCharRef <- liftEvalOnConfState charRef
                let updateFontChar f = case fontChar of
                        HP.SkewChar -> f { skewChar = eCharRef }
                        HP.HyphenChar -> f { hyphenChar = eCharRef }
                HP.runConfState $ modifyFont fNr updateFontChar
                pure []
            oth ->
                panic $ show oth
        HP.runConfState (gets afterAssignmentToken) >>= \case
            Nothing -> pure ()
            Just lt ->
                do
                modify $ \s -> HP.insertLexToken s lt
                modConfState $ \c -> c{afterAssignmentToken = Nothing}
        pure $ AddElems assignElems
    HP.WriteToStream n (HP.ImmediateWriteText eTxt) ->
        liftReadOnConfState $
            do
            en <- texEvaluate n
            fStreams <- asks outFileStreams
            let txtStr = Com.showExpandedBalancedText eTxt
            -- Write to:
            -- if stream number corresponds to existing, open file:
            --     file
            -- otherwise:
            --     log
            --     unless stream number is negative: terminal
            case Com.getFileStream fStreams en of
                Just fStream ->
                     liftIO $ hPutStrLn fStream txtStr
                Nothing ->
                    do
                    -- Write to terminal.
                    when (en >= 0) $ liftIO $ putText txtStr
                    -- Write to log
                    logHandle <- asks logStream
                    liftIO $ hPutStrLn logHandle txtStr
            pure $ AddElems []
    -- Start a new level of grouping.
    HP.ChangeScope (HP.Sign True) trig ->
        do
        modConfState $ pushGroup $ ScopeGroup newLocalScope (LocalStructureGroup trig)
        noElems
    -- Do the appropriate finishing actions, undo the
    -- effects of non-global assignments, and leave the
    -- group. Maybe leave the current mode.
    HP.ChangeScope (HP.Sign False) trig ->
        HP.runConfState $ gets popGroup >>= \case
            Nothing ->
                throwConfigError "No group to leave"
            Just (group, poppedConfig) ->
                do
                put poppedConfig
                case group of
                    -- Undo the effects of non-global
                    -- assignments without leaving the
                    -- current mode.
                    ScopeGroup _ (LocalStructureGroup trigConf) ->
                        do
                        when (trigConf /= trig) $ throwConfigError $ "Entry and exit group triggers differ: " <> showT (trig, trigConf)
                        pure $ AddElems []
                    -- - Undo the effects of non-global
                    --   assignments
                    -- - package the [box] using the size
                    --   that was saved on the stack
                    -- - complete the \setbox command
                    -- - return to the mode we were in at
                    --   the time of the \setbox.
                    ScopeGroup _ (ExplicitBoxGroup desiredLength) ->
                        pure $ FinishBoxMode desiredLength
                -- HP.ExplicitVBox alignType ->
                --     ((\els -> B.VBoxContents els alignType) . BL.setVList BL.NaturallyGood . reverse) <$> extractVList True
    HP.AddBox HP.NaturalPlacement (HP.FetchedRegisterBox fetchMode idx) ->
        do
        eIdx <- liftEvalOnConfState idx
        fetchedMaybeBox <- readOnConfState $ asks $ lookupBoxRegister eIdx
        case fetchMode of
            HP.Lookup -> pure ()
            HP.Pop -> HP.runConfState $ modify $ delBoxRegister eIdx HP.Local
        pure $ AddElems $ case fetchedMaybeBox of
            Nothing -> []
            Just b  -> [(BL.VListBaseElem . B.ElemBox) b]
    HP.AddBox HP.NaturalPlacement (HP.ExplicitBox spec boxType) ->
        -- Initiate a new level of grouping. Enter inner mode.
        do
        eSpec <- liftEvalOnConfState spec
        modConfState $ pushGroup (ScopeGroup newLocalScope (ExplicitBoxGroup eSpec))
        pure $ EnterBoxMode boxType
    oth ->
        panic $ show oth

  where
    noElems = pure $ AddElems []

    checkMode cmdMode =
        when (cmdMode /= mode) (throwConfigError "Command for wrong mode")

    -- HP.LeaveHMode ->
    --     -- Inner mode: forbidden. TODO.
    --     if inRestricted
    --         then throwConfigError "Should not see vertical command in restricted horizontal mode"
    --         -- Outer mode: insert the rol sequence "\par" into the input. The control
    --         -- sequence's current meaning will be used, which might no longer be the \par
    --         -- primitive.
    --         -- (Note that we use oldStream.)
    --         else
    --             do
    --             let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
    --             put $ HP.insertLexToken oldStream parToken
    --             continueUnchanged


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

-- A mode-state always has a main vertical list, and maybe a child.
data VModeContents
    = VModeContents VList (Maybe VModeChild)
    deriving ( Show )

-- A VList's child is either an HList (for a para or box), or a VList for a vbox.
data VModeChild
    = VModeChildHMode HModeType HModeContents
    | VModeChildVBoxMode VBoxModeContents
    deriving ( Show )

data HModeType
    = ParaHMode
    | BoxHMode
    deriving ( Show )

-- Regardless of whether an HList is for a paragraph or an hbox, its child can be either
-- an HList for an hbox, or a VList for a vbox.
data HModeContents = HModeContents HList (Maybe HModeChild)
    deriving ( Show )

data HModeChild
    = HModeChildHBoxMode HModeContents
    | HModeChildVBoxMode VBoxModeContents
    deriving ( Show )

data VBoxModeContents
    = VBoxModeContents B.VBoxAlignType VModeContents
    deriving ( Show )

emptyHModeContents = HModeContents [] Nothing

emptyVModeContents = VModeContents [] Nothing

boxTypeToFreshVChild = \case
    HP.ExplicitHBox ->
        VModeChildHMode
            BoxHMode
            emptyHModeContents
    HP.ExplicitVBox alignType ->
        VModeChildVBoxMode
            (VBoxModeContents alignType emptyVModeContents)

boxTypeToFreshHChild = \case
    HP.ExplicitHBox ->
        HModeChildHBoxMode emptyHModeContents
    HP.ExplicitVBox alignType ->
        HModeChildVBoxMode
            (VBoxModeContents alignType emptyVModeContents)

data ParaParentResult
    = ParaLeaveMode (Maybe B.DesiredLength)
    | ModifyPara HModeContents

handleCommandInParaMode (HModeContents paraHList maybeChild) command =
    case maybeChild of
        Nothing ->
            case command of
                HP.HModeCommand (HP.AddCharacter c) ->
                    do
                    hCharBox <- liftConfigError $ do
                        charCode <- readOnConfState $ texEvaluate c
                        (BL.HListHBaseElem . B.ElemCharacter) <$> (readOnConfState $ Com.characterBox charCode)
                    addElems [hCharBox]
                HP.AddSpace ->
                    do
                    hGlue <- liftConfigError $
                        (BL.HVListElem . BL.ListGlue) <$> readOnConfState Com.spaceGlue
                    addElems [hGlue]
                HP.StartParagraph HP.DoNotIndent ->
                    addElems []
                -- \indent: An empty box of width \parindent is appended to the current
                -- list, and the space factor is set to 1000.
                -- TODO: Space factor.
                HP.StartParagraph HP.Indent ->
                    do
                    indentBox <- liftConfigError $ readOnConfState $ asks parIndentBox
                    addElems [indentBox]
                -- \par: Restricted: does nothing. Unrestricted: ends mode.
                HP.EndParagraph ->
                    pure (ParaLeaveMode Nothing)
            --         -- HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
            --         --     liftReadOnConfState $ do
            --         --     evalW <- case width of
            --         --         Nothing -> pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
            --         --         Just ln -> texEvaluate ln
            --         --     evalH <- case height of
            --         --         Nothing -> pure $ Unit.toScaledPointApprox (10 :: Int) Unit.Point
            --         --         Just ln -> texEvaluate ln
            --         --     evalD <- case depth of
            --         --         Nothing -> pure 0
            --         --         Just ln -> texEvaluate ln
            --         --     let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
            --         --     modAccum $ (BL.HVListElem $ BL.VListBaseElem $ B.ElemRule rule) : acc
                HP.ModeIndependentCommand modeIndependentCommand ->
                    do
                    modeIndependentResult <- handleModeIndependentCommand Horizontal modeIndependentCommand
                    case modeIndependentResult of
                        AddElems extraElems ->
                            addElems ((BL.HVListElem <$> extraElems))
                        EnterBoxMode boxType ->
                            pure $ ModifyPara
                                (HModeContents
                                    paraHList
                                    (Just
                                        (boxTypeToFreshHChild boxType)))
                        FinishBoxMode desiredLength ->
                            pure (ParaLeaveMode (Just desiredLength))
        -- Just (HModeChildHBoxMode hModeContents) ->
        --     handleCommandInHBoxMode hModeContents command >>= \case
        --         ParaLeaveMode finalBoxHList ->
        --             do
        --             let toBox elemList = B.Box (B.HBoxContents elemList) B.Natural
        --             paraHListWithHBox <- paraHList $ BL.VListBaseElem . B.ElemBox . toBox) finalBoxHList
        --             extractBreakAndSetVList (VModeContents paraHListWithHBox Nothing)
        --         ModifyPara hModeContents ->
        --             extractBreakAndSetVList
        --                 (VModeContents
        --                     mainVList
        --                     (Just
        --                         (VModeChildHMode
        --                             ParaHMode
        --                             hModeContents)))
  where
    addElems elems =
        pure $ ModifyPara
            (HModeContents
                (elems <> paraHList)
                Nothing)

data HBoxParentResult
    = LeaveHBoxMode B.DesiredLength
    | ModifyHBox HModeContents

handleCommandInHBoxMode (HModeContents boxHList maybeChild) command =
    case maybeChild of
        Nothing ->
            case command of
                HP.HModeCommand (HP.AddCharacter c) ->
                    do
                    hCharBox <- liftConfigError $ do
                        charCode <- readOnConfState $ texEvaluate c
                        (BL.HListHBaseElem . B.ElemCharacter) <$> (readOnConfState $ Com.characterBox charCode)
                    addElems [hCharBox]
                HP.AddSpace ->
                    do
                    hGlue <- liftConfigError $
                        (BL.HVListElem . BL.ListGlue) <$> readOnConfState Com.spaceGlue
                    addElems [hGlue]
                HP.StartParagraph HP.DoNotIndent ->
                    addElems []
                -- \indent: An empty box of width \parindent is appended to the current
                -- list, and the space factor is set to 1000.
                -- TODO: Space factor.
                HP.StartParagraph HP.Indent ->
                    do
                    indentBox <- liftConfigError $ readOnConfState $ asks parIndentBox
                    addElems [indentBox]
                -- \par: Restricted: does nothing. Unrestricted: ends mode.
                HP.EndParagraph ->
                    addElems []
            --         -- HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
            --         --     liftReadOnConfState $ do
            --         --     evalW <- case width of
            --         --         Nothing -> pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
            --         --         Just ln -> texEvaluate ln
            --         --     evalH <- case height of
            --         --         Nothing -> pure $ Unit.toScaledPointApprox (10 :: Int) Unit.Point
            --         --         Just ln -> texEvaluate ln
            --         --     evalD <- case depth of
            --         --         Nothing -> pure 0
            --         --         Just ln -> texEvaluate ln
            --         --     let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
            --         --     modAccum $ (BL.HVListElem $ BL.VListBaseElem $ B.ElemRule rule) : acc
                HP.ModeIndependentCommand modeIndependentCommand ->
                    do
                    modeIndependentResult <- handleModeIndependentCommand Horizontal modeIndependentCommand
                    case modeIndependentResult of
                        AddElems extraElems ->
                            addElems ((BL.HVListElem <$> extraElems))
                        EnterBoxMode boxType ->
                            pure $ ModifyHBox
                                (HModeContents
                                    boxHList
                                    (Just
                                        (boxTypeToFreshHChild boxType)))
                        FinishBoxMode desiredLength ->
                            pure (LeaveHBoxMode desiredLength)
        -- Just (HModeChildHBoxMode hModeContents) ->
        --     handleCommandInHBoxMode hModeContents command >>= \case
        --         LeaveHBoxMode finalBoxHList ->
        --             do
        --             let toBox elemList = B.Box (B.HBoxContents elemList) B.Natural
        --             paraHListWithHBox <- boxHList $ BL.VListBaseElem . B.ElemBox . toBox) finalBoxHList
        --             extractBreakAndSetVList (VModeContents paraHListWithHBox Nothing)
        --         ModifyHBox hModeContents ->
        --             extractBreakAndSetVList
        --                 (VModeContents
        --                     mainVList
        --                     (Just
        --                         (VModeChildHMode
        --                             ParaHMode
        --                             hModeContents)))
  where
    addElems elems =
        pure $ ModifyHBox
            (HModeContents
                (elems <> boxHList)
                Nothing)

extractBreakAndSetVList :: HP.InhibitableStream s => VModeContents -> ExceptBuildVM s [B.Page]
extractBreakAndSetVList modeState@(VModeContents mainVList maybeMainChild) =
    do
    oldStream <- get
    (P.State { P.stateInput = newStream }, command) <- liftEither $ ParseError `mapLeft` HP.extractCommand oldStream
    put newStream
    case maybeMainChild of
        -- Outer vertical mode.
        Nothing ->
            do
            let inInternal = False
            case command of
                -- End recursion.
                HP.VModeCommand HP.End ->
                    if inInternal
                        then throwConfigError "End not allowed in internal vertical mode"
                    else
                        do
                        gets HP.getConditionBodyState >>= \case
                            Nothing -> pure ()
                            Just _condState -> throwConfigError $ "Cannot end: in condition block: " <> showT _condState
                        readOnConfState (asks finaliseConfig) >>= liftIO
                        desiredH <- HP.runConfState $ gets $ LenParamVal . lookupLengthParameter HP.VSize
                        pure $ BL.runPageBuilder desiredH BL.newCurrentPage $ reverse mainVList
                HP.HModeCommand _ ->
                    startParagraph HP.Indent
                HP.StartParagraph indentFlag ->
                    startParagraph indentFlag
                -- \par does nothing in vertical mode.
                HP.EndParagraph ->
                    extractBreakAndSetVList modeState
                -- <space token> has no effect in vertical modes.
                HP.AddSpace ->
                    extractBreakAndSetVList modeState
                -- HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
                --     do
                --     evalW <- case width of
                --         Nothing -> readOnConfState $ gets $ lookupLengthParameter HP.HSize
                --         Just ln -> liftEvalOnConfState ln
                --     evalH <- case height of
                --         -- TODO.
                --         Nothing -> readOnConfState $ pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                --         Just ln -> liftEvalOnConfState ln
                --     evalD <- case depth of
                --         Nothing -> readOnConfState $ pure 0
                --         Just ln -> liftEvalOnConfState ln
                --     let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
                --     modAccum $ (BL.VListBaseElem $ B.ElemRule rule) : acc
                HP.ModeIndependentCommand modeIndependentCommand ->
                    do
                    modeIndependentResult <- handleModeIndependentCommand Vertical modeIndependentCommand
                    extractBreakAndSetVList $ case modeIndependentResult of
                        AddElems extraElems ->
                            VModeContents (extraElems <> mainVList) maybeMainChild
                        EnterBoxMode boxType ->
                            VModeContents mainVList (Just (boxTypeToFreshVChild boxType))
                oth ->
                    panic $ "Not implemented, outer V mode: " <> show command
              where
                startParagraph indentFlag =
                    do
                    -- If the command shifts to horizontal mode, run
                    -- '\indent', and re-read the stream as if the
                    -- command hadn't been read. (Note that we set
                    -- "oldStream", not "newStream".)
                    put oldStream
                    paraHList <- case indentFlag of
                        HP.Indent ->
                            do
                            b <- readOnConfState $ asks parIndentBox
                            pure [b]
                        HP.DoNotIndent ->
                            pure []
                    extractBreakAndSetVList
                        (VModeContents
                            mainVList
                            (Just
                                (VModeChildHMode
                                    ParaHMode
                                    (HModeContents paraHList Nothing))))
        Just (VModeChildHMode ParaHMode hModeContents@(HModeContents paraHList _)) ->
            handleCommandInParaMode hModeContents command >>= \case
                ParaLeaveMode maybeDesiredLength ->
                    do
                    desiredW <- readOnConfState $ asks $ lookupLengthParameter HP.HSize
                    lineBoxes <- readOnConfState $
                        do
                        lineTol <- asks $ IntParamVal . lookupIntegerParameter HP.Tolerance
                        linePen <- asks $ IntParamVal . lookupIntegerParameter HP.LinePenalty
                        liftEither $ ConfigError `mapLeft` BL.breakAndSetParagraph (LenParamVal desiredW) lineTol linePen paraHList
                    let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
                    mainVListWithParagraph <- HP.runConfState $
                        foldM addVListElem mainVList $ BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes

                    case maybeDesiredLength of
                        Nothing ->
                            extractBreakAndSetVList (VModeContents mainVListWithParagraph Nothing)
                        (Just desiredLength) ->
                            notImplemented
                ModifyPara hModeContents ->
                    extractBreakAndSetVList
                        (VModeContents
                            mainVList
                            (Just
                                (VModeChildHMode
                                    ParaHMode
                                    hModeContents)))
        Just (VModeChildHMode BoxHMode hModeContents@(HModeContents boxHList _)) ->
            handleCommandInHBoxMode hModeContents command >>= \case
                LeaveHBoxMode desiredLength ->
                    do
                    -- TODO: I think glue status and desired length
                    -- duplicate some meaning.
                    let hBoxElems = (BL.setHList BL.NaturallyGood . reverse) boxHList
                        hBox = BL.VListBaseElem $ B.ElemBox $ B.Box (B.HBoxContents hBoxElems) desiredLength
                    extractBreakAndSetVList
                        (VModeContents
                            (hBox : mainVList)
                            Nothing)
                ModifyHBox hModeContents ->
                    extractBreakAndSetVList
                        (VModeContents
                            mainVList
                            (Just
                                (VModeChildHMode
                                    BoxHMode
                                    hModeContents)))
