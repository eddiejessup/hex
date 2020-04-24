{-# LANGUAGE RankNTypes #-}

module Hex.Command.Build where

import           Hexlude

import qualified Data.Sequence               as Seq
import qualified Data.Path

import           TFM                         (TFMError)

import qualified Hex.Box                     as B
import           Hex.BreakList               (HList, VList)
import qualified Hex.BreakList               as BL
import qualified Hex.Config.Codes     as Codes
import           Hex.Command.Commands
import           Hex.Command.Common
import           Hex.Command.ModeIndependent
import           Hex.Config
import qualified Hex.Lex                     as Lex
import qualified Hex.Parse                   as HP
import qualified Hex.Resolve          as HR
import qualified Hex.Variable         as Var
import           Hex.Evaluate

newtype BuildError = BuildError Text
    deriving stock (Show)

data EndParaReason
    = EndParaSawEndParaCommand
    | EndParaSawLeaveBox

handleCommandInParaMode
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> s
    -> HList
    -> HP.Command
    -> m (s, HList, Maybe EndParaReason)
handleCommandInParaMode oldS newS hList@(BL.HList hElemSeq) command =
    case command of
        HP.VModeCommand _ ->
            -- Insert the control sequence "\par" into the input. The control
            -- sequence's current meaning will be used, which might no longer be the \par
            -- primitive.
            -- (Note that we use oldS.)
            pure (HP.insertLexToken oldS Lex.parToken, hList, Nothing)
        HP.HModeCommand (HP.AddHGlue g) ->
            (newS, , Nothing) . addElem <$> hModeAddHGlue g
        HP.HModeCommand (HP.AddCharacter c) ->
            (newS, , Nothing) . addElem <$> hModeAddCharacter c
        HP.HModeCommand (HP.AddHRule rule) ->
            (newS, , Nothing) . addElem <$> hModeAddRule rule
        HP.AddSpace ->
            (newS, , Nothing) . addElem <$> hModeAddSpace
        HP.StartParagraph indentFlag ->
            (newS, , Nothing) . addMaybeElem' <$> hModeStartParagraph indentFlag
        -- \par: Restricted: does nothing. Unrestricted (this mode): ends mode.
        HP.EndParagraph ->
            pure (newS, hList, Just EndParaSawEndParaCommand)
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (doneS, modeIndRes) <- handleModeIndependentCommand newS modeIndependentCommand
            pure $ case modeIndRes of
                AddMaybeElem mayExtraElem ->
                    (doneS, addMaybeElem' (BL.HVListElem <$> mayExtraElem), Nothing)
                FinishBoxMode ->
                    (doneS, hList, Just EndParaSawLeaveBox)
        oth ->
            panic $ show oth
  where
    addElem e = BL.HList $ hElemSeq :|> e
    addMaybeElem' mayE = BL.HList $ addMaybeElem hElemSeq mayE

handleCommandInHBoxMode
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> s
    -> HList
    -> HP.Command
    -> m (s, HList, Maybe ())
handleCommandInHBoxMode _ newS hList@(BL.HList hElemSeq) command =
    case command of
        HP.VModeCommand vModeCommand ->
            throwError $ injectTyped $ BuildError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
        HP.HModeCommand (HP.AddCharacter c) ->
            (newS,,Nothing) . addElem <$> hModeAddCharacter c
        HP.HModeCommand (HP.AddHGlue g) ->
            (newS,,Nothing) . addElem <$> hModeAddHGlue g
        HP.HModeCommand (HP.AddHRule rule) ->
            (newS,,Nothing) . addElem <$> hModeAddRule rule
        HP.AddSpace ->
            (newS,,Nothing) . addElem <$> hModeAddSpace
        HP.StartParagraph indentFlag ->
            (newS,,Nothing) . addMaybeElem' <$> hModeStartParagraph indentFlag
        -- \par: Restricted (this mode): does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            pure (newS, hList, Nothing)
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (doneS, modeIndRes) <- handleModeIndependentCommand newS modeIndependentCommand
            pure $ case modeIndRes of
                AddMaybeElem mayExtraElem ->
                    (doneS, addMaybeElem' (BL.HVListElem <$> mayExtraElem), Nothing)
                FinishBoxMode ->
                    (doneS, hList, Just ())
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    addElem e = BL.HList $ hElemSeq :|> e
    addMaybeElem' mayE = BL.HList $ addMaybeElem hElemSeq mayE

handleCommandInVBoxMode
    :: forall s st e m
     . ( HP.TeXParseable s st e m

       , AsType TFMError e
       , AsType BuildError e
       , AsType Data.Path.PathError e

       , MonadIO m
       )
    => s
    -> s
    -> VList
    -> HP.Command
    -> m (s, VList, Maybe ())
handleCommandInVBoxMode oldS newS vList command =
    case command of
        HP.VModeCommand HP.End ->
            throwError $ injectTyped $ BuildError "End not allowed in internal vertical mode"
        HP.VModeCommand (HP.AddVGlue g) ->
            vModeAddVGlue g >>= addElem <&> (newS,,Nothing)
        HP.VModeCommand (HP.AddVRule rule) ->
            vModeAddRule rule >>= addElem <&> (newS,,Nothing)
        HP.HModeCommand _ ->
            addPara HP.Indent
        HP.StartParagraph indentFlag ->
            addPara indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            pure (newS, vList, Nothing)
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            pure (newS, vList, Nothing)
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (doneS, modeIndRes) <- handleModeIndependentCommand newS modeIndependentCommand
            case modeIndRes of
                AddMaybeElem mayExtraElem ->
                    (doneS,, Nothing) <$> addMaybeElem' mayExtraElem
                FinishBoxMode ->
                    pure (doneS, vList, Just ())
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    addElem e = addVListElem vList e

    addMaybeElem' = maybe (pure vList) addElem

    addPara :: HP.IndentFlag -> m (s, VList, Maybe ())
    addPara indentFlag = do
        -- Note oldS.
        (doneS, finalParaHList, endParaReason) <- extractPara indentFlag oldS
        vListWithPara <- appendParagraph finalParaHList vList
        pure $ (doneS, vListWithPara,) $ case endParaReason of
            EndParaSawEndParaCommand ->
                Nothing
            EndParaSawLeaveBox ->
                Just ()

appendParagraph
    :: ( MonadIO m

       , MonadState st m
       , HasType Config st

       , MonadError e m
       , AsType BuildError e
       )
    => HList
    -> VList
    -> m VList
appendParagraph paraHList vList =
    do
    lineBoxes <- hListToParaLineBoxes paraHList
    let lineBoxContents = (B.HBoxContents <$>) <$> lineBoxes
    let boxElems = BL.VListBaseElem . B.ElemBox <$> lineBoxContents
    foldM addVListElem vList boxElems

hListToParaLineBoxes
    :: ( MonadState st m -- Read-only
       , HasType Config st
       , MonadError e m
       , AsType BuildError e
       )
    => HList
    -> m (Seq (B.Box B.HBox))
hListToParaLineBoxes hList =
    do
    hSize <- gets $ LenParamVal . lookupLengthParameter HP.HSize . getTyped @Config
    lineTol <- gets $ IntParamVal . lookupTeXIntParameter HP.Tolerance . getTyped @Config
    linePen <- gets $ IntParamVal . lookupTeXIntParameter HP.LinePenalty . getTyped @Config
    case BL.breakAndSetParagraph hSize lineTol linePen hList of
        Left err -> throwError $ injectTyped $ BuildError err
        Right v -> pure v

handleCommandInMainVMode
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> s
    -> VList
    -> HP.Command
    -> m (s, VList, Maybe ())
handleCommandInMainVMode oldS newS vList command =
    -- traceText ("in main v mode, handling command: " <> show command) $ case command of
    case command of
        HP.VModeCommand HP.End ->
            pure (newS, vList, Just ())
        HP.VModeCommand (HP.AddVGlue g) ->
            vModeAddVGlue g >>= addElem <&> (newS, , Nothing)
        HP.VModeCommand (HP.AddVRule rule) ->
            vModeAddRule rule >>= addElem <&> (newS, , Nothing)
        HP.HModeCommand _ ->
            addPara HP.Indent
        HP.StartParagraph indentFlag ->
            addPara indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            pure (newS, vList, Nothing)
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            pure (newS, vList, Nothing)
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (doneS, modeIndRes) <- handleModeIndependentCommand newS modeIndependentCommand
            case modeIndRes of
                AddMaybeElem mayExtraElem ->
                    (doneS, , Nothing) <$> addMaybeElem' mayExtraElem
                FinishBoxMode ->
                    throwError $ injectTyped $
                        BuildError "No box to end: in main V mode"
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    addElem e = addVListElem vList e
    addMaybeElem' = maybe (pure vList) addElem

    addPara indentFlag =
        do
        -- If the command shifts to horizontal mode, run '\indent', and re-read
        -- the stream as if the command hadn't been read. (Note that we read
        -- from "oldS", not "newS".)
        (doneS, paraHList, endParaReason) <- extractPara indentFlag oldS
        case endParaReason of
            EndParaSawEndParaCommand -> do
                appendedVList <- appendParagraph paraHList vList
                pure (doneS, appendedVList, Nothing)
            EndParaSawLeaveBox ->
                throwError $ injectTyped $
                    BuildError "No box to end: in paragraph within main V mode"

extractPara
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m -- Read-only.

       , MonadIO m
       )
    => HP.IndentFlag
    -> s
    -> m (s, HList, EndParaReason)
extractPara indentFlag s = do
    initList <- case indentFlag of
        HP.Indent -> do
            indentBox <- gets (view $ typed @Config . to parIndentBox)
            pure $ Seq.singleton indentBox
        HP.DoNotIndent ->
            pure mempty
    runCommandLoop handleCommandInParaMode s (BL.HList initList)

extractMainVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> m (s, VList)
extractMainVList s = do
    (doneS, vList, _) <- runCommandLoop handleCommandInMainVMode s mempty
    pure (doneS, vList)

extractBreakAndSetMainVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> m (s, Seq B.Page, IntParamVal 'HP.Mag)
extractBreakAndSetMainVList s = do
    (finalS, finalMainVList) <- extractMainVList s
    case HP.getConditionBodyState finalS of
        Nothing -> pure ()
        Just _condState -> throwError $ injectTyped $ BuildError $ "Cannot end: in condition block: " <> show _condState
    join $ gets $ view $ typed @Config . to finaliseConfig
    desiredH <- gets $ view $ typed @Config . to (LenParamVal . lookupLengthParameter HP.VSize)
    let pages = BL.runPageBuilder desiredH BL.newCurrentPage finalMainVList
    mag <- gets $ view $ typed @Config . to (IntParamVal . lookupTeXIntParameter HP.Mag)
    pure (finalS, pages, mag)

data ModeIndependentResult
    = AddMaybeElem (Maybe BL.VListElem)
    | FinishBoxMode

handleModeIndependentCommand
    :: ( MonadIO m
       , AsType Data.Path.PathError e
       , AsType TFMError e
       , AsType BuildError e

       , HP.TeXParseable s st e m
       )
    => s
    -> HP.ModeIndependentCommand
    -> m (s, ModeIndependentResult)
handleModeIndependentCommand s = \case
    HP.Message stdOutStream eTxt -> do
        let _handle = case stdOutStream of
                HP.StdOut -> stdout
                HP.StdErr -> stderr
        liftIO $ hPutStrLn _handle (toS (Codes.unsafeCodesAsChars (showExpandedBalancedText eTxt)) :: Text)
        pure (s, AddMaybeElem Nothing)
    HP.Relax ->
        pure (s, AddMaybeElem Nothing)
    HP.IgnoreSpaces ->
        pure (s, AddMaybeElem Nothing)
    -- Re-insert the ⟨token⟩ into the input just after running the next
    -- assignment command. Later \afterassignment commands override earlier
    -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
    -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
    -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
    -- inserted by \everyhbox or \everyvbox.
    HP.SetAfterAssignmentToken lt -> do
        modify $ typed @Config . field @"afterAssignmentToken" ?~ lt
        pure (s, AddMaybeElem Nothing)
    HP.AddPenalty n ->
        (s,) . AddMaybeElem . Just . BL.ListPenalty . BL.Penalty <$> texEvaluate n
    HP.AddKern ln ->
        (s,) . AddMaybeElem . Just . BL.VListBaseElem . B.ElemKern . B.Kern <$> texEvaluate ln
    HP.Assign HP.Assignment { HP.global, HP.body } ->
        do
        (postAssignS, assignResult) <- case body of
            HP.DefineControlSequence cs tgt ->
                do
                (maybeElem, newCSTok) <- case tgt of
                    HP.MacroTarget macro ->
                        pure (Nothing, HR.syntaxTok $ HP.MacroTok macro)
                    -- TODO: If a \let target is an active character, should we
                    -- treat it as a control sequence, or a char-cat pair?
                    HP.LetTarget (Lex.CharCatToken tgtCC) ->
                        pure (Nothing, HR.primTok $ HP.LetCharCat tgtCC)
                    HP.LetTarget (Lex.ControlSequenceToken tgtCS) ->
                        do
                        mayCS <- gets (view $ typed @Config . to (lookupCSProper tgtCS))
                        let resTok = fromMaybe (HP.PrimitiveToken HP.RelaxTok) mayCS
                        pure (Nothing, resTok)
                    HP.ShortDefineTarget q n ->
                        do
                        en <- texEvaluate n
                        pure (Nothing, HR.primTok $ HP.IntRefTok q en)
                    HP.FontTarget fontSpec fPath ->
                        do
                        fontDef@B.FontDefinition { B.fontNr } <- loadFont fPath fontSpec
                        let fontRefTok = HR.primTok $ HP.FontRefToken fontNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure (Just boxElem, fontRefTok)
                    oth ->
                        panic $ "Not implemented: DefineControlSequence target " <> show oth
                -- liftIO $ putText $ "Setting CS " <> show cs <> " to token: " <> show newCSTok <> (if global == HP.Global then " globally" else " locally")
                modify $ typed @Config %~ setControlSequence cs newCSTok global
                pure (s, AddMaybeElem maybeElem)
            HP.SetVariable ass ->
                do
                case ass of
                    HP.TeXIntVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.LengthVariableAssignment v tgt  ->
                        Var.setValueFromAST v global tgt
                    HP.GlueVariableAssignment v tgt    ->
                        Var.setValueFromAST v global tgt
                    HP.MathGlueVariableAssignment v tgt  ->
                        Var.setValueFromAST v global tgt
                    HP.TokenListVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.SpecialTeXIntVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.SpecialLengthVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                pure (s, AddMaybeElem Nothing)
            HP.ModifyVariable modCommand ->
                do
                case modCommand of
                    HP.AdvanceTeXIntVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceLengthVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceGlueVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceMathGlueVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.ScaleVariable vDir numVar scaleVal ->
                        case numVar of
                            HP.TeXIntNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                            HP.LengthNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                            HP.GlueNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                            HP.MathGlueNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                pure (s, AddMaybeElem Nothing)
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- texEvaluate idx
                eVal <- texEvaluate val
                -- liftIO $ putText $ "Evaluated code table index " <> show idx <> " to " <> show eIdx
                -- liftIO $ putText $ "Evaluated code table value " <> show val <> " to " <> show eVal
                idxChar <- note (injectTyped $ ConfigError $ "Invalid character code index: " <> show eIdx) (fromTeXInt eIdx)
                -- liftIO $ putText $ "Setting " <> show codeType <> "@" <> show eIdx <> " (" <> show idxChar <> ") to " <> show eVal
                updateCharCodeMap codeType idxChar eVal global
                pure (s, AddMaybeElem Nothing)
            HP.SelectFont fNr ->
                do
                selectFont fNr global
                pure (s, AddMaybeElem $ Just $ BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr)
            HP.SetFamilyMember fm fontRef ->
                do
                eFm <- texEvaluate fm
                fNr <- texEvaluate fontRef
                modify $ typed @Config %~ setFamilyMemberFont eFm fNr global
                pure (s, AddMaybeElem Nothing)
            -- Start a new level of grouping. Enter inner mode.
            HP.SetBoxRegister lhsIdx box ->
                do
                eLhsIdx <- texEvaluate lhsIdx
                case box of
                    HP.FetchedRegisterBox fetchMode rhsIdx ->
                        do
                        fetchedMaybeBox <- fetchBox fetchMode rhsIdx
                        modify $ typed @Config %~ setBoxRegisterNullable eLhsIdx global fetchedMaybeBox
                        pure (s, AddMaybeElem Nothing)
                    HP.LastBox ->
                        panic "Not implemented: SetBoxRegister to LastBox"
                    HP.VSplitBox _ _ ->
                        panic "Not implemented: SetBoxRegister to VSplitBox"
                    HP.ExplicitBox spec boxType ->
                        do
                        eSpec <- texEvaluate spec
                        modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
                        (doneS, boxArg) <- extractExplicitBoxContents s eSpec boxType
                        modify $ typed @Config %~ setBoxRegister eLhsIdx boxArg global
                        pure (doneS, AddMaybeElem Nothing)
            HP.SetFontChar (HP.FontCharRef fontChar fontRef) charRef ->
                do
                fNr <- texEvaluate fontRef
                eCharRef <- texEvaluate charRef
                let updateFontChar f = case fontChar of
                        HP.SkewChar   -> f { skewChar = eCharRef }
                        HP.HyphenChar -> f { hyphenChar = eCharRef }
                modifyFont fNr updateFontChar
                pure (s, AddMaybeElem Nothing)
            oth ->
                panic $ show oth
        (,assignResult) <$> do
            gets (view $ typed @Config . field @"afterAssignmentToken") >>= \case
                Nothing ->
                    pure postAssignS
                Just lt ->
                    do
                    modify $ typed @Config . field @"afterAssignmentToken" .~ Nothing
                    pure (HP.insertLexToken postAssignS lt)
    HP.WriteToStream n (HP.ImmediateWriteText eTxt) -> do
        en <- texEvaluate n
        fStreams <- gets $ view $ typed @Config . field @"outFileStreams"
        let txtTxt = toS $ Codes.unsafeCodesAsChars (showExpandedBalancedText eTxt)
        -- Write to:
        -- if stream number corresponds to existing, open file:
        --     file
        -- otherwise:
        --     log
        --     unless stream number is negative: terminal
        case getFileStream fStreams en of
            Just fStream ->
                 liftIO $ hPutStrLn fStream txtTxt
            Nothing ->
                do
                -- Write to terminal.
                when (en >= 0) $ liftIO $ putText txtTxt
                -- Write to log
                logHandle <- gets $ view $ typed @Config . field @"logStream"
                liftIO $ hPutStrLn logHandle txtTxt
        pure (s, AddMaybeElem Nothing)
    -- Start a new level of grouping.
    HP.ChangeScope HP.Positive trig ->
        do
        modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup trig))
        pure (s, AddMaybeElem Nothing)
    -- Do the appropriate finishing actions, undo the
    -- effects of non-global assignments, and leave the
    -- group. Maybe leave the current mode.
    HP.ChangeScope HP.Negative trig ->
        gets (view $ typed @Config . to popGroup) >>= \case
            Nothing ->
                throwError $ injectTyped $ ConfigError "No group to leave"
            Just (group, poppedConfig) ->
                do
                modify $ typed @Config .~ poppedConfig
                (s,) <$> case group of
                    -- Undo the effects of non-global
                    -- assignments without leaving the
                    -- current mode.
                    ScopeGroup _ (LocalStructureGroup trigConf) ->
                        do
                        when (trigConf /= trig) $ throwError $ injectTyped $ ConfigError $ "Entry and exit group triggers differ: " <> show (trig, trigConf)
                        pure (AddMaybeElem Nothing)
                    -- - Undo the effects of non-global assignments
                    -- - package the [box] using the size that was saved on the
                    --   stack
                    -- - complete the \setbox command
                    -- - return to the mode we were in at the time of the
                    --   \setbox.
                    ScopeGroup _ ExplicitBoxGroup ->
                        pure FinishBoxMode
                    NonScopeGroup ->
                        pure (AddMaybeElem Nothing)
    HP.AddBox HP.NaturalPlacement (HP.FetchedRegisterBox fetchMode idx) -> do
        mayBox <- fetchBox fetchMode idx
        pure (s, AddMaybeElem (BL.VListBaseElem . B.ElemBox <$> mayBox))
    HP.AddBox HP.NaturalPlacement (HP.ExplicitBox spec boxType) -> do
        -- Start a new level of grouping. Enter inner mode.
        eSpec <- texEvaluate spec
        modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
        (doneS, box) <- extractExplicitBoxContents s eSpec boxType
        pure (doneS, AddMaybeElem $ Just $ BL.VListBaseElem $ B.ElemBox box)
    oth ->
        panic $ show oth

extractExplicitBoxContents
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> B.DesiredLength
    -> HP.ExplicitBox
    -> m (s, B.Box B.BoxContents)
extractExplicitBoxContents s desiredLength = \case
    HP.ExplicitHBox ->
        do
        (doneS, finalHList, ()) <- runCommandLoop handleCommandInHBoxMode s mempty
        pure (doneS, B.HBoxContents <$> BL.setHList finalHList (BL.UncomputedTargetLength desiredLength))
    HP.ExplicitVBox vAlignType ->
        do
        (doneS, finalVList, ()) <- runCommandLoop handleCommandInVBoxMode s mempty
        pure (doneS, B.VBoxContents <$> BL.setVList finalVList desiredLength vAlignType)
