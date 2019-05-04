module HeX.Command.ModeIndependent where

import HeXlude

import           Control.Monad                  ( when )
import           Safe                           ( toEnumMay )

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Command.Common
import           HeX.Config
import qualified HeX.Lex                       as Lex
import           HeX.Evaluate
import qualified HeX.Parse                     as HP
import qualified HeX.Command.Commands          as Com
import qualified HeX.Variable                  as Var

data ModeIndependentResult
    = AddElems [BL.BreakableVListElem]
    | EnterBoxMode B.DesiredLength HP.ExplicitBox BoxModeIntent
    | FinishBoxMode

fetchBox
    :: HP.InhibitableStream s
    => HP.BoxFetchMode
    -> HP.EightBitNumber
    -> ExceptMonadBuild s (Maybe B.Box)
fetchBox fetchMode idx =
    do
    eIdx <- liftEvalOnConfState idx
    fetchedMaybeBox <- readOnConfState $ asks $ lookupBoxRegister eIdx
    case fetchMode of
        HP.Lookup -> pure ()
        HP.Pop -> HP.runConfState $ modify $ delBoxRegister eIdx HP.Local
    pure fetchedMaybeBox

handleModeIndependentCommand
    :: HP.InhibitableStream s
    => HP.ModeIndependentCommand -> ExceptMonadBuild s ModeIndependentResult
handleModeIndependentCommand = \case
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
    HP.Assign HP.Assignment { HP.global, HP.body } ->
        do
        assignResult <- case body of
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
                pure $ AddElems acc
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
                noElems
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
                            HP.MathGlueNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                noElems
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- liftEvalOnConfState idx
                eVal <- liftEvalOnConfState val
                liftIO $ putText $ "Evaluated code table index " <> showT idx <> " to " <> showT eIdx
                liftIO $ putText $ "Evaluated code table value " <> showT val <> " to " <> showT eVal
                idxChar <- liftMaybeConfigError ("Invalid character code index: " <> showT eIdx) (toEnumMay eIdx)
                liftIO $ putText $ "Setting " <> showT codeType <> "@" <> showT eIdx <> " (" <> showT idxChar <> ") to " <> showT eVal
                liftConfState $ updateCharCodeMap codeType idxChar eVal global
                noElems
            HP.SelectFont fNr ->
                do
                HP.runConfState $ Com.selectFont fNr global
                pure $ AddElems [BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr]
            HP.SetFamilyMember fm fontRef ->
                do
                eFm <- liftEvalOnConfState fm
                fNr <- liftEvalOnConfState fontRef
                modConfState $ setFamilyMemberFont eFm fNr global
                noElems
            -- Start a new level of grouping. Enter inner mode.
            HP.SetBoxRegister lhsIdx box ->
                do
                eLhsIdx <- liftEvalOnConfState lhsIdx
                case box of
                    HP.FetchedRegisterBox fetchMode rhsIdx ->
                        do
                        fetchedMaybeBox <- fetchBox fetchMode rhsIdx
                        modConfState $ setBoxRegisterNullable eLhsIdx global fetchedMaybeBox
                        noElems
                    HP.LastBox ->
                        notImplemented
                    HP.VSplitBox _ _ ->
                        notImplemented
                    HP.ExplicitBox spec boxType ->
                        do
                        eSpec <- liftEvalOnConfState spec
                        modConfState $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
                        pure $ EnterBoxMode eSpec boxType (IntentToSetBoxRegister eLhsIdx global)
            HP.SetFontChar (HP.FontCharRef fontChar fontRef) charRef ->
                do
                fNr <- liftEvalOnConfState fontRef
                eCharRef <- liftEvalOnConfState charRef
                let updateFontChar f = case fontChar of
                        HP.SkewChar -> f { skewChar = eCharRef }
                        HP.HyphenChar -> f { hyphenChar = eCharRef }
                HP.runConfState $ modifyFont fNr updateFontChar
                noElems
            oth ->
                panic $ show oth
        HP.runConfState (gets afterAssignmentToken) >>= \case
            Nothing -> pure ()
            Just lt ->
                do
                modify $ \s -> HP.insertLexToken s lt
                modConfState $ \c -> c{afterAssignmentToken = Nothing}
        pure assignResult
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
                    -- - Undo the effects of non-global assignments
                    -- - package the [box] using the size that was saved on the
                    --   stack
                    -- - complete the \setbox command
                    -- - return to the mode we were in at the time of the
                    --   \setbox.
                    ScopeGroup _ ExplicitBoxGroup ->
                        pure FinishBoxMode
                    NonScopeGroup ->
                        pure $ AddElems []
    HP.AddBox HP.NaturalPlacement (HP.FetchedRegisterBox fetchMode idx) ->
        (AddElems . maybeBoxToElems) <$> fetchBox fetchMode idx
      where
        maybeBoxToElems = \case
            Nothing -> []
            Just b -> [BL.VListBaseElem $ B.ElemBox b]
    HP.AddBox HP.NaturalPlacement (HP.ExplicitBox spec boxType) ->
        -- Start a new level of grouping. Enter inner mode.
        do
        eSpec <- liftEvalOnConfState spec
        modConfState $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
        pure $ EnterBoxMode eSpec boxType IntentToAddBox
    oth ->
        panic $ show oth

  where
    noElems = pure $ AddElems []
