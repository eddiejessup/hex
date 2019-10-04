module HeX.Command.ModeIndependent where

import           HeXlude

import           Control.Monad        (when)
import qualified Data.Path             as D.Path
import           Safe                 (toEnumMay)

import           TFM                  (TFMError)

import qualified HeX.Box              as B
import qualified HeX.BreakList        as BL
import qualified HeX.Command.Commands as Com
import           HeX.Command.Common
import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Lex              as Lex
import qualified HeX.Parse            as HP
import qualified HeX.Variable         as Var

data ModeIndependentResult
    = AddElem BL.VListElem
    | EnterBoxMode B.DesiredLength HP.ExplicitBox BoxModeIntent
    | FinishBoxMode
    | DoNothing

fetchBox
    :: ( HP.TeXStream s
       , MonadState s m
       , MonadErrorAnyOf e m
            '[ ConfigError
             , EvaluationError
             ]
       )
    => HP.BoxFetchMode
    -> HP.EightBitTeXInt
    -> m (Maybe (B.Box B.BoxContents))
fetchBox fetchMode idx =
    do
    eIdx <- evalOnConfState idx
    fetchedMaybeBox <- readOnConfState $ asks $ lookupBoxRegister eIdx
    case fetchMode of
        HP.Lookup -> pure ()
        HP.Pop    -> HP.runConfState $ modify $ delBoxRegister eIdx HP.Local
    pure fetchedMaybeBox

handleModeIndependentCommand
    :: ( HP.TeXStream s
       , MonadState s m
       , MonadIO m
       , MonadErrorAnyOf e m
           '[ ConfigError
            , EvaluationError
            , D.Path.PathError
            , TFMError
            ]
       )
    => HP.ModeIndependentCommand
    -> m ModeIndependentResult
handleModeIndependentCommand = \case
    HP.Message stdOutStream eTxt ->
        do
        let _handle = case stdOutStream of
                HP.StdOut -> stdout
                HP.StdErr -> stderr
        liftIO $ hPutStrLn _handle $ Com.showExpandedBalancedText eTxt
        pure DoNothing
    HP.Relax ->
        pure DoNothing
    HP.IgnoreSpaces ->
        pure DoNothing
    -- Re-insert the ⟨token⟩ into the input just after running the next
    -- assignment command. Later \afterassignment commands override earlier
    -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
    -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
    -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
    -- inserted by \everyhbox or \everyvbox.
    HP.SetAfterAssignmentToken lt ->
        do
        modConfState $ \conf -> conf{ afterAssignmentToken = Just lt }
        pure DoNothing
    HP.AddPenalty n ->
        AddElem . BL.ListPenalty . BL.Penalty <$> evalOnConfState n
    HP.AddKern ln ->
        AddElem . BL.VListBaseElem . B.ElemKern . B.Kern <$> evalOnConfState ln
    HP.Assign HP.Assignment { HP.global, HP.body } ->
        do
        assignResult <- case body of
            HP.DefineControlSequence cs tgt ->
                do
                (maybeElem, newCSTok) <- readOnConfState $ case tgt of
                    HP.MacroTarget macro ->
                        pure (Nothing, HP.syntaxTok $ HP.MacroTok macro)
                    -- TODO: If a \let target is an active character, should we
                    -- treat it as a control sequence, or a char-cat pair?
                    HP.LetTarget (Lex.CharCatToken tgtCC) ->
                        pure (Nothing, HP.primTok $ HP.LetCharCat tgtCC)
                    HP.LetTarget (Lex.ControlSequenceToken tgtCS) ->
                        do
                        resTok <- fromMaybe (HP.PrimitiveToken HP.RelaxTok) <$> asks (lookupCSProper tgtCS)
                        pure (Nothing, resTok)
                    HP.ShortDefineTarget q n ->
                        do
                        en <- texEvaluate n
                        pure (Nothing, HP.primTok $ HP.IntRefTok q en)
                    HP.FontTarget fontSpec fPath ->
                        do
                        fontDef@B.FontDefinition { B.fontNr } <- Com.loadFont fPath fontSpec
                        let fontRefTok = HP.primTok $ HP.FontRefToken fontNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure (Just boxElem, fontRefTok)
                    oth ->
                        panic $ "Not implemented: DefineControlSequence target " <> show oth
                liftIO $ putText $ "Setting CS " <> show cs <> " to token: " <> show newCSTok <> (if global == HP.Global then " globally" else " locally")
                modConfState $ setControlSequence cs newCSTok global
                pure $ maybe DoNothing AddElem maybeElem
            HP.SetVariable ass ->
                do
                HP.runConfState $ case ass of
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
                pure DoNothing
            HP.ModifyVariable modCommand ->
                do
                HP.runConfState $ case modCommand of
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
                pure DoNothing
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- evalOnConfState idx
                eVal <- evalOnConfState val
                liftIO $ putText $ "Evaluated code table index " <> show idx <> " to " <> show eIdx
                liftIO $ putText $ "Evaluated code table value " <> show val <> " to " <> show eVal
                idxChar <- liftMaybe (throw $ ConfigError $ "Invalid character code index: " <> show eIdx) (toEnumMay eIdx)
                liftIO $ putText $ "Setting " <> show codeType <> "@" <> show eIdx <> " (" <> show idxChar <> ") to " <> show eVal
                HP.runConfState $ updateCharCodeMap codeType idxChar eVal global
                pure DoNothing
            HP.SelectFont fNr ->
                do
                HP.runConfState $ Com.selectFont fNr global
                pure $ AddElem $ BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr
            HP.SetFamilyMember fm fontRef ->
                do
                eFm <- evalOnConfState fm
                fNr <- evalOnConfState fontRef
                modConfState $ setFamilyMemberFont eFm fNr global
                pure DoNothing
            -- Start a new level of grouping. Enter inner mode.
            HP.SetBoxRegister lhsIdx box ->
                do
                eLhsIdx <- evalOnConfState lhsIdx
                case box of
                    HP.FetchedRegisterBox fetchMode rhsIdx ->
                        do
                        fetchedMaybeBox <- fetchBox fetchMode rhsIdx
                        modConfState $ setBoxRegisterNullable eLhsIdx global fetchedMaybeBox
                        pure DoNothing
                    HP.LastBox ->
                        panic "Not implemented: SetBoxRegister to LastBox"
                    HP.VSplitBox _ _ ->
                        panic "Not implemented: SetBoxRegister to VSplitBox"
                    HP.ExplicitBox spec boxType ->
                        do
                        eSpec <- evalOnConfState spec
                        modConfState $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
                        pure $ EnterBoxMode eSpec boxType (IntentToSetBoxRegister eLhsIdx global)
            HP.SetFontChar (HP.FontCharRef fontChar fontRef) charRef ->
                do
                fNr <- evalOnConfState fontRef
                eCharRef <- evalOnConfState charRef
                let updateFontChar f = case fontChar of
                        HP.SkewChar   -> f { skewChar = eCharRef }
                        HP.HyphenChar -> f { hyphenChar = eCharRef }
                HP.runConfState $ modifyFont fNr updateFontChar
                pure DoNothing
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
        readOnConfState $
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
            pure DoNothing
    -- Start a new level of grouping.
    HP.ChangeScope HP.Positive trig ->
        do
        modConfState $ pushGroup $ ScopeGroup newLocalScope (LocalStructureGroup trig)
        pure DoNothing
    -- Do the appropriate finishing actions, undo the
    -- effects of non-global assignments, and leave the
    -- group. Maybe leave the current mode.
    HP.ChangeScope HP.Negative trig ->
        HP.runConfState $ gets popGroup >>= \case
            Nothing ->
                throwM $ ConfigError "No group to leave"
            Just (group, poppedConfig) ->
                do
                put poppedConfig
                case group of
                    -- Undo the effects of non-global
                    -- assignments without leaving the
                    -- current mode.
                    ScopeGroup _ (LocalStructureGroup trigConf) ->
                        do
                        when (trigConf /= trig) $ throwM $ ConfigError $ "Entry and exit group triggers differ: " <> show (trig, trigConf)
                        pure DoNothing
                    -- - Undo the effects of non-global assignments
                    -- - package the [box] using the size that was saved on the
                    --   stack
                    -- - complete the \setbox command
                    -- - return to the mode we were in at the time of the
                    --   \setbox.
                    ScopeGroup _ ExplicitBoxGroup ->
                        pure FinishBoxMode
                    NonScopeGroup ->
                        pure DoNothing
    HP.AddBox HP.NaturalPlacement (HP.FetchedRegisterBox fetchMode idx) ->
        do
        maybeBox <- fetchBox fetchMode idx
        pure $ addMaybeElem' $ BL.VListBaseElem . B.ElemBox <$> maybeBox
    HP.AddBox HP.NaturalPlacement (HP.ExplicitBox spec boxType) ->
        -- Start a new level of grouping. Enter inner mode.
        do
        eSpec <- evalOnConfState spec
        modConfState $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
        pure $ EnterBoxMode eSpec boxType IntentToAddBox
    oth ->
        panic $ show oth
  where
    addMaybeElem' = \case
        Nothing -> DoNothing
        Just e -> AddElem e
