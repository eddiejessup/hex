module Hex.Command.ModeIndependent where

import           Hexlude

import qualified Data.Path            as D.Path

import           TFM                  (TFMError)

import qualified Hex.Box              as B
import qualified Hex.BreakList        as BL
import qualified Hex.Command.Commands as Com
import qualified Hex.Config.Codes     as Code
import           Hex.Command.Common
import           Hex.Config
import           Hex.Evaluate
import qualified Hex.Lex              as Lex
import qualified Hex.Parse            as HP
import qualified Hex.Resolve          as HR
import qualified Hex.Variable         as Var

data ModeIndependentResult
    = AddElem BL.VListElem
    | EnterBoxMode B.DesiredLength HP.ExplicitBox BoxModeIntent
    | FinishBoxMode
    | DoNothing

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
fetchBox fetchMode idx =
    do
    eIdx <- readOnState $ texEvaluate idx
    fetchedMaybeBox <- gets $ view $ typed @Config . to (lookupBoxRegister eIdx)
    case fetchMode of
        HP.Lookup -> pure ()
        HP.Pop    -> modify $ typed @Config %~ (delBoxRegister eIdx HP.Local)
    pure fetchedMaybeBox

handleModeIndependentCommand
    :: ( HP.TeXStream (HP.Tgt st)
       , MonadState st m
       , HP.HasTgtType st
       , HasType Config st

       , MonadIO m
       , MonadError e m
       , AsType ConfigError e
       , AsType EvaluationError e
       , AsType D.Path.PathError e
       , AsType TFMError e
       )
    => HP.ModeIndependentCommand
    -> m ModeIndependentResult
handleModeIndependentCommand = \case
    HP.Message stdOutStream eTxt ->
        do
        let _handle = case stdOutStream of
                HP.StdOut -> stdout
                HP.StdErr -> stderr
        liftIO $ hPutStrLn _handle (toS (Code.unsafeCodesAsChars (Com.showExpandedBalancedText eTxt)) :: Text)
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
        modify $ typed @Config . field @"afterAssignmentToken" .~ Just lt
        pure DoNothing
    HP.AddPenalty n ->
        AddElem . BL.ListPenalty . BL.Penalty <$> readOnState (texEvaluate n)
    HP.AddKern ln ->
        AddElem . BL.VListBaseElem . B.ElemKern . B.Kern <$> readOnState (texEvaluate ln)
    HP.Assign HP.Assignment { HP.global, HP.body } ->
        do
        assignResult <- case body of
            HP.DefineControlSequence cs tgt ->
                do
                (maybeElem, newCSTok) <- readOnState $ case tgt of
                    HP.MacroTarget macro ->
                        pure (Nothing, HR.syntaxTok $ HP.MacroTok macro)
                    -- TODO: If a \let target is an active character, should we
                    -- treat it as a control sequence, or a char-cat pair?
                    HP.LetTarget (Lex.CharCatToken tgtCC) ->
                        pure (Nothing, HR.primTok $ HP.LetCharCat tgtCC)
                    HP.LetTarget (Lex.ControlSequenceToken tgtCS) ->
                        do
                        mayCS <- asks (view $ typed @Config . to (lookupCSProper tgtCS))
                        let resTok = fromMaybe (HP.PrimitiveToken HP.RelaxTok) mayCS
                        pure (Nothing, resTok)
                    HP.ShortDefineTarget q n ->
                        do
                        en <- texEvaluate n
                        pure (Nothing, HR.primTok $ HP.IntRefTok q en)
                    HP.FontTarget fontSpec fPath ->
                        do
                        fontDef@B.FontDefinition { B.fontNr } <- Com.loadFont fPath fontSpec
                        let fontRefTok = HR.primTok $ HP.FontRefToken fontNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure (Just boxElem, fontRefTok)
                    oth ->
                        panic $ "Not implemented: DefineControlSequence target " <> show oth
                -- liftIO $ putText $ "Setting CS " <> show cs <> " to token: " <> show newCSTok <> (if global == HP.Global then " globally" else " locally")
                modify $ typed @Config %~ setControlSequence cs newCSTok global
                pure $ maybe DoNothing AddElem maybeElem
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
                pure DoNothing
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
                pure DoNothing
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- readOnState (texEvaluate idx)
                eVal <- readOnState (texEvaluate val)
                -- liftIO $ putText $ "Evaluated code table index " <> show idx <> " to " <> show eIdx
                -- liftIO $ putText $ "Evaluated code table value " <> show val <> " to " <> show eVal
                idxChar <- note (injectTyped $ ConfigError $ "Invalid character code index: " <> show eIdx) (fromTeXInt eIdx)
                -- liftIO $ putText $ "Setting " <> show codeType <> "@" <> show eIdx <> " (" <> show idxChar <> ") to " <> show eVal
                updateCharCodeMap codeType idxChar eVal global
                pure DoNothing
            HP.SelectFont fNr ->
                do
                Com.selectFont fNr global
                pure $ AddElem $ BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr
            HP.SetFamilyMember fm fontRef ->
                do
                eFm <- readOnState (texEvaluate fm)
                fNr <- readOnState (texEvaluate fontRef)
                modify $ typed @Config %~ setFamilyMemberFont eFm fNr global
                pure DoNothing
            -- Start a new level of grouping. Enter inner mode.
            HP.SetBoxRegister lhsIdx box ->
                do
                eLhsIdx <- readOnState (texEvaluate lhsIdx)
                case box of
                    HP.FetchedRegisterBox fetchMode rhsIdx ->
                        do
                        fetchedMaybeBox <- fetchBox fetchMode rhsIdx
                        modify $ typed @Config %~ setBoxRegisterNullable eLhsIdx global fetchedMaybeBox
                        pure DoNothing
                    HP.LastBox ->
                        panic "Not implemented: SetBoxRegister to LastBox"
                    HP.VSplitBox _ _ ->
                        panic "Not implemented: SetBoxRegister to VSplitBox"
                    HP.ExplicitBox spec boxType ->
                        do
                        eSpec <- readOnState (texEvaluate spec)
                        modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
                        pure $ EnterBoxMode eSpec boxType (IntentToSetBoxRegister eLhsIdx global)
            HP.SetFontChar (HP.FontCharRef fontChar fontRef) charRef ->
                do
                fNr <- readOnState (texEvaluate fontRef)
                eCharRef <- readOnState (texEvaluate charRef)
                let updateFontChar f = case fontChar of
                        HP.SkewChar   -> f { skewChar = eCharRef }
                        HP.HyphenChar -> f { hyphenChar = eCharRef }
                modifyFont fNr updateFontChar
                pure DoNothing
            oth ->
                panic $ show oth
        (gets $ view $ typed @Config . field @"afterAssignmentToken") >>= \case
            Nothing -> pure ()
            Just lt ->
                do
                modify $ HP.tgtLens %~ (\s -> HP.insertLexToken s lt)
                modify $ typed @Config . field @"afterAssignmentToken" .~ Nothing
        pure assignResult
    HP.WriteToStream n (HP.ImmediateWriteText eTxt) ->
        readOnState $ do
            en <- texEvaluate n
            fStreams <- gets $ view $ typed @Config . field @"outFileStreams"
            let txtTxt = toS $ Code.unsafeCodesAsChars (Com.showExpandedBalancedText eTxt)
            -- Write to:
            -- if stream number corresponds to existing, open file:
            --     file
            -- otherwise:
            --     log
            --     unless stream number is negative: terminal
            case Com.getFileStream fStreams en of
                Just fStream ->
                     liftIO $ hPutStrLn fStream txtTxt
                Nothing ->
                    do
                    -- Write to terminal.
                    when (en >= 0) $ liftIO $ putText txtTxt
                    -- Write to log
                    logHandle <- gets $ view $ typed @Config . field @"logStream"
                    liftIO $ hPutStrLn logHandle txtTxt
            pure DoNothing
    -- Start a new level of grouping.
    HP.ChangeScope HP.Positive trig ->
        do
        modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup trig))
        pure DoNothing
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
                case group of
                    -- Undo the effects of non-global
                    -- assignments without leaving the
                    -- current mode.
                    ScopeGroup _ (LocalStructureGroup trigConf) ->
                        do
                        when (trigConf /= trig) $ throwError $ injectTyped $ ConfigError $ "Entry and exit group triggers differ: " <> show (trig, trigConf)
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
        eSpec <- readOnState (texEvaluate spec)
        modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
        pure $ EnterBoxMode eSpec boxType IntentToAddBox
    oth ->
        panic $ show oth
  where
    addMaybeElem' = \case
        Nothing -> DoNothing
        Just e -> AddElem e
