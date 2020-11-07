module Hex.Build.Command where

import qualified Data.Path
import qualified Data.ByteString.Char8 as BS.C8
import qualified Hex.Box as B
import Hex.BreakList (HList (..))
import qualified Hex.BreakList as BL
import Hex.Build.Class
import Hex.Build.Helpers
import Hex.Config
import qualified Hex.Config.Codes as Codes
import Hex.Evaluate
import qualified Hex.Lex as Lex
import qualified Hex.Parse as HP
import qualified Hex.Resolve as HR
import qualified Hex.Variable as Var
import Hexlude
import TFM (TFMError)

handleCommandInParaMode ::
  ( MonadError e m,
    AsType TFMError e,
    AsType Data.Path.PathError e,
    TeXBuildCtx st e m,
    MonadModeIndependentBuild m,
    MonadHModeBuild m,
    MonadIO m,
    MonadSlog m
  ) =>
  HP.Command ->
  m (Maybe EndParaReason)
handleCommandInParaMode = \case
  HP.VModeCommand _ -> do
    -- Insert the control sequence "\par" into the input. The control
    -- sequence's current meaning will be used, which might no longer be the \par
    -- primitive.
    -- (Note that we use oldS.)
    revertStream
    insertLexToken Lex.parToken
    pure Nothing
  HP.HModeCommand (HP.AddHGlue g) -> do
    addVElem =<< glueToElem g
    pure Nothing
  HP.HModeCommand (HP.AddCharacter c) -> do
    addHElem =<< hModeCharacterElem c
    pure Nothing
  HP.HModeCommand (HP.AddHRule rule) -> do
    addHElem =<< hModeRuleElem rule
    pure Nothing
  HP.AddSpace -> do
    addHElem =<< hModeSpaceElem
    pure Nothing
  HP.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag >>= \case
      Nothing ->
        pure ()
      Just parIndentElem ->
        addHElem parIndentElem
    pure Nothing
  -- \par: Restricted: does nothing. Unrestricted (this mode): ends mode.
  HP.EndParagraph ->
    pure $ Just EndParaSawEndParaCommand
  HP.ModeIndependentCommand modeIndependentCommand -> do
    sawEndBox <- handleModeIndependentCommand modeIndependentCommand
    pure (if sawEndBox then Just EndParaSawLeaveBox else Nothing)
  oth ->
    panic $ show oth

handleCommandInHBoxMode ::
  ( MonadError e m,
    AsType BuildError e,
    AsType TFMError e,
    AsType Data.Path.PathError e,
    TeXBuildCtx st e m,
    MonadModeIndependentBuild m,
    MonadHModeBuild m,
    MonadIO m,
    MonadSlog m
  ) =>
  HP.Command ->
  m (Maybe ())
handleCommandInHBoxMode = \case
  HP.VModeCommand vModeCommand ->
    throwError $ injectTyped $ BuildError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
  HP.HModeCommand (HP.AddCharacter c) -> do
    addHElem =<< hModeCharacterElem c
    pure Nothing
  HP.HModeCommand (HP.AddHGlue g) -> do
    addHElem . BL.HVListElem =<< glueToElem g
    pure Nothing
  HP.HModeCommand (HP.AddHRule rule) -> do
    addHElem =<< hModeRuleElem rule
    pure Nothing
  HP.AddSpace -> do
    addHElem =<< hModeSpaceElem
    pure Nothing
  HP.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag >>= \case
      Nothing ->
        pure ()
      Just parIndentElem ->
        addHElem parIndentElem
    pure Nothing
  -- \par: Restricted (this mode): does nothing. Unrestricted: ends mode.
  HP.EndParagraph ->
    pure Nothing
  HP.ModeIndependentCommand modeIndependentCommand -> do
    sawEndBox <- handleModeIndependentCommand modeIndependentCommand
    pure (if sawEndBox then Just () else Nothing)
  oth ->
    panic $ "Not implemented, outer V mode: " <> show oth

handleCommandInVBoxMode ::
  forall st e m.
  ( TeXBuildCtx st e m,
    MonadModeIndependentBuild m,
    MonadVModeBuild m,
    AsType TFMError e,
    AsType BuildError e,
    AsType Data.Path.PathError e,
    MonadSlog m,
    MonadIO m
  ) =>
  HP.Command ->
  m (Maybe ())
handleCommandInVBoxMode = \case
  HP.VModeCommand HP.End ->
    throwError $ injectTyped $ BuildError "End not allowed in internal vertical mode"
  HP.VModeCommand (HP.AddVGlue g) -> do
    addVElem =<< glueToElem g
    pure Nothing
  HP.VModeCommand (HP.AddVRule rule) -> do
    addVElem =<< vModeRuleElem rule
    pure Nothing
  HP.HModeCommand _ ->
    addPara HP.Indent
  HP.StartParagraph indentFlag ->
    addPara indentFlag
  -- \par does nothing in vertical mode.
  HP.EndParagraph ->
    pure Nothing
  -- <space token> has no effect in vertical modes.
  HP.AddSpace ->
    pure Nothing
  HP.ModeIndependentCommand modeIndependentCommand -> do
    sawEndBox <- handleModeIndependentCommand modeIndependentCommand
    pure (if sawEndBox then Just () else Nothing)
  oth ->
    panic $ "Not implemented, outer V mode: " <> show oth
  where
    addPara :: HP.IndentFlag -> m (Maybe ())
    addPara indentFlag = do
      -- Note oldS.
      revertStream
      (finalParaHList, endParaReason) <- extractParaList indentFlag
      appendParagraph finalParaHList
      pure $ case endParaReason of
        EndParaSawEndParaCommand ->
          Nothing
        EndParaSawLeaveBox ->
          Just ()

appendParagraph ::
  ( MonadIO m,
    MonadSlog m,
    MonadState st m,
    HasType Config st,
    MonadModeIndependentBuild m,
    MonadError e m,
    AsType BuildError e
  ) =>
  HList ->
  m ()
appendParagraph paraHList = do
  lineBoxes <- hListToParaLineBoxes paraHList
  for_ lineBoxes $ \b ->
    addVElem $ BL.VListBaseElem $ B.ElemBox $ B.HBoxContents <$> b

hListToParaLineBoxes ::
  ( MonadState st m, -- Read-only
    HasType Config st,
    MonadError e m,
    AsType BuildError e,
    MonadSlog m
  ) =>
  HList ->
  m (Seq (B.Box B.HBox))
hListToParaLineBoxes hList =
  do
    hSize <- uses (typed @Config) $ LenParamVal . lookupLengthParameter HP.HSize
    lineTol <- uses (typed @Config) $ IntParamVal . lookupTeXIntParameter HP.Tolerance
    linePen <- uses (typed @Config) $ IntParamVal . lookupTeXIntParameter HP.LinePenalty
    runExceptT @(Identity BL.LineBreakError) (BL.breakAndSetParagraph hSize lineTol linePen hList) >>= \case
      Left err -> throwError $ injectTyped $ BuildError $ show err
      Right v -> pure v

handleCommandInMainVMode ::
  ( MonadError e m,
    AsType BuildError e,
    AsType TFMError e,
    AsType Data.Path.PathError e,
    TeXBuildCtx st e m,
    MonadModeIndependentBuild m,
    MonadVModeBuild m,
    MonadIO m,
    MonadSlog m
  ) =>
  HP.Command ->
  m (Maybe ())
handleCommandInMainVMode = \case
  HP.VModeCommand HP.End ->
    pure (Just ())
  HP.VModeCommand (HP.AddVGlue g) -> do
    glueToElem g >>= addVElem
    pure Nothing
  HP.VModeCommand (HP.AddVRule rule) -> do
    vModeRuleElem rule >>= addVElem
    pure Nothing
  HP.HModeCommand _ -> do
    addPara HP.Indent
    pure Nothing
  HP.StartParagraph indentFlag -> do
    addPara indentFlag
    pure Nothing
  -- \par does nothing in vertical mode.
  HP.EndParagraph ->
    pure Nothing
  -- <space token> has no effect in vertical modes.
  HP.AddSpace ->
    pure Nothing
  HP.ModeIndependentCommand modeIndependentCommand -> do
    sawEndBox <- handleModeIndependentCommand modeIndependentCommand
    when sawEndBox $ throwError $ injectTyped $ BuildError "No box to end: in main V mode"
    pure Nothing
  oth ->
    panic $ "Not implemented, outer V mode: " <> show oth
  where
    addPara indentFlag = do
      -- If the command shifts to horizontal mode, run '\indent', and re-read
      -- the stream as if the command hadn't been read. (Note that we read
      -- from "oldS", not "newS".)
      revertStream
      (paraHList, endParaReason) <- extractParaList indentFlag
      appendParagraph paraHList
      case endParaReason of
        EndParaSawEndParaCommand ->
          pure ()
        EndParaSawLeaveBox ->
          throwError $ injectTyped $
            BuildError "No box to end: in paragraph within main V mode"

handleModeIndependentCommand ::
  ( MonadIO m,
    MonadSlog m,
    AsType Data.Path.PathError e,
    AsType TFMError e,
    TeXBuildCtx st e m,
    MonadModeIndependentBuild m
  ) =>
  HP.ModeIndependentCommand ->
  m Bool
handleModeIndependentCommand = \case
  HP.Message stdOutStream eTxt -> do
    let _handle = case stdOutStream of
          HP.StdOut -> stdout
          HP.StdErr -> stderr
    liftIO $ hPutStrLn _handle (toS (Codes.unsafeCodesAsChars (showExpandedBalancedText eTxt)) :: Text)
    pure False
  HP.Relax ->
    pure False
  HP.IgnoreSpaces ->
    pure False
  -- Re-insert the ⟨token⟩ into the input just after running the next
  -- assignment command. Later \afterassignment commands override earlier
  -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
  -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
  -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
  -- inserted by \everyhbox or \everyvbox.
  HP.SetAfterAssignmentToken lt -> do
    assign' (typed @Config % field @"afterAssignmentToken") (Just lt)
    pure False
  HP.AddPenalty n -> do
    addVElem . BL.ListPenalty . BL.Penalty =<< texEvaluate n
    pure False
  HP.AddKern ln -> do
    addVElem . BL.VListBaseElem . B.ElemKern . B.Kern =<< texEvaluate ln
    pure False
  HP.Assign HP.Assignment {HP.scope, HP.body} ->
    do
      case body of
        HP.DefineControlSequence cs tgt ->
          do
            newCSTok <- case tgt of
              HP.MacroTarget macro ->
                pure (HR.syntaxTok $ HP.MacroTok macro)
              -- TODO: If a \let target is an active character, should we
              -- treat it as a control sequence, or a char-cat pair?
              HP.LetTarget (Lex.CharCatToken tgtCC) ->
                pure (HR.primTok $ HP.LetCharCat tgtCC)
              HP.LetTarget (Lex.ControlSequenceToken tgtCS) ->
                do
                  mayCS <- uses (typed @Config) (lookupCSProper tgtCS)
                  let resTok = fromMaybe (HP.PrimitiveToken HP.RelaxTok) mayCS
                  pure resTok
              HP.ShortDefineTarget q n ->
                do
                  en <- texEvaluate n
                  pure (HR.primTok $ HP.IntRefTok q en)
              HP.FontTarget fontSpec fPath ->
                do
                  fontDef@B.FontDefinition {B.fontNr} <- loadFont fPath fontSpec
                  addVElem $ BL.VListBaseElem $ B.ElemFontDefinition fontDef
                  pure $ HR.primTok $ HP.FontRefToken fontNr
              oth ->
                panic $ "Not implemented: DefineControlSequence target " <> show oth
            sLogStampedJSON
              "Defining control sequence"
              [ ("controlSequence", toJSON cs),
                ("token", toJSON newCSTok),
                ("scope", toJSON scope)
              ]
            modifying' (typed @Config) $ setControlSequence cs newCSTok scope
        HP.SetVariable ass ->
          case ass of
            HP.TeXIntVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            HP.LengthVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            HP.GlueVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            HP.MathGlueVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            HP.TokenListVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            HP.SpecialTeXIntVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            HP.SpecialLengthVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
        HP.ModifyVariable modCommand ->
          case modCommand of
            HP.AdvanceTeXIntVariable var plusVal ->
              Var.advanceValueFromAST var scope plusVal
            HP.AdvanceLengthVariable var plusVal ->
              Var.advanceValueFromAST var scope plusVal
            HP.AdvanceGlueVariable var plusVal ->
              Var.advanceValueFromAST var scope plusVal
            HP.AdvanceMathGlueVariable var plusVal ->
              Var.advanceValueFromAST var scope plusVal
            HP.ScaleVariable vDir numVar scaleVal ->
              case numVar of
                HP.TeXIntNumericVariable var ->
                  Var.scaleValueFromAST var scope vDir scaleVal
                HP.LengthNumericVariable var ->
                  Var.scaleValueFromAST var scope vDir scaleVal
                HP.GlueNumericVariable var ->
                  Var.scaleValueFromAST var scope vDir scaleVal
                HP.MathGlueNumericVariable var ->
                  Var.scaleValueFromAST var scope vDir scaleVal
        HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
          do
            eIdx <- texEvaluate idx
            eVal <- texEvaluate val
            idxChar <- note (injectTyped $ ConfigError $ "Invalid character code index: " <> show eIdx) (fromTeXInt eIdx)
            sLogStampedJSON
              "Doing code assignment"
              [ ("codeTableIndexSymbolic", toJSON idx),
                ("codeTableIndexEvaluated", toJSON eIdx),
                ("codeTableIndexAsChar", toJSON idxChar),
                ("codeTableValueSymbolic", toJSON val),
                ("codeTableValueEvaluated", toJSON eVal),
                ("codeType", toJSON codeType),
                ("scope", toJSON scope)
              ]
            updateCharCodeMap codeType idxChar eVal scope
        HP.SelectFont fNr ->
          do
            selectFont fNr scope
            addVElem $ BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr
        HP.SetFamilyMember fm fontRef ->
          do
            eFm <- texEvaluate fm
            fNr <- texEvaluate fontRef
            modifying' (typed @Config) $ setFamilyMemberFont eFm fNr scope
        -- Start a new level of grouping. Enter inner mode.
        HP.SetBoxRegister lhsIdx box ->
          do
            eLhsIdx <- texEvaluate lhsIdx
            case box of
              HP.FetchedRegisterBox fetchMode rhsIdx ->
                do
                  fetchedMaybeBox <- fetchBox fetchMode rhsIdx
                  modifying' (typed @Config) $ setBoxRegisterNullable eLhsIdx scope fetchedMaybeBox
              HP.LastBox ->
                panic "Not implemented: SetBoxRegister to LastBox"
              HP.VSplitBox _ _ ->
                panic "Not implemented: SetBoxRegister to VSplitBox"
              HP.ExplicitBox spec boxType -> do
                eSpec <- texEvaluate spec
                modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
                extractedBox <- extractExplicitBox eSpec boxType
                modifying' (typed @Config) $ setBoxRegister eLhsIdx extractedBox scope
        HP.SetFontChar (HP.FontCharRef fontChar fontRef) charRef ->
          do
            fNr <- texEvaluate fontRef
            eCharRef <- texEvaluate charRef
            let updateFontChar f = case fontChar of
                  HP.SkewChar -> f {skewChar = eCharRef}
                  HP.HyphenChar -> f {hyphenChar = eCharRef}
            modifyFont fNr updateFontChar
        oth ->
          panic $ show oth
      use (typed @Config % field @"afterAssignmentToken") >>= \case
        Nothing ->
          pure ()
        Just lt ->
          do
            insertLexToken lt
            assign' (typed @Config % field @"afterAssignmentToken") Nothing
      pure False
  HP.WriteToStream n (HP.ImmediateWriteText eTxt) -> do
    en <- texEvaluate n
    fStreams <- use $ typed @Config % field @"outFileStreams"
    let txtTxt = Codes.unsafeCodesAsChars (showExpandedBalancedText eTxt)
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
          when (en >= 0) $ sLog (BS.C8.pack txtTxt)
          -- Write to log
          logHandle <- use $ typed @Config % field @"logStream"
          liftIO $ hPutStrLn logHandle txtTxt
    pure False
  -- Start a new level of grouping.
  HP.ChangeScope HP.Positive entryTrig ->
    do
      modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup entryTrig))
      pure False
  -- Do the appropriate finishing actions, undo the
  -- effects of non-global assignments, and leave the
  -- group. Maybe leave the current mode.
  HP.ChangeScope HP.Negative exitTrig -> do
    prePopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
    (group, poppedConfig) <- uses (typed @Config) popGroup >>= \case
      Nothing ->
        throwError $ injectTyped $ ConfigError "No group to leave"
      Just v ->
        pure v
    assign' (typed @Config) poppedConfig
    postPopCurrentFontNr <- uses (typed @Config) lookupCurrentFontNr
    when (prePopCurrentFontNr /= postPopCurrentFontNr) $ do
      sLogStampedJSON
        "After exiting scope, reverting changed font"
        [ ("fontNrPrePop", toJSON prePopCurrentFontNr),
          ("fontNrPostPop", toJSON postPopCurrentFontNr),
          ("groupType", toJSON (renderGroupType group))
        ]
      addVElem $ BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection (fromMaybe 0 postPopCurrentFontNr)
    case group of
      -- Undo the effects of non-global
      -- assignments without leaving the
      -- current mode.
      ScopeGroup _ (LocalStructureGroup entryTrig) -> do
        when (entryTrig /= exitTrig)
          $ throwError
          $ injectTyped
          $ ConfigError
          $ "Entry and exit group triggers differ: " <> show (exitTrig, entryTrig)
        pure False
      -- - Undo the effects of non-global assignments
      -- - package the [box] using the size that was saved on the
      --   stack
      -- - complete the \setbox command
      -- - return to the mode we were in at the time of the
      --   \setbox.
      ScopeGroup _ ExplicitBoxGroup ->
        pure True
      NonScopeGroup ->
        pure False
  HP.AddBox HP.NaturalPlacement boxSource -> do
    case boxSource of
      HP.FetchedRegisterBox fetchMode idx ->
        fetchBox fetchMode idx >>= \case
          Nothing ->
            pure ()
          Just b ->
            addVElem $ BL.VListBaseElem $ B.ElemBox b
      HP.ExplicitBox spec boxType -> do
        -- Start a new level of grouping. Enter inner mode.
        eSpec <- texEvaluate spec
        modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
        b <- extractExplicitBox eSpec boxType
        addVElem $ BL.VListBaseElem $ B.ElemBox b
    pure False
  oth ->
    panic $ show oth
