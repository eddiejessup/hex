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
import qualified Hex.Resolve.Token as Tok
import qualified Hex.Parse.AST as AST
import qualified Hex.Variable as Var
import Hexlude
import TFM (TFMError)

handleCommandInParaMode ::
  ( MonadError e m,
    AsType TFMError e,
    AsType ConfigError e,
    MonadEvaluate m AST.CharCodeRef,
    MonadEvaluate m AST.TeXInt,
    MonadEvaluate m AST.Length,
    MonadEvaluate m AST.Glue,
    MonadEvaluate m AST.MathGlue,
    MonadEvaluate m AST.TokenListAssignmentTarget,
    MonadEvaluate m AST.EightBitTeXInt,
    MonadEvaluate m AST.TeXIntVariable,
    MonadEvaluate m AST.LengthVariable,
    MonadEvaluate m AST.GlueVariable,
    MonadEvaluate m AST.MathGlueVariable,
    MonadEvaluate m AST.FamilyMember,
    MonadEvaluate m AST.FontRef,
    MonadEvaluate m AST.BoxSpecification,
    AsType Data.Path.PathError e,
    TeXBuildCtx st e m,
    MonadModeIndependentBuild m,
    MonadHModeBuild m,
    MonadIO m,
    MonadSlog m
  ) =>
  AST.Command ->
  m (Maybe EndParaReason)
handleCommandInParaMode = \case
  AST.VModeCommand _ -> do
    -- Insert the control sequence "\par" into the input. The control
    -- sequence's current meaning will be used, which might no longer be the \par
    -- primitive.
    -- (Note that we use oldS.)
    revertStream
    insertLexToken Lex.parToken
    pure Nothing
  AST.HModeCommand (AST.AddHGlue g) -> do
    addVElem =<< glueToElem g
    pure Nothing
  AST.HModeCommand (AST.AddCharacter c) -> do
    addHElem =<< hModeCharacterElem c
    pure Nothing
  AST.HModeCommand (AST.AddHRule rule) -> do
    addHElem =<< hModeRuleElem rule
    pure Nothing
  AST.AddSpace -> do
    addHElem =<< hModeSpaceElem
    pure Nothing
  AST.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag >>= \case
      Nothing ->
        pure ()
      Just parIndentElem ->
        addHElem parIndentElem
    pure Nothing
  -- \par: Restricted: does nothing. Unrestricted (this mode): ends mode.
  AST.EndParagraph ->
    pure $ Just EndParaSawEndParaCommand
  AST.ModeIndependentCommand modeIndependentCommand -> do
    sawEndBox <- handleModeIndependentCommand modeIndependentCommand
    pure (if sawEndBox then Just EndParaSawLeaveBox else Nothing)
  oth ->
    panic $ show oth

handleCommandInHBoxMode ::
  ( MonadError e m,
    AsType BuildError e,
    AsType TFMError e,
    AsType Data.Path.PathError e,
    AsType ConfigError e,
    MonadEvaluate m AST.CharCodeRef,
    MonadEvaluate m AST.TeXInt,
    MonadEvaluate m AST.Length,
    MonadEvaluate m AST.Glue,
    MonadEvaluate m AST.MathGlue,
    MonadEvaluate m AST.TokenListAssignmentTarget,
    MonadEvaluate m AST.EightBitTeXInt,
    MonadEvaluate m AST.TeXIntVariable,
    MonadEvaluate m AST.LengthVariable,
    MonadEvaluate m AST.GlueVariable,
    MonadEvaluate m AST.MathGlueVariable,
    MonadEvaluate m AST.FamilyMember,
    MonadEvaluate m AST.FontRef,
    MonadEvaluate m AST.BoxSpecification,
    TeXBuildCtx st e m,
    MonadModeIndependentBuild m,
    MonadHModeBuild m,
    MonadIO m,
    MonadSlog m
  ) =>
  AST.Command ->
  m (Maybe ())
handleCommandInHBoxMode = \case
  AST.VModeCommand vModeCommand ->
    throwError $ injectTyped $ BuildError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
  AST.HModeCommand (AST.AddCharacter c) -> do
    addHElem =<< hModeCharacterElem c
    pure Nothing
  AST.HModeCommand (AST.AddHGlue g) -> do
    addHElem . BL.HVListElem =<< glueToElem g
    pure Nothing
  AST.HModeCommand (AST.AddHRule rule) -> do
    addHElem =<< hModeRuleElem rule
    pure Nothing
  AST.AddSpace -> do
    addHElem =<< hModeSpaceElem
    pure Nothing
  AST.StartParagraph indentFlag -> do
    hModeStartParagraph indentFlag >>= \case
      Nothing ->
        pure ()
      Just parIndentElem ->
        addHElem parIndentElem
    pure Nothing
  -- \par: Restricted (this mode): does nothing. Unrestricted: ends mode.
  AST.EndParagraph ->
    pure Nothing
  AST.ModeIndependentCommand modeIndependentCommand -> do
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
    AsType ConfigError e,
    MonadEvaluate m AST.TeXInt,
    MonadEvaluate m AST.Length,
    MonadEvaluate m AST.Glue,
    MonadEvaluate m AST.MathGlue,
    MonadEvaluate m AST.TokenListAssignmentTarget,
    MonadEvaluate m AST.EightBitTeXInt,
    MonadEvaluate m AST.TeXIntVariable,
    MonadEvaluate m AST.LengthVariable,
    MonadEvaluate m AST.GlueVariable,
    MonadEvaluate m AST.MathGlueVariable,
    MonadEvaluate m AST.FamilyMember,
    MonadEvaluate m AST.FontRef,
    MonadEvaluate m AST.BoxSpecification,
    MonadSlog m,
    MonadIO m
  ) =>
  AST.Command ->
  m (Maybe ())
handleCommandInVBoxMode = \case
  AST.VModeCommand AST.End ->
    throwError $ injectTyped $ BuildError "End not allowed in internal vertical mode"
  AST.VModeCommand (AST.AddVGlue g) -> do
    addVElem =<< glueToElem g
    pure Nothing
  AST.VModeCommand (AST.AddVRule rule) -> do
    addVElem =<< vModeRuleElem rule
    pure Nothing
  AST.HModeCommand _ ->
    addPara Tok.Indent
  AST.StartParagraph indentFlag ->
    addPara indentFlag
  -- \par does nothing in vertical mode.
  AST.EndParagraph ->
    pure Nothing
  -- <space token> has no effect in vertical modes.
  AST.AddSpace ->
    pure Nothing
  AST.ModeIndependentCommand modeIndependentCommand -> do
    sawEndBox <- handleModeIndependentCommand modeIndependentCommand
    pure (if sawEndBox then Just () else Nothing)
  oth ->
    panic $ "Not implemented, outer V mode: " <> show oth
  where
    addPara :: Tok.IndentFlag -> m (Maybe ())
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
    hSize <- uses (typed @Config) $ LenParamVal . lookupLengthParameter Tok.HSize
    lineTol <- uses (typed @Config) $ IntParamVal . lookupTeXIntParameter Tok.Tolerance
    linePen <- uses (typed @Config) $ IntParamVal . lookupTeXIntParameter Tok.LinePenalty
    runExceptT @(Identity BL.LineBreakError) (BL.breakAndSetParagraph hSize lineTol linePen hList) >>= \case
      Left err -> throwError $ injectTyped $ BuildError $ show err
      Right v -> pure v

handleCommandInMainVMode ::
  ( MonadError e m,
    AsType BuildError e,
    AsType TFMError e,
    AsType Data.Path.PathError e,
    AsType ConfigError e,
    MonadEvaluate m AST.TeXInt,
    MonadEvaluate m AST.Length,
    MonadEvaluate m AST.Glue,
    MonadEvaluate m AST.MathGlue,
    MonadEvaluate m AST.TokenListAssignmentTarget,
    MonadEvaluate m AST.EightBitTeXInt,
    MonadEvaluate m AST.TeXIntVariable,
    MonadEvaluate m AST.LengthVariable,
    MonadEvaluate m AST.GlueVariable,
    MonadEvaluate m AST.MathGlueVariable,
    MonadEvaluate m AST.FamilyMember,
    MonadEvaluate m AST.FontRef,
    MonadEvaluate m AST.BoxSpecification,
    TeXBuildCtx st e m,
    MonadModeIndependentBuild m,
    MonadVModeBuild m,
    MonadIO m,
    MonadSlog m
  ) =>
  AST.Command ->
  m (Maybe ())
handleCommandInMainVMode = \case
  AST.VModeCommand AST.End ->
    pure (Just ())
  AST.VModeCommand (AST.AddVGlue g) -> do
    glueToElem g >>= addVElem
    pure Nothing
  AST.VModeCommand (AST.AddVRule rule) -> do
    vModeRuleElem rule >>= addVElem
    pure Nothing
  AST.HModeCommand _ -> do
    addPara Tok.Indent
    pure Nothing
  AST.StartParagraph indentFlag -> do
    addPara indentFlag
    pure Nothing
  -- \par does nothing in vertical mode.
  AST.EndParagraph ->
    pure Nothing
  -- <space token> has no effect in vertical modes.
  AST.AddSpace ->
    pure Nothing
  AST.ModeIndependentCommand modeIndependentCommand -> do
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
    AsType ConfigError e,
    MonadEvaluate m AST.TeXInt,
    MonadEvaluate m AST.Length,
    MonadEvaluate m AST.Glue,
    MonadEvaluate m AST.MathGlue,
    MonadEvaluate m AST.TokenListAssignmentTarget,
    MonadEvaluate m AST.EightBitTeXInt,
    MonadEvaluate m AST.TeXIntVariable,
    MonadEvaluate m AST.LengthVariable,
    MonadEvaluate m AST.GlueVariable,
    MonadEvaluate m AST.MathGlueVariable,
    MonadEvaluate m AST.FamilyMember,
    MonadEvaluate m AST.FontRef,
    MonadEvaluate m AST.BoxSpecification,
    TeXBuildCtx st e m,
    MonadModeIndependentBuild m
  ) =>
  AST.ModeIndependentCommand ->
  m Bool
handleModeIndependentCommand = \case
  AST.Message stdOutStream eTxt -> do
    let _handle = case stdOutStream of
          Tok.StdOut -> stdout
          Tok.StdErr -> stderr
    liftIO $ hPutStrLn _handle (toS (Codes.unsafeCodesAsChars (showExpandedBalancedText eTxt)) :: Text)
    pure False
  AST.Relax ->
    pure False
  AST.IgnoreSpaces ->
    pure False
  -- Re-insert the ⟨token⟩ into the input just after running the next
  -- assignment command. Later \afterassignment commands override earlier
  -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
  -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
  -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
  -- inserted by \everyhbox or \everyvbox.
  AST.SetAfterAssignmentToken lt -> do
    assign' (typed @Config % field @"afterAssignmentToken") (Just lt)
    pure False
  AST.AddPenalty n -> do
    addVElem . BL.ListPenalty . BL.Penalty =<< astEval n
    pure False
  AST.AddKern ln -> do
    addVElem . BL.VListBaseElem . B.ElemKern . B.Kern =<< astEval ln
    pure False
  AST.Assign AST.Assignment {AST.scope, AST.body} ->
    do
      case body of
        AST.DefineControlSequence cs tgt ->
          do
            newCSTok <- case tgt of
              AST.MacroTarget macro ->
                pure (Tok.SyntaxCommandHeadToken $ Tok.MacroTok macro)
              -- TODO: If a \let target is an active character, should we
              -- treat it as a control sequence, or a char-cat pair?
              AST.LetTarget (Lex.CharCatToken tgtCC) ->
                pure (Tok.PrimitiveToken $ Tok.LetCharCat tgtCC)
              AST.LetTarget (Lex.ControlSequenceToken tgtCS) ->
                do
                  mayCS <- uses (typed @Config) (lookupCSProper tgtCS)
                  let resTok = fromMaybe (Tok.PrimitiveToken Tok.RelaxTok) mayCS
                  pure resTok
              AST.ShortDefineTarget q n ->
                do
                  en <- astEval n
                  pure (Tok.PrimitiveToken $ Tok.IntRefTok q en)
              AST.FontTarget fontSpec fPath ->
                do
                  fontDef@B.FontDefinition {B.fontNr} <- loadFont fPath fontSpec
                  addVElem $ BL.VListBaseElem $ B.ElemFontDefinition fontDef
                  pure $ Tok.PrimitiveToken $ Tok.FontRefToken fontNr
              oth ->
                panic $ "Not implemented: DefineControlSequence target " <> show oth
            sLogStampedJSON
              "Defining control sequence"
              [ ("controlSequence", toJSON cs),
                ("token", toJSON newCSTok),
                ("scope", toJSON scope)
              ]
            modifying' (typed @Config) $ setControlSequence cs newCSTok scope
        AST.SetVariable ass ->
          case ass of
            AST.TeXIntVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            AST.LengthVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            AST.GlueVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            AST.MathGlueVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            AST.TokenListVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            AST.SpecialTeXIntVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
            AST.SpecialLengthVariableAssignment v tgt ->
              Var.setValueFromAST v scope tgt
        AST.ModifyVariable modCommand ->
          case modCommand of
            AST.AdvanceTeXIntVariable var plusVal ->
              Var.advanceValueFromAST var scope plusVal
            AST.AdvanceLengthVariable var plusVal ->
              Var.advanceValueFromAST var scope plusVal
            AST.AdvanceGlueVariable var plusVal ->
              Var.advanceValueFromAST var scope plusVal
            AST.AdvanceMathGlueVariable var plusVal ->
              Var.advanceValueFromAST var scope plusVal
            AST.ScaleVariable vDir numVar scaleVal ->
              case numVar of
                AST.TeXIntNumericVariable var ->
                  Var.scaleValueFromAST var scope vDir scaleVal
                AST.LengthNumericVariable var ->
                  Var.scaleValueFromAST var scope vDir scaleVal
                AST.GlueNumericVariable var ->
                  Var.scaleValueFromAST var scope vDir scaleVal
                AST.MathGlueNumericVariable var ->
                  Var.scaleValueFromAST var scope vDir scaleVal
        AST.AssignCode (AST.CodeAssignment (AST.CodeTableRef codeType idx) val) ->
          do
            eIdx <- astEval idx
            eVal <- astEval val
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
        AST.SelectFont fNr ->
          do
            selectFont fNr scope
            addVElem $ BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr
        AST.SetFamilyMember fm fontRef ->
          do
            eFm <- astEval fm
            fNr <- astEval fontRef
            modifying' (typed @Config) $ setFamilyMemberFont eFm fNr scope
        -- Start a new level of grouping. Enter inner mode.
        AST.SetBoxRegister lhsIdx box ->
          do
            eLhsIdx <- astEval lhsIdx
            case box of
              AST.FetchedRegisterBox fetchMode rhsIdx ->
                do
                  fetchedMaybeBox <- fetchBox fetchMode rhsIdx
                  modifying' (typed @Config) $ setBoxRegisterNullable eLhsIdx scope fetchedMaybeBox
              AST.LastBox ->
                panic "Not implemented: SetBoxRegister to LastBox"
              AST.VSplitBox _ _ ->
                panic "Not implemented: SetBoxRegister to VSplitBox"
              AST.ExplicitBox spec boxType -> do
                eSpec <- astEval spec
                modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
                extractedBox <- extractExplicitBox eSpec boxType
                modifying' (typed @Config) $ setBoxRegister eLhsIdx extractedBox scope
        AST.SetFontChar (AST.FontCharRef fontChar fontRef) charRef ->
          do
            fNr <- astEval fontRef
            eCharRef <- astEval charRef
            let updateFontChar f = case fontChar of
                  Tok.SkewChar -> f {skewChar = eCharRef}
                  Tok.HyphenChar -> f {hyphenChar = eCharRef}
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
  AST.WriteToStream n (AST.ImmediateWriteText eTxt) -> do
    en <- astEval n
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
  AST.ChangeScope Tok.Positive entryTrig ->
    do
      modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup entryTrig))
      pure False
  -- Do the appropriate finishing actions, undo the
  -- effects of non-global assignments, and leave the
  -- group. Maybe leave the current mode.
  AST.ChangeScope Tok.Negative exitTrig -> do
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
  AST.AddBox AST.NaturalPlacement boxSource -> do
    case boxSource of
      AST.FetchedRegisterBox fetchMode idx ->
        fetchBox fetchMode idx >>= \case
          Nothing ->
            pure ()
          Just b ->
            addVElem $ BL.VListBaseElem $ B.ElemBox b
      AST.ExplicitBox spec boxType -> do
        -- Start a new level of grouping. Enter inner mode.
        eSpec <- astEval spec
        modifying' (typed @Config) $ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
        b <- extractExplicitBox eSpec boxType
        addVElem $ BL.VListBaseElem $ B.ElemBox b
    pure False
  oth ->
    panic $ show oth
