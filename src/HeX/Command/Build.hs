{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Command.Build where

import           Hexlude

import           Control.Monad.Trans.Class

import qualified Data.Sequence               as Seq
import qualified Data.Path

import           TFM                         (TFMError)


import qualified Hex.Box                     as B
import           Hex.BreakList               (HList(..), VList(..))
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

type TeXBuildCtx st e m
  = ( MonadState st m
    , HasType Config st

    , MonadError e m
    , HP.AsTeXParseErrors e
    , AsType HP.ParseError e
    )

newtype BuildError = BuildError Text
    deriving stock (Show)

data EndParaReason
    = EndParaSawEndParaCommand
    | EndParaSawLeaveBox




class MonadModeIndependentBuild m where

    extractExplicitBox :: B.DesiredLength -> HP.ExplicitBox -> m (B.Box B.BoxContents)

    insertLexToken :: Lex.Token -> m ()

    revertStream :: m ()

    addVElem :: BL.VListElem -> m ()

class MonadHModeBuild m where

    addHElem :: BL.HListElem -> m ()

newtype ListBuilderT s l m a
  = ListBuilderT {unListBuilderT :: s -> s -> l -> m (s, s, l, a)}

instance Functor m => Functor (ListBuilderT s l m) where
    fmap g (ListBuilderT f) = ListBuilderT $ \s0 s1 l ->
        f s0 s1 l <&> (\(s0_, s1_, l_, a_) -> (s0_, s1_, l_, g a_))

instance Monad m => Applicative (ListBuilderT s l m) where
  pure a = ListBuilderT $ \s0 s1 l -> pure (s0, s1, l, a)

  -- (<*>) :: m (a -> b) -> m a -> m b
  (ListBuilderT fAToB) <*> (ListBuilderT fA) = ListBuilderT $ \s0 s1 l -> do
    (s0_, s1_, l_, aToB) <- fAToB s0 s1 l
    (s0__, s1__, l__, a) <- fA s0_ s1_ l_
    pure (s0__, s1__, l__, aToB a)

instance Monad m => Monad (ListBuilderT s l m) where
  return = pure

  -- m a -> (a -> m b) -> m b
  (ListBuilderT fA) >>= aToTFB = ListBuilderT $ \s0 s1 l -> do
    (s0_, s1_, l_, a) <- fA s0 s1 l
    let (ListBuilderT fB) = aToTFB a
    fB s0_ s1_ l_

instance MonadTrans (ListBuilderT s l) where
  lift m = ListBuilderT $ \s0 s1 l -> do
    a <- m
    pure (s0, s1, l, a)

instance (MonadError e m) => MonadError e (ListBuilderT s l m) where
  throwError = lift . throwError

  catchError (ListBuilderT f) errToHandle = ListBuilderT $ \s0 s1 l ->
    catchError (f s0 s1 l) $ \e -> do
      let (ListBuilderT fRecover) = errToHandle e
      fRecover s0 s1 l

instance MonadState st m => MonadState st (ListBuilderT s l m) where
  get = lift get
  put = lift . put

instance MonadIO m => MonadIO (ListBuilderT s l m) where
  liftIO = lift . liftIO

instance MonadSlog m => MonadSlog (ListBuilderT s l m) where
  sLog = lift . sLog

  sTime = lift sTime

instance
  ( MonadError e m
  , AsType BuildError e
  , AsType TFMError e
  , AsType Data.Path.PathError e

  , TeXBuildCtx st e m

  , HP.MonadTeXParse (HP.TeXParseT s m)
  , HP.TeXStream s

  , MonadIO m
  , MonadSlog m
  ) => MonadModeIndependentBuild (ListBuilderT s HList m) where
    addVElem e = addHElem (BL.HVListElem e)

    insertLexToken = commonInsertLexToken

    extractExplicitBox = commonExtractExplicitBox

    revertStream = commonRevertStream

instance
  ( MonadError e m
  , TeXBuildCtx st e m
  , MonadIO m
  ) => MonadHModeBuild (ListBuilderT s HList m) where
    addHElem e =
      ListBuilderT $ \s0 s1 (HList hElemSeq) ->
        let newLst = HList $ hElemSeq :|> e
        in pure (s0, s1, newLst, ())

instance
  ( MonadError e m
  , AsType BuildError e
  , AsType TFMError e
  , AsType Data.Path.PathError e

  , TeXBuildCtx st e m

  , HP.MonadTeXParse (HP.TeXParseT s m)
  , HP.TeXStream s

  , MonadIO m
  , MonadSlog m
  ) => MonadModeIndependentBuild (ListBuilderT s VList m) where
    addVElem e =
        ListBuilderT $ \s0 s1 (VList vElemSeq) ->
            let newLst = VList $ vElemSeq :|> e
            in pure (s0, s1, newLst, ())

    insertLexToken = commonInsertLexToken

    extractExplicitBox = commonExtractExplicitBox

    revertStream = commonRevertStream

commonInsertLexToken
  :: ( Applicative m
     , HP.TeXStream s
     )
  => Lex.Token
  -> ListBuilderT s l m ()
commonInsertLexToken lt =
  ListBuilderT $ \s0 s1 list_ ->
    pure (s0, HP.insertLexToken s1 lt, list_, ())

commonExtractExplicitBox
  :: ( MonadError e m
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , TeXBuildCtx st e m

     , HP.MonadTeXParse (HP.TeXParseT s m)
     , HP.TeXStream s

     , MonadIO m
     , MonadSlog m
     )
  => B.DesiredLength
  -> HP.ExplicitBox
  -> ListBuilderT s l m (B.Box B.BoxContents)
commonExtractExplicitBox desiredLength boxType =
  ListBuilderT $ \s0 s1 list_ -> do
    (s1_, box) <- extractExplicitBoxContents s1 desiredLength boxType
    pure (s0, s1_, list_, box)

commonRevertStream
  :: (Applicative m)
  => ListBuilderT s l m ()
commonRevertStream =
  ListBuilderT $ \s0 _s1 list_ ->
    pure (s0, s0, list_, ())






handleCommandInParaMode
    :: ( MonadError e m
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , MonadModeIndependentBuild m
       , MonadHModeBuild m

       , MonadIO m
       , MonadSlog m
       )
    => HP.Command
    -> m (Maybe EndParaReason)
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

handleCommandInHBoxMode
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
       )
    => s
    -> s
    -> HList
    -> HP.Command
    -> m (s, HList, Maybe ())
handleCommandInHBoxMode oldS newS hList@(HList hElemSeq) command =
    case command of
        HP.VModeCommand vModeCommand ->
            throwError $ injectTyped $ BuildError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
        HP.HModeCommand (HP.AddCharacter c) ->
            (newS,,Nothing) . addElem_ <$> hModeCharacterElem c
        HP.HModeCommand (HP.AddHGlue g) ->
            (newS,,Nothing) . addElem_ . BL.HVListElem <$> glueToElem g
        HP.HModeCommand (HP.AddHRule rule) ->
            (newS,,Nothing) . addElem_ <$> hModeRuleElem rule
        HP.AddSpace ->
            (newS,,Nothing) . addElem_ <$> hModeSpaceElem
        HP.StartParagraph indentFlag ->
            (newS,,Nothing) . addMaybeElem_ <$> hModeStartParagraph indentFlag
        -- \par: Restricted (this mode): does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            pure (newS, hList, Nothing)
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (_, doneS, doneList, sawEndBox) <- unListBuilderT (handleModeIndependentCommand modeIndependentCommand) oldS newS hList
            pure (doneS, doneList, if sawEndBox then Just () else Nothing)
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    addElem_ e = HList $ hElemSeq :|> e
    addMaybeElem_ mayE = HList $ addMaybeElem hElemSeq mayE

handleCommandInVBoxMode
    :: forall s st e m
     . ( TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , AsType TFMError e
       , AsType BuildError e
       , AsType Data.Path.PathError e

       , MonadSlog m
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
            glueToElem g >>= addElem_ <&> (newS,,Nothing)
        HP.VModeCommand (HP.AddVRule rule) ->
            vModeRuleElem rule >>= addElem_ <&> (newS,,Nothing)
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
            (_, doneS, doneList, sawEndBox) <- unListBuilderT (handleModeIndependentCommand modeIndependentCommand) oldS newS vList
            pure (doneS, doneList, if sawEndBox then Just () else Nothing)
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    addElem_ e = addVListElem vList e

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

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
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
            glueToElem g >>= addElem_ <&> (newS, , Nothing)
        HP.VModeCommand (HP.AddVRule rule) ->
            vModeRuleElem rule >>= addElem_ <&> (newS, , Nothing)
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
            (_, doneS, doneList, sawEndBox) <- unListBuilderT (handleModeIndependentCommand modeIndependentCommand) oldS newS vList
            when sawEndBox $ throwError $ injectTyped $ BuildError "No box to end: in main V mode"
            pure (doneS, doneList, Nothing)
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    addElem_ e = addVListElem vList e

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

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
       )
    => HP.IndentFlag
    -> s
    -> m (s, HList, EndParaReason)
extractPara indentFlag s = do
    initList <- case indentFlag of
        HP.Indent -> do
            indentBox <- gets (view $ typed @Config % to parIndentBox)
            pure $ Seq.singleton indentBox
        HP.DoNotIndent ->
            pure mempty
    let handleCommand oldS newS hList cmd = do
          (_, doneS, doneList, mayEndParaReason) <- unListBuilderT (handleCommandInParaMode cmd) oldS newS hList
          pure (doneS, doneList, mayEndParaReason)

    runCommandLoop handleCommand s (HList initList)

extractMainVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
       )
    => s
    -> m (s, VList)
extractMainVList s = do
    (doneS, vList, _) <- runCommandLoop handleCommandInMainVMode s mempty
    sLog "In extractMainVList, done runCommandLoop handleCommandInMainVMode"
    pure (doneS, vList)

extractBreakAndSetMainVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
       )
    => s
    -> m (s, Seq B.Page, IntParamVal 'HP.Mag)
extractBreakAndSetMainVList s = do
    (finalS, finalMainVList) <- extractMainVList s
    sLog "In extractBreakAndSetMainVList, done extractMainVList"
    case finalS ^. HP.conditionBodyStateLens of
        [] -> pure ()
        condStates -> throwError $ injectTyped $ BuildError $ "Cannot end: in condition block: " <> show condStates
    join $ gets $ view $ typed @Config % to finaliseConfig
    desiredH <- gets $ view $ typed @Config % to (LenParamVal . lookupLengthParameter HP.VSize)
    let pages = BL.runPageBuilder desiredH BL.newCurrentPage finalMainVList
    mag <- gets $ view $ typed @Config % to (IntParamVal . lookupTeXIntParameter HP.Mag)
    pure (finalS, pages, mag)

handleModeIndependentCommand
    :: ( MonadIO m
       , MonadSlog m

       , AsType Data.Path.PathError e
       , AsType TFMError e

       , TeXBuildCtx st e m

       , MonadModeIndependentBuild m
       )
    => HP.ModeIndependentCommand
    -> m Bool
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
        modify $ typed @Config % field @"afterAssignmentToken" ?~ lt
        pure False
    HP.AddPenalty n -> do
        addVElem . BL.ListPenalty . BL.Penalty =<< texEvaluate n
        pure False
    HP.AddKern ln -> do
        addVElem . BL.VListBaseElem . B.ElemKern . B.Kern =<< texEvaluate ln
        pure False
    HP.Assign HP.Assignment { HP.scope, HP.body } ->
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
                        mayCS <- gets (view $ typed @Config % to (lookupCSProper tgtCS))
                        let resTok = fromMaybe (HP.PrimitiveToken HP.RelaxTok) mayCS
                        pure resTok
                    HP.ShortDefineTarget q n ->
                        do
                        en <- texEvaluate n
                        pure (HR.primTok $ HP.IntRefTok q en)
                    HP.FontTarget fontSpec fPath ->
                        do
                        fontDef@B.FontDefinition { B.fontNr } <- loadFont fPath fontSpec
                        addVElem $ BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure $ HR.primTok $ HP.FontRefToken fontNr
                    oth ->
                        panic $ "Not implemented: DefineControlSequence target " <> show oth
                sLogStampedJSON "Defining control sequence"
                    [ ("controlSequence", toJSON cs)
                    , ("token", toJSON newCSTok)
                    , ("scope", toJSON scope)
                    ]
                modify $ typed @Config %~ setControlSequence cs newCSTok scope
            HP.SetVariable ass ->
                case ass of
                    HP.TeXIntVariableAssignment v tgt ->
                        Var.setValueFromAST v scope tgt
                    HP.LengthVariableAssignment v tgt  ->
                        Var.setValueFromAST v scope tgt
                    HP.GlueVariableAssignment v tgt    ->
                        Var.setValueFromAST v scope tgt
                    HP.MathGlueVariableAssignment v tgt  ->
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
                sLogStampedJSON "Doing code assignment"
                    [ ("codeTableIndexSymbolic", toJSON idx)
                    , ("codeTableIndexEvaluated", toJSON eIdx)
                    , ("codeTableIndexAsChar", toJSON idxChar)
                    , ("codeTableValueSymbolic", toJSON val)
                    , ("codeTableValueEvaluated", toJSON eVal)
                    , ("codeType", toJSON codeType)
                    , ("scope", toJSON scope)
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
                modify $ typed @Config %~ setFamilyMemberFont eFm fNr scope
            -- Start a new level of grouping. Enter inner mode.
            HP.SetBoxRegister lhsIdx box ->
                do
                eLhsIdx <- texEvaluate lhsIdx
                case box of
                    HP.FetchedRegisterBox fetchMode rhsIdx ->
                        do
                        fetchedMaybeBox <- fetchBox fetchMode rhsIdx
                        modify $ typed @Config %~ setBoxRegisterNullable eLhsIdx scope fetchedMaybeBox
                    HP.LastBox ->
                        panic "Not implemented: SetBoxRegister to LastBox"
                    HP.VSplitBox _ _ ->
                        panic "Not implemented: SetBoxRegister to VSplitBox"
                    HP.ExplicitBox spec boxType -> do
                        eSpec <- texEvaluate spec
                        modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
                        extractedBox <- extractExplicitBox eSpec boxType
                        modify $ typed @Config %~ setBoxRegister eLhsIdx extractedBox scope
            HP.SetFontChar (HP.FontCharRef fontChar fontRef) charRef ->
                do
                fNr <- texEvaluate fontRef
                eCharRef <- texEvaluate charRef
                let updateFontChar f = case fontChar of
                        HP.SkewChar   -> f { skewChar = eCharRef }
                        HP.HyphenChar -> f { hyphenChar = eCharRef }
                modifyFont fNr updateFontChar
            oth ->
                panic $ show oth
        gets (view $ typed @Config % field @"afterAssignmentToken") >>= \case
            Nothing ->
                pure ()
            Just lt ->
                do
                insertLexToken lt
                modify $ typed @Config % field @"afterAssignmentToken" .~ Nothing
        pure False
    HP.WriteToStream n (HP.ImmediateWriteText eTxt) -> do
        en <- texEvaluate n
        fStreams <- gets $ view $ typed @Config % field @"outFileStreams"
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
                when (en >= 0) $ sLog txtTxt
                -- Write to log
                logHandle <- gets $ view $ typed @Config % field @"logStream"
                liftIO $ hPutStrLn logHandle txtTxt
        pure False
    -- Start a new level of grouping.
    HP.ChangeScope HP.Positive trig ->
        do
        modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope (LocalStructureGroup trig))
        pure False
    -- Do the appropriate finishing actions, undo the
    -- effects of non-global assignments, and leave the
    -- group. Maybe leave the current mode.
    HP.ChangeScope HP.Negative trig -> do
        (group, poppedConfig) <- gets (view $ typed @Config % to popGroup) >>= \case
            Nothing ->
                throwError $ injectTyped $ ConfigError "No group to leave"
            Just v ->
                pure v
        modify $ typed @Config .~ poppedConfig
        case group of
            -- Undo the effects of non-global
            -- assignments without leaving the
            -- current mode.
            ScopeGroup _ (LocalStructureGroup trigConf) -> do
                when (trigConf /= trig) $
                    throwError $ injectTyped $ ConfigError $ "Entry and exit group triggers differ: " <> show (trig, trigConf)
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
                        addVElem (BL.VListBaseElem $ B.ElemBox b)
            HP.ExplicitBox spec boxType -> do
                -- Start a new level of grouping. Enter inner mode.
                eSpec <- texEvaluate spec
                modify $ typed @Config %~ pushGroup (ScopeGroup newLocalScope ExplicitBoxGroup)
                b <- extractExplicitBox eSpec boxType
                addVElem (BL.VListBaseElem $ B.ElemBox b)
        pure False
    oth ->
        panic $ show oth

extractExplicitBoxContents
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
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
