{-# LANGUAGE RankNTypes #-}

module Hex.Command.Build where

import           Hexlude

import qualified Data.Sequence               as Seq
import qualified Data.Path

import           TFM                         (TFMError)

import qualified Hex.Box                     as B
import           Hex.BreakList               (HList, VList)
import qualified Hex.BreakList               as BL
import           Hex.Command.Commands
import           Hex.Command.Common
import           Hex.Command.ModeIndependent
import           Hex.Config
import qualified Hex.Lex                     as Lex
import qualified Hex.Parse                   as HP

newtype BuildError = BuildError Text
    deriving stock (Show)

data ParaResult = ParaResult EndParaReason HList

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
    -> m (s, RecursionResult HList ParaResult)
handleCommandInParaMode oldS newS hList@(BL.HList hElemSeq) command =
    case command of
        HP.VModeCommand _ ->
            -- Insert the control sequence "\par" into the input. The control
            -- sequence's current meaning will be used, which might no longer be the \par
            -- primitive.
            -- (Note that we use oldS.)
            pure (HP.insertLexToken oldS Lex.parToken, doNothing)
        HP.HModeCommand (HP.AddHGlue g) ->
            (newS,) . addElem <$> hModeAddHGlue g
        HP.HModeCommand (HP.AddCharacter c) ->
            (newS,) . addElem <$> hModeAddCharacter c
        HP.HModeCommand (HP.AddHRule rule) ->
            (newS,) . addElem <$> hModeAddRule rule
        HP.AddSpace ->
            (newS,) . addElem <$> hModeAddSpace
        HP.StartParagraph indentFlag ->
            (newS,) . addMaybeElem' <$> hModeStartParagraph indentFlag
        -- \par: Restricted: does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            pure $ endLoop EndParaSawEndParaCommand hList
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (doneS, modeIndRes) <- handleModeIndependentCommand newS modeIndependentCommand
            case modeIndRes of
                AddElem extraElem ->
                    pure (doneS, addElem (BL.HVListElem extraElem))
                EnterBoxMode desiredLength boxType boxIntent -> do
                    (done1S, mayBox) <- extractHSubBox desiredLength boxIntent boxType doneS
                    pure (done1S, addMaybeElem' mayBox)
                FinishBoxMode ->
                    pure $ endLoop EndParaSawLeaveBox hList
                DoNothing ->
                    pure (newS, doNothing)
        oth ->
            panic $ show oth
  where
    doNothing = LoopAgain hList
    addElem e = LoopAgain $ BL.HList $ hElemSeq :|> e
    addMaybeElem' mayE = LoopAgain $ BL.HList $ addMaybeElem hElemSeq mayE
    endLoop reason result = (newS, EndLoop $ ParaResult reason result)

newtype HBoxResult = HBoxResult HList

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
    -> m (s, RecursionResult HList HBoxResult)
handleCommandInHBoxMode _ newS hList@(BL.HList hElemSeq) command =
    case command of
        HP.VModeCommand vModeCommand ->
            throwError $ injectTyped $ BuildError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
        HP.HModeCommand (HP.AddCharacter c) ->
            (newS,) . addElem <$> hModeAddCharacter c
        HP.HModeCommand (HP.AddHGlue g) ->
            (newS,) . addElem <$> hModeAddHGlue g
        HP.HModeCommand (HP.AddHRule rule) ->
            (newS,) . addElem <$> hModeAddRule rule
        HP.AddSpace ->
            (newS,) . addElem <$> hModeAddSpace
        HP.StartParagraph indentFlag ->
            (newS,) . addMaybeElem' <$> hModeStartParagraph indentFlag
        -- \par: Restricted: does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            pure (newS, doNothing)
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (doneS, modeIndRes) <- handleModeIndependentCommand newS modeIndependentCommand
            case modeIndRes of
                AddElem extraElem ->
                    pure (doneS, addElem (BL.HVListElem extraElem))
                EnterBoxMode desiredLength boxType boxIntent -> do
                    (done1S, mayBox) <- extractHSubBox desiredLength boxIntent boxType doneS
                    pure (done1S, LoopAgain $ BL.HList $ addMaybeElem hElemSeq mayBox)
                FinishBoxMode ->
                    pure $ endLoop hList
                DoNothing ->
                    pure (doneS, doNothing)
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing = LoopAgain hList
    addElem e = LoopAgain $ BL.HList $ hElemSeq :|> e
    addMaybeElem' mayE = LoopAgain $ BL.HList $ addMaybeElem hElemSeq mayE
    endLoop lst = (newS, EndLoop $ HBoxResult lst)

newtype VBoxResult = VBoxResult VList

addVListElemAndLoop
    :: ( MonadState st m
       , HasType Config st
       )

    => VList
    -> BL.VListElem
    -> m (RecursionResult VList b)
addVListElemAndLoop vl e = LoopAgain <$> addVListElem vl e

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
    -> m (s, RecursionResult VList VBoxResult)
handleCommandInVBoxMode oldS newS vList command =
    case command of
        HP.VModeCommand HP.End ->
            throwError $ injectTyped $ BuildError "End not allowed in internal vertical mode"
        HP.VModeCommand (HP.AddVGlue g) ->
            vModeAddVGlue g >>= addElem <&> (newS,)
        HP.VModeCommand (HP.AddVRule rule) ->
            vModeAddRule rule >>= addElem <&> (newS,)
        HP.HModeCommand _ ->
            addPara HP.Indent
        HP.StartParagraph indentFlag ->
            addPara indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            pure (newS, doNothing)
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            pure (newS, doNothing)
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (doneS, modeIndRes) <- handleModeIndependentCommand newS modeIndependentCommand
            case modeIndRes of
                AddElem extraElem -> do
                    (doneS,) <$> addElem extraElem
                EnterBoxMode desiredLength boxType boxIntent -> do
                    (done1S, mayElem) <- extractVSubBox desiredLength boxIntent boxType doneS
                    recRes <- case mayElem of
                        Nothing ->
                            pure doNothing
                        Just box -> do
                            addElem box
                    pure (done1S, recRes)
                FinishBoxMode ->
                    pure (doneS, endLoop vList)
                DoNothing ->
                    pure (doneS, doNothing)
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing = LoopAgain vList
    addElem = addVListElemAndLoop vList
    endLoop = EndLoop . VBoxResult

    addPara :: HP.IndentFlag -> m (s, RecursionResult VList VBoxResult)
    addPara indentFlag = do
        -- Note oldS.
        (doneS, ParaResult endParaReason finalParaHList) <- extractPara indentFlag oldS
        vListWithPara <- appendParagraph finalParaHList vList
        pure $ (doneS,) $ case endParaReason of
            EndParaSawEndParaCommand ->
                LoopAgain vListWithPara
            EndParaSawLeaveBox ->
                endLoop vListWithPara

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

newtype MainVModeResult = MainVModeResult VList

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
    -> m (s, RecursionResult VList MainVModeResult)
handleCommandInMainVMode oldS newS vList command =
    -- traceText ("in main v mode, handling command: " <> show command) $ case command of
    case command of
        HP.VModeCommand HP.End ->
            pure (newS, endLoop vList)
        HP.VModeCommand (HP.AddVGlue g) ->
            vModeAddVGlue g >>= addElem <&> (newS,)
        HP.VModeCommand (HP.AddVRule rule) ->
            vModeAddRule rule >>= addElem <&> (newS,)
        HP.HModeCommand _ ->
            addPara HP.Indent
        HP.StartParagraph indentFlag ->
            addPara indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            pure (newS, doNothing)
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            pure (newS, doNothing)
        HP.ModeIndependentCommand modeIndependentCommand -> do
            (doneS, modeIndRes) <- handleModeIndependentCommand newS modeIndependentCommand
            case modeIndRes of
                AddElem extraElem ->
                    (doneS,) <$> addElem extraElem
                EnterBoxMode desiredLength boxType boxIntent -> do
                    (done1S, mayElem) <- extractVSubBox desiredLength boxIntent boxType doneS
                    recRes <- case mayElem of
                        Nothing -> pure doNothing
                        Just box -> addElem box
                    pure (done1S, recRes)
                FinishBoxMode ->
                    throwError $ injectTyped $ BuildError "No box to end: in main V mode"
                DoNothing ->
                    pure (doneS, doNothing)
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing = LoopAgain vList
    addElem = addVListElemAndLoop vList
    endLoop = EndLoop . MainVModeResult

    addPara indentFlag =
        do
        -- If the command shifts to horizontal mode, run '\indent', and re-read
        -- the stream as if the command hadn't been read. (Note that we read
        -- from "oldS", not "newS".)
        (doneS, ParaResult endParaReason finalParaHList) <- extractPara indentFlag oldS
        case endParaReason of
            EndParaSawEndParaCommand ->
                (doneS,) . LoopAgain <$> appendParagraph finalParaHList vList
            EndParaSawLeaveBox ->
                throwError $ injectTyped $ BuildError "No box to end: in paragraph within main V mode"

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
    -> m (s, ParaResult)
extractPara indentFlag s = do
    initList <- case indentFlag of
        HP.Indent -> do
            indentBox <- gets (view $ typed @Config . to parIndentBox)
            pure $ Seq.singleton indentBox
        HP.DoNotIndent ->
            pure mempty
    runCommandLoop handleCommandInParaMode s (BL.HList initList)

extractHBox
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> m (s, HBoxResult)
extractHBox s = runCommandLoop handleCommandInHBoxMode s mempty

extractVBox
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> m (s, VBoxResult)
extractVBox s = runCommandLoop handleCommandInVBoxMode s mempty

extractVSubBox
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => B.DesiredLength
    -> BoxModeIntent
    -> HP.ExplicitBox
    -> s
    -> m (s, Maybe BL.VListElem)
extractVSubBox desiredLength boxIntent boxType s =
    do
    (doneS, box) <- case boxType of
        HP.ExplicitHBox ->
            do
            (doneS, HBoxResult finalHList) <- extractHBox s
            pure (doneS, B.HBoxContents <$> BL.setHList finalHList (BL.UncomputedTargetLength desiredLength))
        HP.ExplicitVBox vAlignType ->
            do
            (doneS, VBoxResult finalVList) <- extractVBox s
            pure (doneS, B.VBoxContents <$> BL.setVList finalVList desiredLength vAlignType)
    mayElem <- case boxIntent of
        IntentToAddBox ->
            pure (Just $ BL.VListBaseElem $ B.ElemBox box)
        IntentToSetBoxRegister idx global ->
            do
            modify $ typed @Config %~ setBoxRegister idx box global
            pure Nothing
    pure (doneS, mayElem)

extractHSubBox
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => B.DesiredLength
    -> BoxModeIntent
    -> HP.ExplicitBox
    -> s
    -> m (s, Maybe BL.HListElem)
extractHSubBox desiredLength boxIntent boxType s =
    do
    (finalS, maybeElem) <- extractVSubBox desiredLength boxIntent boxType s
    pure (finalS, BL.HVListElem <$> maybeElem)

extractMainVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> m (s, MainVModeResult)
extractMainVList s = runCommandLoop handleCommandInMainVMode s mempty

extractBreakAndSetVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable s st e m

       , MonadIO m
       )
    => s
    -> m (s, Seq B.Page, IntParamVal 'HP.Mag)
extractBreakAndSetVList s = do
    (finalS, MainVModeResult finalMainVList) <- extractMainVList s
    case HP.getConditionBodyState finalS of
        Nothing -> pure ()
        Just _condState -> throwError $ injectTyped $ BuildError $ "Cannot end: in condition block: " <> show _condState
    (gets $ view $ typed @Config . to finaliseConfig) >>= liftIO
    desiredH <- gets $ view $ typed @Config . to (LenParamVal . lookupLengthParameter HP.VSize)
    -- putText $ describe finalMainVList
    let pages = BL.runPageBuilder desiredH BL.newCurrentPage finalMainVList
    mag <- gets $ view $ typed @Config . to (IntParamVal . lookupTeXIntParameter HP.Mag)
    pure (finalS, pages, mag)
