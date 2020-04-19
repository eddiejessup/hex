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

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => HList
    -> HP.Command
    -> HP.Tgt st
    -> m (RecursionResult HList ParaResult)
handleCommandInParaMode hList@(BL.HList hElemSeq) command oldStream =
    case command of
        HP.VModeCommand _ ->
            -- Insert the control sequence "\par" into the input. The control
            -- sequence's current meaning will be used, which might no longer be the \par
            -- primitive.
            -- (Note that we use oldStream.)
            do
            modify $ HP.tgtLens .~ (HP.insertLexToken oldStream Lex.parToken)
            pure doNothing
        HP.HModeCommand (HP.AddHGlue g) ->
            addElem <$> hModeAddHGlue g
        HP.HModeCommand (HP.AddCharacter c) ->
            addElem <$> hModeAddCharacter c
        HP.HModeCommand (HP.AddHRule rule) ->
            addElem <$> hModeAddRule rule
        HP.AddSpace ->
            addElem <$> hModeAddSpace
        HP.StartParagraph indentFlag ->
            addMaybeElem' <$> hModeStartParagraph indentFlag
        -- \par: Restricted: does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            pure $ endLoop EndParaSawEndParaCommand hList
        HP.ModeIndependentCommand modeIndependentCommand ->
            handleModeIndependentCommand modeIndependentCommand >>= \case
                AddElem extraElem ->
                    pure $ addElem (BL.HVListElem extraElem)
                EnterBoxMode desiredLength boxType boxIntent ->
                    addMaybeElem' <$> extractHSubBox desiredLength boxIntent boxType
                FinishBoxMode ->
                    pure $ endLoop EndParaSawLeaveBox hList
                DoNothing ->
                    pure doNothing
        oth ->
            panic $ show oth
  where
    doNothing = LoopAgain hList
    addElem e = LoopAgain $ BL.HList $ hElemSeq :|> e
    addMaybeElem' mayE = LoopAgain $ BL.HList $ addMaybeElem hElemSeq mayE
    endLoop reason result = EndLoop $ ParaResult reason result

newtype HBoxResult = HBoxResult HList

handleCommandInHBoxMode
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => HList
    -> HP.Command
    -> HP.Tgt st
    -> m (RecursionResult HList HBoxResult)
handleCommandInHBoxMode hList@(BL.HList hElemSeq) command _ =
    case command of
        HP.VModeCommand vModeCommand ->
            throwError $ injectTyped $ BuildError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
        HP.HModeCommand (HP.AddCharacter c) ->
            addElem <$> hModeAddCharacter c
        HP.HModeCommand (HP.AddHGlue g) ->
            addElem <$> hModeAddHGlue g
        HP.HModeCommand (HP.AddHRule rule) ->
            addElem <$> hModeAddRule rule
        HP.AddSpace ->
            addElem <$> hModeAddSpace
        HP.StartParagraph indentFlag ->
            addMaybeElem' <$> hModeStartParagraph indentFlag
        -- \par: Restricted: does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            pure doNothing
        HP.ModeIndependentCommand modeIndependentCommand ->
            handleModeIndependentCommand modeIndependentCommand >>= \case
                AddElem extraElem ->
                    pure $ addElem (BL.HVListElem extraElem)
                EnterBoxMode desiredLength boxType boxIntent ->
                    addMaybeElem' <$> extractHSubBox desiredLength boxIntent boxType
                FinishBoxMode ->
                    pure $ endLoop hList
                DoNothing ->
                    pure doNothing
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing = LoopAgain hList
    addElem e = LoopAgain $ BL.HList $ hElemSeq :|> e
    addMaybeElem' mayE = LoopAgain $ BL.HList $ addMaybeElem hElemSeq mayE
    endLoop = EndLoop . HBoxResult

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
    :: ( MonadState st m
       , HP.HasTgtType st

       , HP.TeXParseable (HP.Tgt st) st e m

       , AsType TFMError e
       , AsType BuildError e
       , AsType Data.Path.PathError e
       , MonadIO m
       )
    => VList
    -> HP.Command
    -> HP.Tgt st
    -> m (RecursionResult VList VBoxResult)
handleCommandInVBoxMode vList command oldStream =
    case command of
        HP.VModeCommand HP.End ->
            throwError $ injectTyped $ BuildError "End not allowed in internal vertical mode"
        HP.VModeCommand (HP.AddVGlue g) ->
            vModeAddVGlue g >>= addElem
        HP.VModeCommand (HP.AddVRule rule) ->
            vModeAddRule rule >>= addElem
        HP.HModeCommand _ ->
            addPara HP.Indent
        HP.StartParagraph indentFlag ->
            addPara indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            pure doNothing
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            pure doNothing
        HP.ModeIndependentCommand modeIndependentCommand ->
            handleModeIndependentCommand modeIndependentCommand >>= \case
                AddElem extraElem ->
                    addElem extraElem
                EnterBoxMode desiredLength boxType boxIntent ->
                    extractVSubBox desiredLength boxIntent boxType >>= \case
                        Nothing -> pure doNothing
                        Just box -> addElem box
                FinishBoxMode ->
                    pure $ endLoop vList
                DoNothing ->
                    pure doNothing
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing = LoopAgain vList
    addElem = addVListElemAndLoop vList
    endLoop = EndLoop . VBoxResult

    addPara indentFlag =
        do
        ParaResult endParaReason finalParaHList <- extractParaFromVMode indentFlag oldStream
        vListWithPara <- appendParagraph finalParaHList vList
        pure $ case endParaReason of
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
    lineBoxes <- readOnState (hListToParaLineBoxes paraHList)
    let lineBoxContents = (B.HBoxContents <$>) <$> lineBoxes
    let boxElems = BL.VListBaseElem . B.ElemBox <$> lineBoxContents
    foldM addVListElem vList boxElems

hListToParaLineBoxes
    :: ( MonadReader st m, HasType Config st
       , MonadError e m
       , AsType BuildError e
       )
    => HList
    -> m (Seq (B.Box B.HBox))
hListToParaLineBoxes hList =
    do
    hSize <- asks $ LenParamVal . lookupLengthParameter HP.HSize . getTyped @Config
    lineTol <- asks $ IntParamVal . lookupTeXIntParameter HP.Tolerance . getTyped @Config
    linePen <- asks $ IntParamVal . lookupTeXIntParameter HP.LinePenalty . getTyped @Config
    case BL.breakAndSetParagraph hSize lineTol linePen hList of
        Left err -> throwError $ injectTyped $ BuildError err
        Right v -> pure v

newtype MainVModeResult = MainVModeResult VList

handleCommandInMainVMode
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m

       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => VList
    -> HP.Command
    -> HP.Tgt st
    -> m (RecursionResult VList MainVModeResult)
handleCommandInMainVMode vList command oldStream =
    -- traceText ("in main v mode, handling command: " <> show command) $ case command of
    case command of
        HP.VModeCommand HP.End ->
            pure $ endLoop vList
        HP.VModeCommand (HP.AddVGlue g) ->
            vModeAddVGlue g >>= addElem
        HP.VModeCommand (HP.AddVRule rule) ->
            vModeAddRule rule >>= addElem
        HP.HModeCommand _ ->
            addPara HP.Indent
        HP.StartParagraph indentFlag ->
            addPara indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            pure doNothing
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            pure doNothing
        HP.ModeIndependentCommand modeIndependentCommand ->
            handleModeIndependentCommand modeIndependentCommand >>= \case
                AddElem extraElem ->
                    addElem extraElem
                EnterBoxMode desiredLength boxType boxIntent ->
                    extractVSubBox desiredLength boxIntent boxType >>= \case
                        Nothing -> pure doNothing
                        Just box -> addElem box
                FinishBoxMode ->
                    throwError $ injectTyped $ BuildError "No box to end: in main V mode"
                DoNothing ->
                    pure doNothing
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing = LoopAgain vList
    addElem = addVListElemAndLoop vList
    endLoop = EndLoop . MainVModeResult

    addPara indentFlag =
        do
        ParaResult endParaReason finalParaHList <- extractParaFromVMode indentFlag oldStream
        case endParaReason of
            EndParaSawEndParaCommand ->
                LoopAgain <$> appendParagraph finalParaHList vList
            EndParaSawLeaveBox ->
                throwError $ injectTyped $ BuildError "No box to end: in paragraph within main V mode"

extractPara
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => HP.IndentFlag
    -> m ParaResult
extractPara indentFlag =
    runCommandLoop handleCommandInParaMode =<< BL.HList <$> case indentFlag of
        HP.Indent ->
            Seq.singleton <$> gets (view $ typed @Config . to parIndentBox)
        HP.DoNotIndent ->
            pure mempty

extractParaFromVMode
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => HP.IndentFlag
    -> HP.Tgt st
    -> m ParaResult
extractParaFromVMode indentFlag oldStream = do
    -- If the command shifts to horizontal mode, run
    -- '\indent', and re-read the stream as if the
    -- command hadn't been read. (Note that we set
    -- "oldStream", not "newStream".)
    modify $ HP.tgtLens .~ oldStream
    extractPara indentFlag

extractHBox
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => m HBoxResult
extractHBox = runCommandLoop handleCommandInHBoxMode mempty

extractVBox
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => m VBoxResult
extractVBox = runCommandLoop handleCommandInVBoxMode mempty

extractVSubBox
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => B.DesiredLength
    -> BoxModeIntent
    -> HP.ExplicitBox
    -> m (Maybe BL.VListElem)
extractVSubBox desiredLength boxIntent boxType =
    do
    box <- case boxType of
        HP.ExplicitHBox ->
            do
            HBoxResult finalHList <- extractHBox
            pure $ B.HBoxContents <$> BL.setHList finalHList (BL.UncomputedTargetLength desiredLength)
        HP.ExplicitVBox vAlignType ->
            do
            VBoxResult finalVList <- extractVBox
            pure $ B.VBoxContents <$> BL.setVList finalVList desiredLength vAlignType
    case boxIntent of
        IntentToAddBox ->
            pure (Just $ BL.VListBaseElem $ B.ElemBox box)
        IntentToSetBoxRegister idx global ->
            do
            modify $ typed @Config %~ setBoxRegister idx box global
            pure Nothing

extractHSubBox
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => B.DesiredLength
    -> BoxModeIntent
    -> HP.ExplicitBox
    -> m (Maybe BL.HListElem)
extractHSubBox desiredLength boxIntent boxType =
    do
    maybeElem <- extractVSubBox desiredLength boxIntent boxType
    pure $ BL.HVListElem <$> maybeElem

extractMainVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => m MainVModeResult
extractMainVList = runCommandLoop handleCommandInMainVMode mempty

extractBreakAndSetVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , HP.TeXParseable (HP.Tgt st) st e m
       , HP.HasTgtType st

       , MonadState st m

       , MonadIO m
       )
    => m (Seq B.Page, IntParamVal 'HP.Mag)
extractBreakAndSetVList =
    do
    MainVModeResult finalMainVList <- extractMainVList
    gets (view (HP.tgtLens . to HP.getConditionBodyState)) >>= \case
        Nothing -> pure ()
        Just _condState -> throwError $ injectTyped $ BuildError $ "Cannot end: in condition block: " <> show _condState
    (gets $ view $ typed @Config . to finaliseConfig) >>= liftIO
    desiredH <- gets $ view $ typed @Config . to (LenParamVal . lookupLengthParameter HP.VSize)
    -- putText $ describe finalMainVList
    let pages = BL.runPageBuilder desiredH BL.newCurrentPage finalMainVList
    mag <- gets $ view $ typed @Config . to (IntParamVal . lookupTeXIntParameter HP.Mag)
    pure (pages, mag)
