{-# LANGUAGE RankNTypes #-}

module HeX.Command.Build where

import           HeXlude

import           Control.Monad               (foldM)
import qualified Data.Sequence               as Seq

import           TFM                         (TFMError)

import qualified HeX.Box                     as B
import           HeX.BreakList               (HList, VList)
import qualified HeX.BreakList               as BL
import           HeX.Command.Commands
import           HeX.Command.Common
import           HeX.Command.ModeIndependent
import           HeX.Config
import qualified HeX.Lex                     as Lex
import qualified HeX.Parse                   as HP

newtype BuildError = BuildError Text
    deriving (Show)

data ParaResult = ParaResult EndParaReason HList

data EndParaReason
    = EndParaSawEndParaCommand
    | EndParaSawLeaveBox

handleCommandInParaMode
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => HList
    -> HP.Command
    -> s
    -> m (RecursionResult HList ParaResult)
handleCommandInParaMode hList@(BL.HList hElemSeq) command oldStream =
    case command of
        HP.VModeCommand _ ->
            -- Insert the control sequence "\par" into the input. The control
            -- sequence's current meaning will be used, which might no longer be the \par
            -- primitive.
            -- (Note that we use oldStream.)
            do
            put $ HP.insertLexToken oldStream Lex.parToken
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
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => HList
    -> HP.Command
    -> s
    -> m (RecursionResult HList HBoxResult)
handleCommandInHBoxMode hList@(BL.HList hElemSeq) command _ =
    case command of
        HP.VModeCommand vModeCommand ->
            throwM $ BuildError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
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
    :: ( HP.TeXStream s
       , MonadState s m)
    => VList
    -> BL.VListElem
    -> m (RecursionResult VList b)
addVListElemAndLoop vl e = LoopAgain <$> HP.runConfState (addVListElem vl e)

handleCommandInVBoxMode
    :: ( MonadState s m
       , HP.TeXParseable s e m
       , MonadErrorAnyOf e m
           '[ TFMError
            , BuildError
            ]
       )
    => VList
    -> HP.Command
    -> s
    -> m (RecursionResult VList VBoxResult)
handleCommandInVBoxMode vList command oldStream =
    case command of
        HP.VModeCommand HP.End ->
            throwM $ BuildError "End not allowed in internal vertical mode"
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
    addElem e = addVListElemAndLoop vList e
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
    :: ( HP.TeXStream s
       , MonadIO m
       , MonadState s m
       , MonadErrorAnyOf e m '[BuildError]
       )
    => HList
    -> VList
    -> m VList
appendParagraph paraHList vList =
    do
    lineBoxes <- readOnConfState (hListToParaLineBoxes paraHList)
    let lineBoxContents = (B.HBoxContents <$>) <$> lineBoxes
    let boxElems = BL.VListBaseElem . B.ElemBox <$> lineBoxContents
    HP.runConfState $ foldM addVListElem vList boxElems

hListToParaLineBoxes
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[BuildError]
       )
    => HList
    -> m (Seq (B.Box B.HBox))
hListToParaLineBoxes hList =
    do
    hSize <- asks $ LenParamVal . lookupLengthParameter HP.HSize
    lineTol <- asks $ IntParamVal . lookupTeXIntParameter HP.Tolerance
    linePen <- asks $ IntParamVal . lookupTeXIntParameter HP.LinePenalty
    case BL.breakAndSetParagraph hSize lineTol linePen hList of
        Left err -> throwM $ BuildError err
        Right v -> pure v

newtype MainVModeResult = MainVModeResult VList

handleCommandInMainVMode
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => VList
    -> HP.Command
    -> s
    -> m (RecursionResult VList MainVModeResult)
handleCommandInMainVMode vList command oldStream =
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
                    throwM $ BuildError "No box to end: in main V mode"
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
                throwM $ BuildError "No box to end: in paragraph within main V mode"

extractPara
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => HP.IndentFlag
    -> m ParaResult
extractPara indentFlag =
    do
    bx <- BL.HList <$> case indentFlag of
        HP.Indent ->
            do
            indentElem <- readOnConfState (asks parIndentBox)
            pure (Seq.singleton indentElem)
        HP.DoNotIndent ->
            pure mempty
    runCommandLoop handleCommandInParaMode bx

extractParaFromVMode
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => HP.IndentFlag
    -> s
    -> m ParaResult
extractParaFromVMode indentFlag oldStream =
    -- If the command shifts to horizontal mode, run
    -- '\indent', and re-read the stream as if the
    -- command hadn't been read. (Note that we set
    -- "oldStream", not "newStream".)
    put oldStream >> extractPara indentFlag

extractHBox
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => m HBoxResult
extractHBox = runCommandLoop handleCommandInHBoxMode mempty

extractVBox
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => m VBoxResult
extractVBox = runCommandLoop handleCommandInVBoxMode mempty

extractVSubBox
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
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
            modConfState $ setBoxRegister idx box global
            pure Nothing

extractHSubBox
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
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
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => m MainVModeResult
extractMainVList = runCommandLoop handleCommandInMainVMode mempty

extractBreakAndSetVList
    :: ( MonadErrorAnyOf e m
           '[ BuildError
            , TFMError
            ]
       , HP.TeXParseable s e m
       , MonadState s m
       )
    => m (Seq B.Page)
extractBreakAndSetVList =
    do
    MainVModeResult finalMainVList <- extractMainVList
    gets HP.getConditionBodyState >>= \case
        Nothing -> pure ()
        Just _condState -> throwM $ BuildError $ "Cannot end: in condition block: " <> showT _condState
    readOnConfState (asks finaliseConfig) >>= liftIO
    desiredH <- HP.runConfState $ gets $ LenParamVal . lookupLengthParameter HP.VSize
    putText $ describe finalMainVList
    pure $ BL.runPageBuilder desiredH BL.newCurrentPage finalMainVList
