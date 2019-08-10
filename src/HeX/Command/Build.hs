{-# LANGUAGE RankNTypes #-}

module HeX.Command.Build where

import           HeXlude

import           Control.Monad               (foldM)
import           Data.Path                   (PathError)

import           TFM                         (TFMError)

import qualified HeX.Box                     as B
import           HeX.BreakList               (ForwardHList, ForwardVList)
import qualified HeX.BreakList               as BL
import           HeX.Command.Commands
import           HeX.Command.Common
import           HeX.Command.ModeIndependent
import           HeX.Evaluate                (EvaluationError)
import           HeX.Config
import qualified HeX.Lex                     as Lex
import qualified HeX.Parse                   as HP

newtype BuildError = BuildError Text
    deriving (Show)

data ParaResult = ParaResult EndParaReason ForwardHList

data EndParaReason
    = EndParaSawEndParaCommand
    | EndParaSawLeaveBox

handleCommandInParaMode
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , PathError
            , HP.StreamTakeError
            , TFMError
            ]
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       )
    => ForwardHList
    -> HP.Command
    -> s
    -> m (RecursionResult ForwardHList ParaResult)
handleCommandInParaMode hList command oldStream =
    case command of
        HP.VModeCommand _ ->
            -- Insert the control sequence "\par" into the input. The control
            -- sequence's current meaning will be used, which might no longer be the \par
            -- primitive.
            -- (Note that we use oldStream.)
            do
            put $ HP.insertLexToken oldStream Lex.parToken
            pure doNothing'
        HP.HModeCommand (HP.AddHGlue g) ->
            addElem' <$> hModeAddHGlue g
        HP.HModeCommand (HP.AddCharacter c) ->
            addElem' <$> hModeAddCharacter c
        HP.HModeCommand (HP.AddHRule rule) ->
            addElem' <$> hModeAddRule rule
        HP.AddSpace ->
            addElem' <$> hModeAddSpace
        HP.StartParagraph indentFlag ->
            addMaybeElem' <$> hModeStartParagraph indentFlag
        -- \par: Restricted: does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            pure $ endLoop EndParaSawEndParaCommand hList
        HP.ModeIndependentCommand modeIndependentCommand ->
            handleModeIndependentCommand modeIndependentCommand >>= \case
                AddElem extraElem ->
                    pure $ addElem' (BL.HVListElem extraElem)
                EnterBoxMode desiredLength boxType boxIntent ->
                    addMaybeElem' <$> extractHSubBox desiredLength boxIntent boxType
                FinishBoxMode ->
                    pure $ endLoop EndParaSawLeaveBox hList
                DoNothing ->
                    pure doNothing'
        oth ->
            panic $ show oth
  where
    doNothing' = doNothing hList
    addElem' = addElem hList
    addMaybeElem' = addMaybeElem hList
    endLoop reason result = EndLoop $ ParaResult reason result

newtype HBoxResult = HBoxResult ForwardHList

handleCommandInHBoxMode
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
            '[ BuildError
             , ConfigError
             , EvaluationError
             , HP.StreamTakeError
             , TFMError
             , PathError
             ]
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       )
    => ForwardHList
    -> HP.Command
    -> s
    -> m (RecursionResult ForwardHList HBoxResult)
handleCommandInHBoxMode hList command _ =
    case command of
        HP.VModeCommand vModeCommand ->
            throwM $ BuildError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
        HP.HModeCommand (HP.AddCharacter c) ->
            addElem' <$> hModeAddCharacter c
        HP.HModeCommand (HP.AddHGlue g) ->
            addElem' <$> hModeAddHGlue g
        HP.HModeCommand (HP.AddHRule rule) ->
            addElem' <$> hModeAddRule rule
        HP.AddSpace ->
            addElem' <$> hModeAddSpace
        HP.StartParagraph indentFlag ->
            addMaybeElem' <$> hModeStartParagraph indentFlag
        -- \par: Restricted: does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            pure doNothing'
        HP.ModeIndependentCommand modeIndependentCommand ->
            handleModeIndependentCommand modeIndependentCommand >>= \case
                AddElem extraElem ->
                    pure $ addElem' (BL.HVListElem extraElem)
                EnterBoxMode desiredLength boxType boxIntent ->
                    addMaybeElem' <$> extractHSubBox desiredLength boxIntent boxType
                FinishBoxMode ->
                    pure $ endLoop hList
                DoNothing ->
                    pure doNothing'
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing' = doNothing hList
    addElem' = addElem hList
    addMaybeElem' = addMaybeElem hList
    endLoop = EndLoop . HBoxResult

newtype VBoxResult = VBoxResult ForwardVList

handleCommandInVBoxMode
    :: ( HP.TeXStream s
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       , MonadErrorAnyOf e m
            '[ EvaluationError
             , ConfigError
             , PathError
             , HP.StreamTakeError
             , TFMError
             , BuildError]
       )
    => ForwardVList
    -> HP.Command
    -> s
    -> m (RecursionResult ForwardVList VBoxResult)
handleCommandInVBoxMode vList command oldStream =
    case command of
        HP.VModeCommand HP.End ->
            throwM $ BuildError "End not allowed in internal vertical mode"
        HP.VModeCommand (HP.AddVGlue g) ->
            addElem' <$> vModeAddVGlue g
        HP.VModeCommand (HP.AddVRule rule) ->
            addElem' <$> vModeAddRule rule
        HP.HModeCommand _ ->
            addPara HP.Indent
        HP.StartParagraph indentFlag ->
            addPara indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            pure doNothing'
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            pure doNothing'
        HP.ModeIndependentCommand modeIndependentCommand ->
            handleModeIndependentCommand modeIndependentCommand >>= \case
                AddElem extraElem ->
                    pure $ addElem' extraElem
                EnterBoxMode desiredLength boxType boxIntent ->
                    addMaybeElem' <$> extractVSubBox desiredLength boxIntent boxType
                FinishBoxMode ->
                    pure $ endLoop vList
                DoNothing ->
                    pure doNothing'
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing' = doNothing vList
    addElem' = addElem vList
    addMaybeElem' = addMaybeElem vList
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
    => ForwardHList
    -> ForwardVList
    -> m ForwardVList
appendParagraph paraHList vList =
    do
    lineBoxes <- readOnConfState (hListToParaLineBoxes paraHList)
    desiredW <- readOnConfState $ asks $ lookupLengthParameter HP.HSize
    let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
    let boxes = BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes
    HP.runConfState $ foldM addVListElem vList boxes

hListToParaLineBoxes
    :: ( MonadReader Config m
       , MonadErrorAnyOf e m '[BuildError]
       )
    => ForwardHList
    -> m (ForwardDirected Seq (ForwardDirected [] B.HBoxElem))
hListToParaLineBoxes hList =
    do
    desiredW <- asks $ LenParamVal . lookupLengthParameter HP.HSize
    lineTol <- asks $ IntParamVal . lookupTeXIntParameter HP.Tolerance
    linePen <- asks $ IntParamVal . lookupTeXIntParameter HP.LinePenalty
    case BL.breakAndSetParagraph desiredW lineTol linePen hList of
        Left err -> throwM $ BuildError err
        Right v -> pure v

newtype MainVModeResult = MainVModeResult ForwardVList

handleCommandInMainVMode
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , PathError
            , HP.StreamTakeError
            , TFMError
            ]
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       )
    => ForwardVList
    -> HP.Command
    -> s
    -> m (RecursionResult ForwardVList MainVModeResult)
handleCommandInMainVMode vList command oldStream =
    case command of
        HP.VModeCommand HP.End ->
            pure $ endLoop vList
        HP.VModeCommand (HP.AddVGlue g) ->
            addElem' <$> vModeAddVGlue g
        HP.VModeCommand (HP.AddVRule rule) ->
            addElem' <$> vModeAddRule rule
        HP.HModeCommand _ ->
            addPara HP.Indent
        HP.StartParagraph indentFlag ->
            addPara indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            pure doNothing'
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            pure doNothing'
        HP.ModeIndependentCommand modeIndependentCommand ->
            handleModeIndependentCommand modeIndependentCommand >>= \case
                AddElem extraElem ->
                    pure $ addElem' extraElem
                EnterBoxMode desiredLength boxType boxIntent ->
                    addMaybeElem' <$> extractVSubBox desiredLength boxIntent boxType
                FinishBoxMode ->
                    throwM $ BuildError "No box to end: in main V mode"
                DoNothing ->
                    pure doNothing'
        oth ->
            panic $ "Not implemented, outer V mode: " <> show oth
  where
    doNothing' = doNothing vList
    addElem' = addElem vList
    addMaybeElem' = addMaybeElem vList
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
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , PathError
            , HP.StreamTakeError
            , TFMError
            ]
       , MonadIO m
       , MonadState s m
       , MonadPlus m
       )
    => HP.IndentFlag
    -> m ParaResult
extractPara indentFlag =
    do
    bx <- case indentFlag of
        HP.Indent ->
            pure <$> readOnConfState (asks parIndentBox)
        HP.DoNotIndent ->
            pure mempty
    runCommandLoop handleCommandInParaMode bx

extractParaFromVMode
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , TFMError
            , PathError
            , HP.StreamTakeError
            ]
       , MonadIO m
       , MonadState s m
       , MonadPlus m
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
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , TFMError
            , PathError
            , HP.StreamTakeError
            ]
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       )
    => m HBoxResult
extractHBox = runCommandLoop handleCommandInHBoxMode mempty

extractVBox
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , TFMError
            , PathError
            , HP.StreamTakeError
            ]
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       )
    => m VBoxResult
extractVBox = runCommandLoop handleCommandInVBoxMode mempty

extractVSubBox
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , TFMError
            , PathError
            , HP.StreamTakeError
            ]
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       )
    => B.DesiredLength
    -> BoxModeIntent
    -> HP.ExplicitBox
    -> m (Maybe BL.VListElem)
extractVSubBox desiredLength boxIntent boxType =
    do
    -- TODO: I think glue status and desired length
    -- duplicate some meaning.
    boxContents <- case boxType of
        HP.ExplicitHBox ->
            extractHBox >>= \case
                HBoxResult finalHList ->
                    pure $ B.HBoxContents (BL.setHList BL.NaturallyGood finalHList)
        HP.ExplicitVBox vAlignType ->
            extractVBox >>= \case
                VBoxResult finalVList ->
                    pure $ B.VBoxContents (BL.setVList BL.NaturallyGood finalVList) vAlignType
    let box = B.Box boxContents desiredLength
    case boxIntent of
        IntentToAddBox ->
            pure (Just $ BL.VListBaseElem $ B.ElemBox box)
        IntentToSetBoxRegister idx global ->
            do
            modConfState $ setBoxRegister idx box global
            pure Nothing

extractHSubBox
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , TFMError
            , PathError
            , HP.StreamTakeError
            ]
       , MonadIO m
       , MonadPlus m
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
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , TFMError
            , PathError
            , HP.StreamTakeError
            ]
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       )
    => m MainVModeResult
extractMainVList = runCommandLoop handleCommandInMainVMode mempty

extractBreakAndSetVList
    :: ( HP.TeXStream s
       , MonadErrorAnyOf e m
           '[ BuildError
            , ConfigError
            , EvaluationError
            , TFMError
            , PathError
            , HP.StreamTakeError
            ]
       , MonadIO m
       , MonadPlus m
       , MonadState s m
       )
    => m (ForwardDirected Seq B.Page)
extractBreakAndSetVList =
    do
    MainVModeResult finalMainVList <- extractMainVList
    gets HP.getConditionBodyState >>= \case
        Nothing -> pure ()
        Just _condState -> throwM $ BuildError $ "Cannot end: in condition block: " <> showT _condState
    readOnConfState (asks finaliseConfig) >>= liftIO
    desiredH <- HP.runConfState $ gets $ LenParamVal . lookupLengthParameter HP.VSize
    pure $ BL.runPageBuilder desiredH BL.newCurrentPage finalMainVList
