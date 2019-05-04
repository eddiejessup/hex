module HeX.Command.Build where

import HeXlude

import           Control.Monad                  ( foldM )
import           Control.Monad.Except           ( liftEither )
import           Data.Either.Combinators        ( mapLeft )
import qualified Text.Megaparsec               as P

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Command.Commands
import           HeX.Command.Common
import           HeX.Command.ModeIndependent
import           HeX.Config
import qualified HeX.Lex                       as Lex
import qualified HeX.Parse                     as HP

data ParaModeResult
    = ParaEndPara
    | ParaLeaveBox
    | ParaModifyPara HModeContents

handleCommandInParaMode :: HP.InhibitableStream s => HModeContents -> HP.Command -> s -> ExceptMonadBuild s ParaModeResult
handleCommandInParaMode hModeContents@(HModeContents _ maybeChild) command oldStream =
    case maybeChild of
        Nothing ->
            case command of
                HP.VModeCommand _ ->
                    -- Insert the control sequence "\par" into the input. The control
                    -- sequence's current meaning will be used, which might no longer be the \par
                    -- primitive.
                    -- (Note that we use oldStream.)
                    do
                    let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
                    put $ HP.insertLexToken oldStream parToken
                    addElems []
                HP.HModeCommand (HP.AddHGlue g) ->
                    hModeAddHGlue g >>= modifyContents
                HP.HModeCommand (HP.AddCharacter c) ->
                    hModeAddCharacter c >>= modifyContents
                HP.HModeCommand (HP.AddHRule rule) ->
                    hModeAddRule rule >>= modifyContents
                HP.AddSpace ->
                    hModeAddSpace >>= modifyContents
                HP.StartParagraph indentFlag ->
                    hModeStartParagraph indentFlag >>= modifyContents
                -- \par: Restricted: does nothing. Unrestricted: ends mode.
                HP.EndParagraph ->
                    pure ParaEndPara
                HP.ModeIndependentCommand modeIndependentCommand ->
                    handleModeIndependentCommand modeIndependentCommand >>= \case
                        AddElems extraElems ->
                            addElems ((BL.HVListElem <$> extraElems))
                        EnterBoxMode desiredLength boxType boxIntent ->
                            setChild (boxTypeToFreshHChild desiredLength boxType boxIntent)
                        FinishBoxMode ->
                            pure ParaLeaveBox
                oth ->
                    panic $ show oth
        Just hOrVBox ->
            handleCommandInHSubBox hOrVBox command oldStream >>= \case
                HPopAndAddSubBox elems ->
                    modifyContents (hModeWithAddedElems elems . hModeWithoutChild)
                HPop ->
                    modifyContents hModeWithoutChild
                HModBox newHOrVBox ->
                    setChild newHOrVBox
  where
    modifyContents f = pure $ ParaModifyPara (f hModeContents)

    addElems elems = modifyContents (hModeWithAddedElems elems)

    setChild x = modifyContents (hModeWithSetChild x)

data HBoxModeResult
    = HBoxEndHBox HList
    | HBoxModifyHBox HModeContents

handleCommandInHBoxMode :: HP.InhibitableStream s
                        => HModeContents
                        -> HP.Command
                        -> s
                        -> ExceptMonadBuild s HBoxModeResult
handleCommandInHBoxMode hModeContents@(HModeContents boxHList maybeChild) command oldStream =
    case maybeChild of
        Nothing ->
            case command of
                HP.VModeCommand vModeCommand ->
                    throwConfigError $ "Saw invalid vertical command in restricted horizontal mode: " <> show vModeCommand
                HP.HModeCommand (HP.AddCharacter c) ->
                    hModeAddCharacter c >>= modifyContents
                HP.HModeCommand (HP.AddHGlue g) ->
                    hModeAddHGlue g >>= modifyContents
                HP.HModeCommand (HP.AddHRule rule) ->
                    hModeAddRule rule >>= modifyContents
                HP.AddSpace ->
                    hModeAddSpace >>= modifyContents
                HP.StartParagraph indentFlag ->
                    hModeStartParagraph indentFlag >>= modifyContents
                -- \par: Restricted: does nothing. Unrestricted: ends mode.
                HP.EndParagraph ->
                    addElems []
                HP.ModeIndependentCommand modeIndependentCommand ->
                    handleModeIndependentCommand modeIndependentCommand >>= \case
                        AddElems extraElems ->
                            addElems (BL.HVListElem <$> extraElems)
                        EnterBoxMode desiredLength boxType boxIntent ->
                            setChild (boxTypeToFreshHChild desiredLength boxType boxIntent)
                        FinishBoxMode ->
                            pure (HBoxEndHBox boxHList)
                oth ->
                    panic $ "Not implemented, outer V mode: " <> show oth
        Just hOrVBox ->
            handleCommandInHSubBox hOrVBox command oldStream >>= \case
                HPopAndAddSubBox elems ->
                    modifyContents (hModeWithAddedElems elems . hModeWithoutChild)
                HPop ->
                    modifyContents hModeWithoutChild
                HModBox newHOrVBox ->
                    setChild newHOrVBox
  where
    modifyContents f = pure $ HBoxModifyHBox (f hModeContents)

    addElems elems = modifyContents (hModeWithAddedElems elems)

    setChild x = modifyContents (hModeWithSetChild x)

data VBoxModeResult
    = VBoxEndVBox VList
    | VBoxModifyVBox VModeContents

handleCommandInVBoxMode :: HP.InhibitableStream s => VModeContents -> HP.Command -> s -> ExceptMonadBuild s VBoxModeResult
handleCommandInVBoxMode vModeContents@(VModeContents boxVList maybeChild) command oldStream =
    case maybeChild of
        Nothing ->
            case command of
                -- End recursion.
                HP.VModeCommand HP.End ->
                    throwConfigError "End not allowed in internal vertical mode"
                HP.VModeCommand (HP.AddVGlue g) ->
                    vModeAddVGlue g >>= modifyContents
                HP.VModeCommand (HP.AddVRule rule) ->
                    vModeAddRule rule >>= modifyContents
                HP.HModeCommand _ ->
                    startParagraph HP.Indent
                HP.StartParagraph indentFlag ->
                    startParagraph indentFlag
                -- \par does nothing in vertical mode.
                HP.EndParagraph ->
                    addElems []
                -- <space token> has no effect in vertical modes.
                HP.AddSpace ->
                    addElems []
                HP.ModeIndependentCommand modeIndependentCommand ->
                    handleModeIndependentCommand modeIndependentCommand >>= \case
                        AddElems extraElems ->
                            addElems extraElems
                        EnterBoxMode desiredLength boxType boxIntent ->
                            setChild (boxTypeToFreshVChild desiredLength boxType boxIntent)
                        FinishBoxMode ->
                            pure (VBoxEndVBox boxVList)
                oth ->
                    panic $ "Not implemented, outer V mode: " <> show oth
              where
                startParagraph indentFlag =
                    do
                    -- If the command shifts to horizontal mode, run
                    -- '\indent', and re-read the stream as if the
                    -- command hadn't been read. (Note that we set
                    -- "oldStream", not "newStream".)
                    put oldStream
                    paraHList <- case indentFlag of
                        HP.Indent ->
                            do
                            b <- readOnConfState $ asks parIndentBox
                            pure [b]
                        HP.DoNotIndent ->
                            pure []
                    setChild (ParaOrBoxPara (HModeContents paraHList Nothing))
        Just (ParaOrBoxPara paraHModeContents@(HModeContents paraHList _)) ->
            handleCommandInParaMode paraHModeContents command oldStream >>= \case
                ParaEndPara ->
                    do
                    mainVListWithParagraph <- appendParagraphAndPop paraHList
                    pure $ VBoxModifyVBox (VModeContents mainVListWithParagraph Nothing)
                ParaModifyPara newParaHModeContents ->
                    setChild (ParaOrBoxPara newParaHModeContents)
                ParaLeaveBox ->
                    VBoxEndVBox <$> appendParagraphAndPop paraHList
        Just (ParaOrBoxBox hOrVBox) ->
            handleCommandInVSubBox hOrVBox command oldStream >>= \case
                VPopAndAddSubBox elems ->
                    modifyContents (vModeWithAddedElems elems . vModeWithoutChild)
                VPop ->
                    modifyContents vModeWithoutChild
                VModBox newHOrVBox ->
                    setChild (ParaOrBoxBox newHOrVBox)
  where
    modifyContents f = pure $ VBoxModifyVBox (f vModeContents)

    addElems elems = modifyContents (vModeWithAddedElems elems)

    setChild x = modifyContents (vModeWithSetChild x)

    appendParagraphAndPop paraHList =
        do
        desiredW <- readOnConfState $ asks $ lookupLengthParameter HP.HSize
        lineBoxes <- readOnConfState $
            do
            lineTol <- asks $ IntParamVal . lookupIntegerParameter HP.Tolerance
            linePen <- asks $ IntParamVal . lookupIntegerParameter HP.LinePenalty
            liftEither $ ConfigError `mapLeft` BL.breakAndSetParagraph (LenParamVal desiredW) lineTol linePen paraHList
        let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
        HP.runConfState $ foldM addVListElem boxVList $ BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes

data MainVModeResult
    = MainVEnd VList
    | MainVModifyVList VModeContents

handleCommandInMainVMode :: HP.InhibitableStream s => VModeContents -> HP.Command -> s -> ExceptMonadBuild s MainVModeResult
handleCommandInMainVMode vModeContents@(VModeContents mainVList maybeChild) command oldStream =
    case maybeChild of
        -- Outer vertical mode.
        Nothing ->
            case command of
                -- End recursion.
                HP.VModeCommand HP.End ->
                    pure (MainVEnd mainVList)
                HP.VModeCommand (HP.AddVGlue g) ->
                    vModeAddVGlue g >>= modifyContents
                HP.VModeCommand (HP.AddVRule rule) ->
                    vModeAddRule rule >>= modifyContents
                HP.HModeCommand _ ->
                    startParagraph HP.Indent
                HP.StartParagraph indentFlag ->
                    startParagraph indentFlag
                -- \par does nothing in vertical mode.
                HP.EndParagraph ->
                    addElems []
                -- <space token> has no effect in vertical modes.
                HP.AddSpace ->
                    addElems []
                HP.ModeIndependentCommand modeIndependentCommand ->
                    handleModeIndependentCommand modeIndependentCommand >>= \case
                        AddElems extraElems ->
                            addElems extraElems
                        EnterBoxMode desiredLength boxType boxIntent ->
                            setChild (boxTypeToFreshVChild desiredLength boxType boxIntent)
                        FinishBoxMode ->
                            throwConfigError "No box to end: in main V mode"
                oth ->
                    panic $ "Not implemented, outer V mode: " <> show oth
              where
                startParagraph indentFlag =
                    do
                    -- If the command shifts to horizontal mode, run
                    -- '\indent', and re-read the stream as if the
                    -- command hadn't been read. (Note that we set
                    -- "oldStream", not "newStream".)
                    put oldStream
                    paraHList <- case indentFlag of
                        HP.Indent ->
                            do
                            b <- readOnConfState $ asks parIndentBox
                            pure [b]
                        HP.DoNotIndent ->
                            pure []
                    setChild (ParaOrBoxPara (HModeContents paraHList Nothing))
        Just (ParaOrBoxPara paraHModeContents@(HModeContents paraHList _)) ->
            handleCommandInParaMode paraHModeContents command oldStream >>= \case
                ParaEndPara ->
                    do
                    desiredW <- readOnConfState $ asks $ lookupLengthParameter HP.HSize
                    lineBoxes <- readOnConfState $
                        do
                        lineTol <- asks $ IntParamVal . lookupIntegerParameter HP.Tolerance
                        linePen <- asks $ IntParamVal . lookupIntegerParameter HP.LinePenalty
                        liftEither $ ConfigError `mapLeft` BL.breakAndSetParagraph (LenParamVal desiredW) lineTol linePen paraHList
                    let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
                    mainVListWithParagraph <- HP.runConfState $
                        foldM addVListElem mainVList $ BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes
                    pure $ MainVModifyVList (VModeContents mainVListWithParagraph Nothing)
                ParaModifyPara newParaHModeContents ->
                    setChild (ParaOrBoxPara newParaHModeContents)
                ParaLeaveBox ->
                    throwConfigError "No box to end: in paragraph within main V mode"
        Just (ParaOrBoxBox hOrVBox) ->
            handleCommandInVSubBox hOrVBox command oldStream >>= \case
                VPopAndAddSubBox elems ->
                    modifyContents (vModeWithAddedElems elems . vModeWithoutChild)
                VPop ->
                    modifyContents vModeWithoutChild
                VModBox newHOrVBox ->
                    setChild (ParaOrBoxBox newHOrVBox)
  where
    modifyContents f = pure $ MainVModifyVList (f vModeContents)

    addElems elems = modifyContents (vModeWithAddedElems elems)

    setChild x = modifyContents (vModeWithSetChild x)

data VSubBoxCommandResult
    = VPopAndAddSubBox [BL.BreakableVListElem]
    | VPop
    | VModBox HOrVBox

handleCommandInVSubBox
    :: HP.InhibitableStream s
    => HOrVBox
    -> HP.Command
    -> s
    -> ExceptMonadBuild s VSubBoxCommandResult
handleCommandInVSubBox (HOrVBox desiredLength boxIntent hOrVModeContents) command oldStream =
    case hOrVModeContents of
        HOrVModeContentsH hModeContents ->
            handleCommandInHBoxMode hModeContents command oldStream >>= \case
                HBoxEndHBox finalHList ->
                    -- TODO: I think glue status and desired length
                    -- duplicate some meaning.
                    let
                        hBoxElems = (BL.setHList BL.NaturallyGood . reverse) finalHList
                        setBox = B.Box (B.HBoxContents hBoxElems) desiredLength
                    in
                        case boxIntent of
                            IntentToAddBox ->
                                pure $ VPopAndAddSubBox [BL.VListBaseElem $ B.ElemBox setBox]
                            IntentToSetBoxRegister idx global ->
                                do
                                modConfState $ setBoxRegister idx setBox global
                                pure VPop
                HBoxModifyHBox newBoxHModeContents ->
                    pure $ VModBox (HOrVBox desiredLength boxIntent (HOrVModeContentsH newBoxHModeContents))
        HOrVModeContentsV vAlignType vModeContents ->
            handleCommandInVBoxMode vModeContents command oldStream >>= \case
                VBoxEndVBox finalVList ->
                    -- TODO: I think glue status and desired length
                    -- duplicate some meaning.
                    let
                        vBoxElems = (BL.setVList BL.NaturallyGood . reverse) finalVList
                        setBox = B.Box (B.VBoxContents vBoxElems vAlignType) desiredLength
                    in
                        case boxIntent of
                            IntentToAddBox ->
                                pure $ VPopAndAddSubBox [BL.VListBaseElem $ B.ElemBox setBox]
                            IntentToSetBoxRegister idx global ->
                                do
                                modConfState $ setBoxRegister idx setBox global
                                pure VPop
                VBoxModifyVBox newBoxVModeContents ->
                    pure $ VModBox (HOrVBox desiredLength boxIntent (HOrVModeContentsV vAlignType newBoxVModeContents))

data HSubBoxCommandResult
    = HPopAndAddSubBox [BL.BreakableHListElem]
    | HPop
    | HModBox HOrVBox

handleCommandInHSubBox
    :: HP.InhibitableStream s
    => HOrVBox
    -> HP.Command
    -> s
    -> ExceptMonadBuild s HSubBoxCommandResult
handleCommandInHSubBox hOrVBox command oldStream =
    g <$> handleCommandInVSubBox hOrVBox command oldStream
  where
    g = \case
            VPopAndAddSubBox vListElems ->
                HPopAndAddSubBox (BL.HVListElem <$> vListElems)
            VPop ->
                HPop
            VModBox b ->
                HModBox b

extractBreakAndSetVList :: HP.InhibitableStream s => VModeContents -> ExceptMonadBuild s [B.Page]
extractBreakAndSetVList mainVModeContents =
    do
    oldStream <- get
    (P.State { P.stateInput = newStream }, command) <- liftEither $ ParseError `mapLeft` HP.extractCommand oldStream
    traceShow (summariseMainVMode mainVModeContents) $ put newStream
    handleCommandInMainVMode mainVModeContents command oldStream >>= \case
        MainVEnd finalMainVList ->
            do
            gets HP.getConditionBodyState >>= \case
                Nothing -> pure ()
                Just _condState -> throwConfigError $ "Cannot end: in condition block: " <> showT _condState
            readOnConfState (asks finaliseConfig) >>= liftIO
            desiredH <- HP.runConfState $ gets $ LenParamVal . lookupLengthParameter HP.VSize
            pure $ BL.runPageBuilder desiredH BL.newCurrentPage $ reverse finalMainVList
        MainVModifyVList newModeState ->
            extractBreakAndSetVList newModeState
  where
    summariseMainVMode x = ("vmain" :) (summariseVMode x)

    summariseVMode :: VModeContents -> [Text]
    summariseVMode (VModeContents _ maybeChild) =
        case maybeChild of
            Nothing ->
                []
            Just (ParaOrBoxPara hmc) ->
                "hpara" : (summariseHMode hmc)
            Just (ParaOrBoxBox hOrVBox) ->
                summariseHOrVBox hOrVBox

    summariseHOrVBox (HOrVBox desLength boxIntent hOrVModeContents) =
        case hOrVModeContents of
            HOrVModeContentsH hmc ->
                summarise "hbox" : summariseHMode hmc
            HOrVModeContentsV vAlignType hmc ->
                summarise (alignStr vAlignType) : summariseVMode hmc
      where
        desLengthStr = case desLength of
            B.Natural ->
                mempty
            B.To lenTo ->
                " to " <> show lenTo
            B.Spread lenSpread ->
                " spread " <> show lenSpread

        intentStr = case boxIntent of
            IntentToAddBox ->
                mempty
            IntentToSetBoxRegister idx HP.Local ->
                " for box[" <> show idx <> "]"
            IntentToSetBoxRegister idx HP.Global ->
                " for global box[" <> show idx <> "]"

        alignStr = \case
                    B.DefaultAlign -> "vbox"
                    B.TopAlign -> "vtop"

        summarise boxStr = boxStr <> desLengthStr <> intentStr

    summariseHMode (HModeContents _ maybeHOrVBox) =
        case maybeHOrVBox of
            Nothing ->
                []
            Just hOrVBox ->
                summariseHOrVBox hOrVBox
