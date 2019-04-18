module HeX.Build where

import HeXlude

import           Control.Monad                  ( foldM
                                                , when )
import           Control.Monad.Except           ( liftEither )
import           Data.Either.Combinators        ( mapLeft )
import           Safe                           ( toEnumMay )

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.BuildHelp
import           HeX.Config
import qualified HeX.Lex                       as Lex
import           HeX.Evaluate
import qualified HeX.Parse                     as HP
import qualified HeX.Unit                      as Unit
import qualified HeX.Command                   as Com
import qualified HeX.Variable                  as Var

-- constructBox :: HP.InhibitableStream s => HP.Box -> ExceptBuildVM s (Maybe B.Box)
-- constructBox = \case
--     HP.ExplicitBox spec boxType ->
--         do
--         eSpec <- liftEvalOnConfState spec
--         modConfState $ pushGroup ExplicitBoxGroup
--         b <- case boxType of
--             HP.ExplicitHBox ->
--                 (B.HBoxContents . BL.setHList BL.NaturallyGood . reverse) <$> extractHList HP.DoNotIndent True
--             HP.ExplicitVBox alignType ->
--                 ((\els -> B.VBoxContents els alignType) . BL.setVList BL.NaturallyGood . reverse) <$> extractVList True
--         pure $ Just $ B.Box b eSpec
--     HP.FetchedRegisterBox fetchMode idx ->
--         do
--         eIdx <- liftEvalOnConfState idx
--         box <- readOnConfState $ asks $ lookupBoxRegister eIdx
--         case fetchMode of
--             HP.Lookup -> pure ()
--             HP.Pop -> HP.runConfState $ modify $ delBoxRegister eIdx HP.Local
--         pure box

-- If the box is null, delete the target register's value.
-- Otherwise, set the register value to the non-null box.
maySetBoxRegister :: EightBitInt -> HP.GlobalFlag -> Maybe B.Box -> Config -> Config
maySetBoxRegister idx global = \case
    Nothing -> delBoxRegister idx global
    Just b -> setBoxRegister idx b global

data ModeIndependentResult
    = AddElems VList
    | LeaveExplicitBoxGroup ExplicitBoxGroup
    | StartExplicitB  ox B.DesiredLength HP.ExplicitBox ExplicitBoxContext

data ExplicitBoxContext
    = SetBoxRegister HP.GlobalFlag EightBitInt
    | AddBox

class MonadBuild m where
    modConf :: (Config -> Config) -> m ()

instance HP.InhibitableStream s => MonadBuild (VM s) where
    modConf = modConfState

handleModeIndep :: (HP.InhibitableStream s, MonadState s m, MonadIO m, MonadBuild m)
                => HP.ModeIndependentCommand -> ExceptBuildT s m ModeIndependentResult
handleModeIndep = \case
    HP.ChangeScope (HP.Sign True) trig ->
        do
        modConfState $ pushGroup $ LocalStructureGroup trig newLocalScope
        noElems
    HP.ChangeScope (HP.Sign False) trig ->
        HP.runConfState $
            gets popGroup >>= \case
                Nothing -> throwConfigError "No group to leave"
                Just (grp, conf') ->
                    do
                    put conf'
                    case grp of
                        LocalStructureGroup trigConf _ ->
                            do
                            when (trigConf /= trig) $ throwConfigError $ "Entry and exit group triggers differ: " <> showT (trig, trigConf)
                            noElems
                        ExplicitBoxGroup boxGroup ->
                            pure $ LeaveExplicitBoxGroup boxGroup
    HP.Message stdOutStream eTxt ->
        do
        let _handle = case stdOutStream of
                HP.StdOut -> stdout
                HP.StdErr -> stderr
        liftIO $ hPutStrLn _handle $ Com.showExpandedBalancedText eTxt
        noElems
    HP.Relax ->
        noElems
    HP.IgnoreSpaces ->
        noElems
    -- Re-insert the ⟨token⟩ into the input just after running the next
    -- assignment command. Later \afterassignment commands override earlier
    -- commands. If the assignment is a \setbox, and if the assigned ⟨box⟩ is
    -- \{hbox,vbox,vtop}, insert the ⟨token⟩ just after the '{' in the box
    -- construction (not after the '}'). Insert the ⟨token⟩ just before tokens
    -- inserted by \everyhbox or \everyvbox.
    HP.SetAfterAssignmentToken lt ->
        do
        modConfState $ \conf -> conf{afterAssignmentToken = Just lt}
        noElems
    HP.AddPenalty n ->
        do
        p <- liftEvalOnConfState n
        pure $ AddElems [BL.ListPenalty $ BL.Penalty p]
    HP.AddKern ln ->
        do
        k <- liftEvalOnConfState ln
        pure $ AddElems [BL.VListBaseElem $ B.ElemKern $ B.Kern k]
    HP.AddGlue g ->
        do
        eG <- liftEvalOnConfState g
        pure $ AddElems [BL.ListGlue eG]
    HP.Assign HP.Assignment { HP.global, HP.body } ->
        do
        result <- case body of
            HP.DefineControlSequence cs tgt ->
                do
                (acc, newCSTok) <- liftReadOnConfState $ case tgt of
                    HP.MacroTarget macro ->
                        pure ([], HP.syntaxTok $ HP.MacroTok macro)
                    -- TODO: If a \let target is an active character, should we
                    -- treat it as a control sequence, or a char-cat pair?
                    HP.LetTarget (Lex.CharCatToken tgtCC) ->
                        pure ([], HP.primTok $ HP.LetCharCat tgtCC)
                    HP.LetTarget (Lex.ControlSequenceToken tgtCS) ->
                        do
                        resTok <- fromMaybe (HP.PrimitiveToken HP.RelaxTok) <$> asks (lookupCSProper tgtCS)
                        pure ([], resTok)
                    HP.ShortDefineTarget q n ->
                        do
                        en <- texEvaluate n
                        pure ([], HP.primTok $ HP.IntRefTok q en)
                    HP.FontTarget fontSpec fPath ->
                        do
                        fontDef@B.FontDefinition { B.fontNr } <- Com.loadFont fPath fontSpec
                        let fontRefTok = HP.primTok $ HP.FontRefToken fontNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure ([boxElem], fontRefTok)
                liftIO $ putText $ "Setting CS " <> showT cs <> " to token: " <> showT newCSTok <> (if global == HP.Global then " globally" else " locally")
                modConfState $ setControlSequence cs newCSTok global
                pure $ AddElems acc
            HP.SetVariable ass ->
                do
                liftConfState $ case ass of
                    HP.IntegerVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.LengthVariableAssignment v tgt  ->
                        Var.setValueFromAST v global tgt
                    HP.GlueVariableAssignment v tgt    ->
                        Var.setValueFromAST v global tgt
                    HP.MathGlueVariableAssignment v tgt  ->
                        Var.setValueFromAST v global tgt
                    HP.TokenListVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.SpecialIntegerVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                    HP.SpecialLengthVariableAssignment v tgt ->
                        Var.setValueFromAST v global tgt
                noElems
            HP.ModifyVariable modCommand ->
                do
                liftConfState $ case modCommand of
                    HP.AdvanceIntegerVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceLengthVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceGlueVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.AdvanceMathGlueVariable var plusVal ->
                        Var.advanceValueFromAST var global plusVal
                    HP.ScaleVariable vDir numVar scaleVal ->
                        case numVar of
                            HP.IntegerNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                            HP.LengthNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                            HP.GlueNumericVariable var ->
                                Var.scaleValueFromAST var global vDir scaleVal
                noElems
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- liftEvalOnConfState idx
                eVal <- liftEvalOnConfState val
                liftIO $ putText $ "Evaluated code table index " <> showT idx <> " to " <> showT eIdx
                liftIO $ putText $ "Evaluated code table value " <> showT val <> " to " <> showT eVal
                idxChar <- liftMaybeConfigError ("Invalid character code index: " <> showT eIdx) (toEnumMay eIdx)
                liftIO $ putText $ "Setting " <> showT codeType <> "@" <> showT eIdx <> " (" <> showT idxChar <> ") to " <> showT eVal
                liftConfState $ updateCharCodeMap codeType idxChar eVal global
                noElems
            HP.SelectFont fNr ->
                do
                HP.runConfState $ Com.selectFont fNr global
                pure $ AddElems
                    [BL.VListBaseElem $ B.ElemFontSelection $ B.FontSelection fNr]
            HP.SetFamilyMember fm fontRef ->
                do
                eFm <- liftEvalOnConfState fm
                fNr <- liftEvalOnConfState fontRef
                modConfState $ setFamilyMemberFont eFm fNr global
                noElems
            HP.SetBoxRegister idx box ->
                do
                eIdx <- liftEvalOnConfState idx
                case box of
                    HP.ExplicitBox spec boxType ->
                        do
                        eSpec <- liftEvalOnConfState spec
                        pure $ StartExplicitBox eSpec boxType $ SetBoxRegister global eIdx
                        -- b <- case boxType of
                        --     HP.ExplicitHBox ->
                        --         (B.HBoxContents . BL.setHList BL.NaturallyGood . reverse) <$> extractHList HP.DoNotIndent True
                        --     HP.ExplicitVBox alignType ->
                        --         ((\els -> B.VBoxContents els alignType) . BL.setVList BL.NaturallyGood . reverse) <$> extractVList True
                        -- pure $ Just $ B.Box b eSpec
                    HP.FetchedRegisterBox fetchMode fetchIdx ->
                        do
                        fetchedBox <- fetchBox fetchIdx fetchMode
                        modConfState $ maySetBoxRegister eIdx global fetchedBox
                        noElems
            HP.SetFontChar (HP.FontCharRef fontChar fontRef) charRef ->
                do
                fNr <- liftEvalOnConfState fontRef
                eCharRef <- liftEvalOnConfState charRef
                let updateFontChar f = case fontChar of
                        HP.SkewChar -> f { skewChar = eCharRef }
                        HP.HyphenChar -> f { hyphenChar = eCharRef }
                HP.runConfState $ modifyFont fNr updateFontChar
                noElems
            oth ->
                panic $ show oth
        HP.runConfState (gets afterAssignmentToken) >>= \case
            Nothing -> pure ()
            Just lt ->
                do
                modify $ \s -> HP.insertLexToken s lt
                modConfState $ \c -> c{afterAssignmentToken = Nothing}
        pure result
    HP.WriteToStream n (HP.ImmediateWriteText eTxt) ->
        do
        liftReadOnConfState $ do
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
        noElems
    HP.AddBox HP.NaturalPlacement box ->
        case box of
            HP.ExplicitBox spec boxType ->
                do
                eSpec <- liftEvalOnConfState spec
                pure $ StartExplicitBox eSpec boxType AddBox
            HP.FetchedRegisterBox fetchMode fetchIdx ->
                do
                fetchedBox <- fetchBox fetchIdx fetchMode
                pure $ AddElems $ case fetchedBox of
                    Nothing -> []
                    Just b  -> [(BL.VListBaseElem . B.ElemBox) b]
    oth ->
        panic $ show oth
  where
    noElems :: Monad m => m ModeIndependentResult
    noElems = pure $ AddElems []

    fetchBox idx fetchMode =
        do
        eIdx <- liftEvalOnConfState idx
        fetchedBox <- lookupBoxRegister eIdx & asks & readOnConfState
        case fetchMode of
            HP.Lookup -> pure ()
            HP.Pop -> HP.runConfState $ modify $ delBoxRegister eIdx HP.Local
        pure fetchedBox

-- processHCommand
--     :: HP.InhibitableStream s
--     => s
--     -> HList
--     -> Bool  -- restricted?
--     -> HP.HModeCommand
--     -> ExceptBuildVM s (HList, Bool)
-- processHCommand oldStream acc inRestricted = \case
--     HP.LeaveHMode ->
--         if inRestricted
--             then throwConfigError "Should not see vertical command in restricted horizontal mode"
--             -- Outer mode: insert the control sequence "\par" into the input. The control
--             -- sequence's current meaning will be used, which might no longer be the \par
--             -- primitive.
--             -- (Note that we use oldStream.)
--             else
--                 do
--                 let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
--                 put $ HP.insertLexToken oldStream parToken
--                 continueUnchanged
--     HP.AddCharacter c ->
--         liftConfigError $ do
--         charCode <- readOnConfState $ texEvaluate c
--         hCharBox <- (BL.HListHBaseElem . B.ElemCharacter) <$> (readOnConfState $ Com.characterBox charCode)
--         modAccum $ hCharBox : acc
--     HP.HAllModesCommand aCom -> case aCom of
--         -- \indent: An empty box of width \parindent is appended to the current
--         -- list, and the space factor is set to 1000.
--         -- TODO: Space factor.
--         HP.StartParagraph HP.DoNotIndent ->
--             continueUnchanged
--         HP.StartParagraph HP.Indent ->
--             liftConfigError $ do
--             indentBox <- readOnConfState $ asks parIndentBox
--             modAccum (indentBox : acc)
--         -- \par: Restricted: does nothing. Unrestricted: ends mode.
--         HP.EndParagraph ->
--             if inRestricted
--                 then continueUnchanged
--                 else pure (acc, False)
--         HP.AddSpace ->
--             liftConfigError $ do
--             hGlue <- (BL.HVListElem . BL.ListGlue) <$> readOnConfState Com.spaceGlue
--             modAccum $ hGlue : acc
--         HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
--             liftReadOnConfState $ do
--             evalW <- case width of
--                 Nothing -> pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
--                 Just ln -> texEvaluate ln
--             evalH <- case height of
--                 Nothing -> pure $ Unit.toScaledPointApprox (10 :: Int) Unit.Point
--                 Just ln -> texEvaluate ln
--             evalD <- case depth of
--                 Nothing -> pure 0
--                 Just ln -> texEvaluate ln
--             let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
--             modAccum $ (BL.HVListElem $ BL.VListBaseElem $ B.ElemRule rule) : acc
--         HP.ModeIndependentCommand mcom ->
--             handleModeIndep mcom >>= \case
--                 Nothing ->
--                     pure (acc, False)
--                 Just extraAcc ->
--                     modAccum $ (BL.HVListElem <$> extraAcc) <> acc
--   where
--     modAccum newAcc = pure (newAcc, True)

--     continueUnchanged = pure (acc, True)

-- extractHList
--     :: HP.InhibitableStream s
--     => HP.IndentFlag
--     -> Bool
--     -> ExceptBuildVM s HList
-- extractHList indentFlag inRestricted =
--     do
--     indentBox <- readOnConfState $ asks parIndentBox
--     extractList HP.extractHModeCommand processCommand [indentBox | indentFlag == HP.Indent]
--   where
--     processCommand s ac c =
--         do
--         let subModeTxt = if inRestricted
--             then "restricted"
--             else "unrestricted"
--         putText $ "In " <> subModeTxt <> " H-mode, saw command: " <> show c
--         processHCommand s ac inRestricted c

-- extractBreakAndSetHList
--     :: HP.InhibitableStream s
--     => HP.IndentFlag
--     -> ExceptBuildVM s [[B.HBoxElem]]
-- extractBreakAndSetHList indentFlag =
--     do
--     hList <- extractHList indentFlag False
--     readOnConfState $ do
--         desiredW <- asks $ LenParamVal . lookupLengthParameter HP.HSize
--         lineTol <- asks $ IntParamVal . lookupIntegerParameter HP.Tolerance
--         linePen <- asks $ IntParamVal . lookupIntegerParameter HP.LinePenalty
--         liftEither $ ConfigError `mapLeft` BL.breakAndSetParagraph desiredW lineTol linePen hList

-- Assume we are adding a non-rule box of height h to the vertical list.
-- Let \prevdepth = p, \lineskiplimit = l, \baselineskip = (b plus y minus z).
-- Add interline glue, above the new box, of:
-- If p ≤ −1000 pt:
--    No glue.
-- Otherwise, if b−p−h ≥ l:
--    (b−p−h) plus y minus z
-- Otherwise:
--    \lineskip
-- Then set \prevdepth to the depth of the new box.
addVListElem :: MonadState Config m
             => VList
             -> BL.BreakableVListElem
             -> m VList
addVListElem acc e = case e of
    (BL.VListBaseElem (B.ElemBox b)) -> addVListBox b
    _                                -> pure $ e : acc
  where
    addVListBox :: MonadState Config m => B.Box -> m VList
    addVListBox b =
        do
        _prevDepth <- gets $ lookupSpecialLength HP.PrevDepth
        BL.Glue blineLength blineStretch blineShrink <- gets $ lookupGlueParameter HP.BaselineSkip
        skipLimit <- gets $ lookupLengthParameter HP.LineSkipLimit
        skip <- gets $ lookupGlueParameter HP.LineSkip
        modify $ setSpecialLength HP.PrevDepth $ naturalDepth e
        pure $ if _prevDepth <= -Unit.oneKPt
            then e : acc
            else
                let proposedBaselineLength = blineLength - _prevDepth - naturalHeight b
                -- Intuition: set the distance between baselines to \baselineskip, but no
                -- closer than \lineskiplimit [theBaselineLengthMin], in which case
                -- \lineskip [theMinBaselineGlue] is used.
                    glue = BL.ListGlue $ if proposedBaselineLength >= skipLimit
                        then BL.Glue proposedBaselineLength blineStretch blineShrink
                        else skip
                in  e : glue : acc

-- processVCommand
--     :: HP.InhibitableStream s
--     => s
--     -> VList
--     -> Bool -- internal?
--     -> HP.VModeCommand
--     -> ExceptBuildVM s (VList, Bool)
-- processVCommand oldStream acc inInternal = \case
--     -- End recursion.
--     HP.End ->
--         if inInternal
--             then throwConfigError "End not allowed in internal vertical mode"
--         else
--             do
--             readOnConfState $ asks scopedConfig >>= \case
--                 [] -> pure ()
--                 _ -> throwConfigError "Cannot end: not in global scope"
--             gets HP.getConditionBodyState >>= \case
--                 Nothing -> pure ()
--                 Just _condState -> throwConfigError $ "Cannot end: in condition block: " <> showT _condState
--             readOnConfState (asks finaliseConfig) >>= liftIO
--             pure (acc, False)
--     HP.EnterHMode ->
--         addParagraphToPage HP.Indent
--     HP.VAllModesCommand aCom -> case aCom of
--         HP.StartParagraph indentFlag ->
--             addParagraphToPage indentFlag
--         -- \par does nothing in vertical mode.
--         HP.EndParagraph ->
--             continueUnchanged
--         -- <space token> has no effect in vertical modes.
--         HP.AddSpace ->
--             continueUnchanged
--         HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
--             do
--             evalW <- case width of
--                 Nothing -> readOnConfState $ gets $ lookupLengthParameter HP.HSize
--                 Just ln -> liftEvalOnConfState ln
--             evalH <- case height of
--                 -- TODO.
--                 Nothing -> readOnConfState $ pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
--                 Just ln -> liftEvalOnConfState ln
--             evalD <- case depth of
--                 Nothing -> readOnConfState $ pure 0
--                 Just ln -> liftEvalOnConfState ln
--             let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
--             modAccum $ (BL.VListBaseElem $ B.ElemRule rule) : acc
--         HP.ModeIndependentCommand mcom ->
--             handleModeIndep mcom >>= \case
--                 Nothing ->
--                     pure (acc, False)
--                 Just extraAcc ->
--                     modAccum $ extraAcc <> acc
--   where
--     modAccum newAcc = pure (newAcc, True)

--     continueUnchanged = pure (acc, True)

--     addParagraphToPage indentFlag =
--         do
--         -- If the command shifts to horizontal mode, run '\indent', and re-read the
--         -- stream as if the commands just seen hadn't been read.
--         -- (Note that we set "oldStream", not "newStream".)
--         -- Paraboxes are returned in reading order.
--         put oldStream
--         lineBoxes <- extractBreakAndSetHList indentFlag
--         desiredW <- readOnConfState $ asks $ lookupLengthParameter HP.HSize
--         let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
--         newAcc <- HP.runConfState $ foldM addVListElem acc $ BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes
--         -- Continue iff not in internal mode.
--         pure (newAcc, not inInternal)

-- extractVList
--     :: HP.InhibitableStream s
--     => Bool
--     -> ExceptBuildVM s VList
-- extractVList inInternal =
--     extractList HP.extractVModeCommand processCommand []
--   where
--     processCommand s ac c =
--         do
--         let subModeTxt = if inInternal
--             then "internal"
--             else "outer"
--         putText $ "In " <> subModeTxt <> " V-mode, saw command: " <> show c
--         processVCommand s ac inInternal c

extractBreakAndSetVList :: HP.InhibitableStream s => ExceptBuildVM s [B.Page]
extractBreakAndSetVList = do
    -- vList <- extractVList False
    vList <- processCommand
    desiredH <- HP.runConfState $ gets $ LenParamVal . lookupLengthParameter HP.VSize
    pure $ BL.runPageBuilder desiredH BL.newCurrentPage $ reverse vList

processCommand :: forall s. HP.InhibitableStream s => ExceptBuildVM s VList
processCommand =
    do
    (boxGroups, mayOuterHList) <- readOnConfState $ do
        boxGroups <- asks groups <&> filter isBoxGroup
        mayOuterHList <- asks mainHList
        pure (boxGroups, mayOuterHList)
    oldStream <- get
    case (mayOuterHList, boxGroups) of
        -- Outer vertical mode.
        (Nothing, []) ->
            do
            sth HP.extractVModeCommand >>= \case
                -- End recursion.
                HP.End ->
                    do
                    gets HP.getConditionBodyState >>= \case
                        Nothing -> pure ()
                        Just _condState -> throwConfigError $ "Cannot end: in condition block: " <> showT _condState
                    readOnConfState (asks finaliseConfig) >>= liftIO
                    HP.runConfState $ gets mainVList
                HP.EnterHMode ->
                    enterHMode HP.Indent
                HP.VAllModesCommand aCom -> case aCom of
                    HP.StartParagraph indentFlag ->
                        enterHMode indentFlag
                    -- \par does nothing in vertical mode.
                    HP.EndParagraph ->
                        continueUnchanged
                    -- <space token> has no effect in vertical modes.
                    HP.AddSpace ->
                        continueUnchanged
                    HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
                        do
                        evalW <- case width of
                            Nothing -> readOnConfState $ gets $ lookupLengthParameter HP.HSize
                            Just ln -> liftEvalOnConfState ln
                        evalH <- case height of
                            -- TODO.
                            Nothing -> readOnConfState $ pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                            Just ln -> liftEvalOnConfState ln
                        evalD <- case depth of
                            Nothing -> readOnConfState $ pure 0
                            Just ln -> liftEvalOnConfState ln
                        let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
                        modAccum [BL.VListBaseElem $ B.ElemRule rule]
                    HP.ModeIndependentCommand mcom ->
                        handleModeIndep mcom >>= \case
                            -- TODO: This should be impossible, make it so by
                            -- construction.
                            LeaveExplicitBoxGroup _ ->
                                throwConfigError "Saw unexpected end-explicit-box in main vertical mode"
                            AddElems extraVListElems  ->
                                modAccum $ extraVListElems
              where
                modAccum newElems =
                    do
                    modConfState (\c@Config { mainVList } -> c { mainVList = newElems <> mainVList })
                    processCommand

                enterHMode indentFlag =
                    do
                    -- If the command shifts to horizontal mode, run '\indent', and re-read the
                    -- stream as if the commands just seen hadn't been read.
                    -- (Note that we set "oldStream", not "newStream".)
                    -- Paraboxes are returned in reading order.
                    put oldStream
                    initialOuterHList <- case indentFlag of
                        HP.Indent ->
                            do
                            indentBox <- readOnConfState $ asks parIndentBox
                            pure [indentBox]
                        HP.DoNotIndent ->
                            pure []
                    modConfState (\c -> c { mainHList = Just initialOuterHList })
                    processCommand
        -- Unrestricted horizontal mode.
        (Just outerHList, []) ->
            do
            sth HP.extractHModeCommand >>= \case
                HP.LeaveHMode ->
                    do
                    let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
                    put $ HP.insertLexToken oldStream parToken
                    continueUnchanged
                HP.AddCharacter c ->
                    do
                    charCode <- liftReadOnConfState $ texEvaluate c
                    hCharBox <- liftConfigError $ (BL.HListHBaseElem . B.ElemCharacter) <$> (readOnConfState $ Com.characterBox charCode)
                    modAccum [hCharBox]
                HP.HAllModesCommand aCom -> case aCom of
                    -- \indent: An empty box of width \parindent is appended to the current
                    -- list, and the space factor is set to 1000.
                    -- TODO: Space factor.
                    HP.StartParagraph HP.DoNotIndent ->
                        continueUnchanged
                    HP.StartParagraph HP.Indent ->
                        do
                        indentBox <- liftReadOnConfState $ asks parIndentBox
                        modAccum [indentBox]
                    -- \par: Restricted: does nothing. Unrestricted: ends mode.
                    HP.EndParagraph ->
                        endMode
                    HP.AddSpace ->
                        do
                        hGlue <- (BL.HVListElem . BL.ListGlue) <$> liftReadOnConfState Com.spaceGlue
                        modAccum [hGlue]
                    HP.AddRule HP.Rule { HP.width, HP.height, HP.depth } ->
                        do
                        evalW <- liftReadOnConfState $ case width of
                            Nothing -> pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                            Just ln -> texEvaluate ln
                        evalH <- liftReadOnConfState $ case height of
                            Nothing -> pure $ Unit.toScaledPointApprox (10 :: Int) Unit.Point
                            Just ln -> texEvaluate ln
                        evalD <- liftReadOnConfState $ case depth of
                            Nothing -> pure 0
                            Just ln -> texEvaluate ln
                        let rule = B.Rule { B.ruleWidth = evalW, B.ruleHeight = evalH, B.ruleDepth = evalD }
                        modAccum [BL.HVListElem $ BL.VListBaseElem $ B.ElemRule rule]
                    HP.ModeIndependentCommand mcom ->
                        handleModeIndep mcom >>= \case
                            LeaveExplicitBoxGroup _ ->
                                -- TODO: This should be impossible, make it so by
                                -- construction.
                                throwConfigError "Saw unexpected end-explicit-box in unrestricted horizontal mode"
                            AddElems extraHListElems ->
                                modAccum (BL.HVListElem <$> extraHListElems)
                            -- StartExplicitBox desiredLength boxType boxCtx ->
              where
                modAccum newElems =
                    do
                    modConfState (\c@Config { mainHList } -> c { mainHList = (newElems <>) <$> mainHList })
                    processCommand

                endMode =
                    do
                    newMainVList <- HP.runConfState $ do
                        desiredW <- gets $ LenParamVal . lookupLengthParameter HP.HSize
                        lineTol <- gets $ IntParamVal . lookupIntegerParameter HP.Tolerance
                        linePen <- gets $ IntParamVal . lookupIntegerParameter HP.LinePenalty
                        let toBox elemList = B.Box (B.HBoxContents elemList) (B.To $ unLenParam desiredW)
                        lineBoxes <- liftEither $ ConfigError `mapLeft` BL.breakAndSetParagraph desiredW lineTol linePen outerHList
                        -- Add boxes to outer vertical list
                        let hboxes = (BL.VListBaseElem . B.ElemBox . toBox) <$> lineBoxes
                        curMainVList <- gets mainVList
                        newMainVList <- foldM addVListElem curMainVList hboxes
                        pure newMainVList
                    -- Return to outer vertical mode.
                    modConfState (\c -> c { mainVList = newMainVList
                                          , mainHList = Nothing })
                    processCommand
  where
    continueUnchanged = processCommand
