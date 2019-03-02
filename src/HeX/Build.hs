{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HeX.Build where

import           Control.Monad                  ( foldM
                                                , when )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , liftEither
                                                , throwError
                                                , withExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                , runReaderT
                                                )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT
                                                , get
                                                , gets
                                                , put
                                                , liftIO
                                                , modify
                                                )
import           Data.Either.Combinators        ( mapLeft )
import qualified Data.HashMap.Strict           as HMap
import           Data.Maybe                     ( fromMaybe )
import           Path
import           Safe                           ( headMay
                                                , toEnumMay )
import           System.IO                      ( Handle
                                                , hPutStrLn
                                                )
import qualified Text.Megaparsec               as PS

import           Data.Adjacent                  ( Adj(..) )
import           Data.Path                      ( findFilePath )

import qualified TFM
import           TFM                            ( TexFont(..) )
import qualified TFM.Character                 as TFMC

import           HeXPrelude
import           HeX.Type
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.BreakList.Line             ( breakAndSetParagraph )
import           HeX.BreakList.Page             ( PageBreakJudgment(..)
                                                , pageBreakJudgment
                                                , setPage
                                                )
import           HeX.Config
import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex
import           HeX.Evaluate
import qualified HeX.Parse                     as HP
import qualified HeX.Unit                      as Unit

loadFont
    :: (MonadState Config m, MonadIO m, MonadError String m)
    => Path Rel File
    -> HP.FontSpecification
    -> m B.FontDefinition
loadFont relPath fontSpec =
    do
    fontInfo_ <- readOnState $ readRelPath relPath
    let designSizeSP = TFM.designSizeSP $ fontMetrics fontInfo_
    scaleRatio <- readOnState $ evaluateFontSpecification designSizeSP fontSpec
    fontName <- extractFontName relPath
    liftIO $ putStrLn $ "Loading font: " ++ show fontName ++ ", with design size: " ++ show designSizeSP ++ ", with scale ratio: " ++ show scaleRatio
    fNr <- addFont fontInfo_
    pure B.FontDefinition
        { fontNr           = fNr
        -- TODO: Improve mapping of name and path.
        , fontPath         = fontName
        , fontName         = fontName
        , fontInfo         = fontMetrics fontInfo_
        , scaleFactorRatio = scaleRatio
        }
  where
    readRelPath p =
        findFilePath p <$> asks searchDirectories
        >>= liftThrow ("Could not find font: " ++ show p)
        >>= readFontInfo

    stripExtension p =
        liftThrow "Could not strip font extension" $ Path.setFileExtension "" p

    fileName = Path.filename >>> Path.toFilePath

    extractFontName p = stripExtension p <&> fileName

selectFont :: MonadState Config m => Int -> HP.GlobalFlag -> m B.FontSelection
selectFont n globalFlag =
    do
    modify $ selectFontNr n globalFlag
    pure $ B.FontSelection n

characterBox :: (MonadReader Config m, MonadError String m) => CharCode -> m B.Character
characterBox char =
    do
    fontMetrics <- currentFontMetrics
    let toSP = TFM.designScaleSP fontMetrics
    TFMC.Character {width = w, height = h, depth = d} <-
        liftMaybe "No such character" $ (HMap.lookup char $ characters fontMetrics)
    pure
        B.Character {char = char, width = toSP w, height = toSP h, depth = toSP d}

spaceGlue :: (MonadReader Config m, MonadError String m) => m BL.Glue
spaceGlue =
    do
    fontMetrics@TexFont{spacing = d, spaceStretch = str, spaceShrink = shr} <- currentFontMetrics
    let toSP = TFM.designScaleSP fontMetrics
        toFlex = toSP >>> fromIntegral >>> BL.finiteFlex
    pure BL.Glue {dimen = toSP d, stretch = toFlex str, shrink = toFlex shr}

readOnState :: MonadState r m => ReaderT r m b -> m b
readOnState f = get >>= runReaderT f

readOnConfState
    :: (MonadState HP.ExpandedStream m)
    => ReaderT Config (StateT Config m) a
    -> m a
readOnConfState f = HP.runConfState $ readOnState f

setIntegerVariable
    :: (MonadState Config m, MonadError String m)
    => HP.IntegerVariable -> HP.GlobalFlag -> IntVal -> m ()
setIntegerVariable v globalFlag tgt = case v of
    HP.ParamVar p       -> modify $ setIntegerParameter p tgt globalFlag
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> modify $ setIntegerRegister i tgt globalFlag)

setLengthVariable
    :: (MonadState Config m, MonadError String m)
    => HP.LengthVariable -> HP.GlobalFlag -> LenVal -> m ()
setLengthVariable v globalFlag tgt = case v of
    HP.ParamVar p       -> modify $ setLengthParameter p tgt globalFlag
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> modify $ setLengthRegister i tgt globalFlag)

setGlueVariable
    :: (MonadState Config m, MonadError String m)
    => HP.GlueVariable -> HP.GlobalFlag -> BL.Glue -> m ()
setGlueVariable v globalFlag tgt = case v of
    HP.ParamVar p       -> modify $ setGlueParameter p tgt globalFlag
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> modify $ setGlueRegister i tgt globalFlag)

setMathGlueVariable
    :: (MonadState Config m, MonadError String m)
    => HP.MathGlueVariable -> HP.GlobalFlag -> BL.MathGlue -> m ()
setMathGlueVariable v globalFlag tgt = case v of
    HP.ParamVar p       -> modify $ setMathGlueParameter p tgt globalFlag
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> modify $ setMathGlueRegister i tgt globalFlag)

setTokenListVariable
    :: (MonadState Config m, MonadError String m)
    => HP.TokenListVariable -> HP.GlobalFlag -> HP.BalancedText -> m ()
setTokenListVariable v globalFlag tgt = case v of
    HP.ParamVar p       -> modify $ setTokenListParameter p tgt globalFlag
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> modify $ setTokenListRegister i tgt globalFlag)

showLexTok :: Lex.Token -> String
showLexTok (Lex.CharCatToken (Lex.CharCat {char = c, cat = Lex.Letter})) = [c]
showLexTok (Lex.CharCatToken (Lex.CharCat {char = c, cat = Lex.Other})) = [c]
showLexTok (Lex.CharCatToken (Lex.CharCat {char = c, cat = Lex.Space})) = [c]
showLexTok (Lex.CharCatToken cc) = show cc
showLexTok (Lex.ControlSequenceToken (Lex.ControlSequence cs)) = "\\" ++ cs

showPrimTok :: HP.PrimitiveToken -> String
showPrimTok (HP.UnexpandedTok t) = showLexTok t
showPrimTok (HP.SubParserError err) = err
showPrimTok (HP.ResolutionError cs) = "Unknown control sequence: " ++ show cs
showPrimTok pt = show pt

showBalancedText :: HP.BalancedText -> String
showBalancedText (HP.BalancedText txt) = concatMap showLexTok txt

showExpandedBalancedText :: HP.ExpandedBalancedText -> String
showExpandedBalancedText (HP.ExpandedBalancedText txt) = concatMap showPrimTok txt

handleModeIndep
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.ModeIndependentCommand -> ExceptT BuildError m (Maybe [BL.BreakableVListElem])
handleModeIndep = \case
    HP.ChangeScope (HP.Sign True) trig ->
        do
        pushGroup' $ LocalStructureGroup trig
        noElems
    HP.ChangeScope (HP.Sign False) trig ->
        HP.runConfState $
            gets popGroup >>= \case
                Nothing -> throwConfigError "No group to leave"
                Just (grp, conf') ->
                    do
                    put conf'
                    case grp of
                        LocalStructureGroup trigConf ->
                            do
                            when (trigConf /= trig) $ throwConfigError $ "Entry and exit group triggers differ: " ++ show (trig, trigConf)
                            retElems []
                        ExplicitBoxGroup ->
                            pure Nothing
    HP.Message HP.Out eTxt ->
        do
        liftIO $ putStrLn $ showExpandedBalancedText eTxt
        noElems
    HP.Relax ->
        noElems
    HP.IgnoreSpaces ->
        noElems
    HP.AddPenalty n ->
        do
        p <- liftReadOnConfState $ evaluatePenalty n
        retElems [BL.ListPenalty p]
    HP.AddKern ln ->
        do
        k <- liftReadOnConfState $ evaluateKern ln
        retElems [BL.VListBaseElem $ B.ElemKern k]
    HP.AddGlue g ->
        do
        eG <- liftReadOnConfState $ evaluateGlue g
        retElems [BL.ListGlue eG]
    HP.Assign HP.Assignment{global = globalFlag, body = _body} ->
        case _body of
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
                        en <- evaluateNumber n
                        pure ([], HP.primTok $ HP.IntRefTok q en)
                    HP.FontTarget fontSpec fPath ->
                        do
                        fontDef@B.FontDefinition{fontNr=fNr} <- loadFont fPath fontSpec
                        let fontRefTok = HP.primTok $ HP.FontRefToken fNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure ([boxElem], fontRefTok)
                    oth -> throwError $ "Unimplemented: " ++ show oth
                liftIO $ putStrLn $ "Setting CS " ++ show cs ++ " to token: " ++ show newCSTok ++ (if globalFlag == HP.Global then " globally" else " locally")
                HP.runConfState $ modify $ setControlSequence cs newCSTok globalFlag
                retElems acc
            HP.SetVariable ass ->
                do
                liftConfState $ case ass of
                    HP.IntegerVariableAssignment v tgt ->
                        readOnState (evaluateNumber tgt) >>= setIntegerVariable v globalFlag
                    HP.LengthVariableAssignment v tgt  ->
                        readOnState (evaluateLength tgt) >>= setLengthVariable v globalFlag
                    HP.GlueVariableAssignment v tgt    ->
                        readOnState (evaluateGlue tgt) >>= setGlueVariable v globalFlag
                    HP.MathGlueVariableAssignment v tgt  ->
                        readOnState (evaluateMathGlue tgt) >>= setMathGlueVariable v globalFlag
                    HP.TokenListVariableAssignment v tgt ->
                        do
                        eTgt <- readOnState $ case tgt of
                            HP.TokenListAssignmentVar tgtVar   -> evaluateTokenListVariable tgtVar
                            HP.TokenListAssignmentText tgtText -> pure tgtText
                        setTokenListVariable v globalFlag eTgt
                    HP.SpecialIntegerVariableAssignment v tgt ->
                        readOnState (evaluateNumber tgt) >>= (\en -> modify $ setSpecialInteger v en)
                    HP.SpecialLengthVariableAssignment v tgt ->
                        readOnState (evaluateLength tgt) >>= (\en -> modify $ setSpecialLength v en)
                noElems
            HP.ModifyVariable modCommand ->
                do
                liftConfState $ case modCommand of
                    HP.AdvanceIntegerVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ (+) <$> evaluateIntegerVariable var <*> evaluateNumber plusVal
                        setIntegerVariable var globalFlag newVarVal
                    HP.AdvanceLengthVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ (+) <$> evaluateLengthVariable var <*> evaluateLength plusVal
                        setLengthVariable var globalFlag newVarVal
                    HP.AdvanceGlueVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ mappend <$> evaluateGlueVariable var <*> evaluateGlue plusVal
                        setGlueVariable var globalFlag newVarVal
                    HP.AdvanceMathGlueVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ mappend <$> evaluateMathGlueVariable var <*> evaluateMathGlue plusVal
                        setMathGlueVariable var globalFlag newVarVal
                    -- Division of a positive integer by a positive integer
                    -- discards the remainder, and the sign of the result
                    -- changes if you change the sign of either operand.
                    HP.ScaleVariable vDir numVar scaleVal ->
                        do
                        eScaleVal <- readOnState $ evaluateNumber scaleVal
                        case numVar of
                            HP.IntegerNumericVariable var ->
                                do
                                eVar <- readOnState $ evaluateIntegerVariable var
                                let op = case vDir of
                                        Upward -> (*)
                                        Downward -> quot
                                setIntegerVariable var globalFlag $ op eVar eScaleVal
                            HP.LengthNumericVariable var ->
                                do
                                eVar <- readOnState $ evaluateLengthVariable var
                                let op = case vDir of
                                        Upward -> (*)
                                        Downward -> quot
                                setLengthVariable var globalFlag $ op eVar eScaleVal
                            HP.GlueNumericVariable var ->
                                do
                                eVar <- readOnState $ evaluateGlueVariable var
                                let op = case vDir of
                                        Upward -> BL.multiplyGlue
                                        Downward -> BL.divGlue
                                setGlueVariable var globalFlag $ op eVar eScaleVal
                noElems
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- liftReadOnConfState $ evaluateNumber idx
                eVal <- liftReadOnConfState $ evaluateNumber val
                liftIO $ putStrLn $ "Evaluated code table index " ++ show idx ++ " to " ++ show eIdx
                liftIO $ putStrLn $ "Evaluated code table value " ++ show val ++ " to " ++ show eVal
                idxChar <- liftMaybeConfigError ("Invalid character code index: " ++ show eIdx) (toEnumMay eIdx)
                liftIO $ putStrLn $ "Setting " ++ show codeType ++ "@" ++ show eIdx ++ " (" ++ show idxChar ++ ") to " ++ show eVal
                liftConfState $ updateCharCodeMap codeType idxChar eVal globalFlag
                noElems
            HP.SelectFont fNr ->
                do
                fontSel <- HP.runConfState $ selectFont fNr globalFlag
                retElems [BL.VListBaseElem $ B.ElemFontSelection fontSel]
            HP.SetFamilyMember fm fontRef ->
                do
                eFm <- liftReadOnConfState $ evaluateFamilyMember fm
                fNr <- liftReadOnConfState $ evaluateFontRef fontRef
                HP.runConfState $ modify $ setFamilyMemberFont eFm fNr globalFlag
                noElems
            oth -> throwConfigError $ "Unimplemented: " ++ show oth
    HP.WriteToStream n (HP.ImmediateWriteText eTxt) ->
        liftReadOnConfState $ do
        en <- evaluateNumber n
        fStreams <- asks outFileStreams
        let txtStr = showExpandedBalancedText eTxt
        -- Write to:
        -- if stream number corresponds to existing, open file:
        --     file
        -- otherwise:
        --     log
        --     unless stream number is negative: terminal
        case getStream fStreams en of
            Just fStream ->
                 liftIO $ hPutStrLn fStream txtStr
            Nothing ->
                do
                -- Write to terminal.
                when (en >= 0) $ liftIO $ putStrLn txtStr
                -- Write to log
                logHandle <- asks logStream
                liftIO $ hPutStrLn logHandle txtStr
        retElems []
    HP.AddBox HP.NaturalPlacement (HP.ExplicitBox HP.Natural boxType) ->
        do
        pushGroup' ExplicitBoxGroup
        b <- case boxType of
            HP.ExplicitHBox ->
                (B.HBoxContents . BL.setHList BL.NaturallyGood . reverse) <$> extractHList HP.DoNotIndent True
            HP.ExplicitVBox alignType ->
                ((\els -> B.VBoxContents els alignType) . BL.setVList BL.NaturallyGood . reverse) <$> extractVList True
        retElems $ [(BL.VListBaseElem . B.ElemBox . (\bc -> B.Box bc B.Natural)) b]

  where
    retElems els = pure $ Just els

    noElems = retElems []

    pushGroup' grp = HP.runConfState $ modify $ pushGroup grp

getStream :: HMap.HashMap FourBitInt Handle -> IntVal -> Maybe Handle
getStream strms n =
    do
    fourBitn <- newFourBitInt n
    HMap.lookup fourBitn strms

processHCommand
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.ExpandedStream
    -> [BL.BreakableHListElem]
    -> Bool  -- restricted?
    -> HP.HModeCommand
    -> ExceptT BuildError m ([BL.BreakableHListElem], Bool)
processHCommand oldStream acc inRestricted = \case
    HP.LeaveHMode ->
        -- Inner mode: forbidden. TODO.
        if inRestricted
            then throwConfigError "Should not see vertical command in restricted horizontal mode"
            -- Outer mode: insert the rol sequence "\par" into the input. The control
            -- sequence's current meaning will be used, which might no longer be the \par
            -- primitive.
            -- (Note that we use oldStream.)
            else
                do
                let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
                put $ HP.insertLexToken oldStream parToken
                continueUnchanged
    HP.AddCharacter c ->
        liftConfigError $ do
        charCode <- readOnConfState $ evaluateCharCodeRef c
        hCharBox <- (BL.HListHBaseElem . B.ElemCharacter) <$> (readOnConfState $ characterBox charCode)
        modAccum $ hCharBox : acc
    HP.HAllModesCommand aCom -> case aCom of
        -- \indent: An empty box of width \parindent is appended to the current
        -- list, and the space factor is set to 1000.
        -- TODO: Space factor.
        HP.StartParagraph HP.DoNotIndent ->
            continueUnchanged
        HP.StartParagraph HP.Indent ->
            liftConfigError $ do
            indentBox <- readOnConfState $ asks parIndentBox
            modAccum (indentBox : acc)
        -- \par: Restricted: does nothing. Unrestricted: ends mode.
        HP.EndParagraph ->
            if inRestricted
                then continueUnchanged
                else pure (acc, False)
        HP.AddSpace ->
            liftConfigError $ do
            hGlue <- (BL.HVListElem . BL.ListGlue) <$> readOnConfState spaceGlue
            modAccum $ hGlue : acc
        HP.AddRule HP.Rule{..} ->
            liftReadOnConfState $ do
            evalW <- case width of
                Nothing -> pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> evaluateLength ln
            evalH <- case height of
                Nothing -> pure $ Unit.toScaledPointApprox (10 :: Int) Unit.Point
                Just ln -> evaluateLength ln
            evalD <- case depth of
                Nothing -> pure 0
                Just ln -> evaluateLength ln
            let rule = B.Rule{width = evalW, height = evalH, depth = evalD}
            modAccum $ (BL.HVListElem $ BL.VListBaseElem $ B.ElemRule rule) : acc
        HP.ModeIndependentCommand mcom ->
            handleModeIndep mcom >>= \case
                Nothing ->
                    pure (acc, False)
                Just extraAcc ->
                    modAccum $ (BL.HVListElem <$> extraAcc) ++ acc
  where
    modAccum newAcc = pure (newAcc, True)

    continueUnchanged = pure (acc, True)

data BuildError
  = ParseError (HP.ParseErrorBundle HP.ExpandedStream)
  | ConfigError String
  deriving (Show)

liftConfigError :: Monad m => ExceptT String m a -> ExceptT BuildError m a
liftConfigError f = withExceptT ConfigError f

liftConfState
    :: MonadState HP.ExpandedStream m
    => StateT Config (ExceptT String m) a
    -> ExceptT BuildError m a
liftConfState x = liftConfigError $ HP.runConfState x

liftReadOnConfState
    :: MonadState HP.ExpandedStream m
    => ReaderT Config (StateT Config (ExceptT String m)) a
    -> ExceptT BuildError m a
liftReadOnConfState x = liftConfigError $ readOnConfState x

throwConfigError :: MonadError BuildError m => String -> m a
throwConfigError s = throwError $ ConfigError s

liftMaybeConfigError
    :: MonadError BuildError m
    => String -> Maybe a -> m a
liftMaybeConfigError s = liftMaybe (ConfigError s)

extractHList
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.IndentFlag
    -> Bool
    -> ExceptT BuildError m [BL.BreakableHListElem]
extractHList indentFlag inRestricted =
    do
    indentBox <- readOnConfState $ asks parIndentBox
    extractParagraphInner [indentBox | indentFlag == HP.Indent]
  where
    -- We build a paragraph list in reverse order.
    extractParagraphInner acc =
        do
        oldStream <- get
        (PS.State{stateInput = newStream}, com) <- liftEither $ ParseError `mapLeft` HP.extractHModeCommand oldStream
        put newStream
        liftIO $ putStrLn $ (if inRestricted then "Restricted" else "Unrestricted") ++ " horizontal mode, processing command: " ++ show com
        (procAcc, continue) <- processHCommand oldStream acc inRestricted com
        if continue
            then extractParagraphInner procAcc
            else pure procAcc

extractBreakAndSetHList
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.IndentFlag
    -> ExceptT BuildError m [[B.HBoxElem]]
extractBreakAndSetHList indentFlag =
    do
    hList <- extractHList indentFlag False
    readOnConfState $ do
        desiredW <- asks $ LenParamVal . lookupLengthParameter HP.HSize
        lineTol <- asks $ IntParamVal . lookupIntegerParameter HP.Tolerance
        linePen <- asks $ IntParamVal . lookupIntegerParameter HP.LinePenalty
        pure $ breakAndSetParagraph desiredW lineTol linePen hList

data CurrentPage = CurrentPage
    { items :: [BL.BreakableVListElem]
    , bestPointAndCost :: Maybe (Int, Int)
    }

runPageBuilder
    :: MonadState Config m
    => CurrentPage
    -> [BL.BreakableVListElem]
    -> m [B.Page]
runPageBuilder (CurrentPage cur _) [] =
    do
    desiredH <- gets $ LenParamVal . lookupLengthParameter HP.VSize
    pure [setPage desiredH $ reverse cur]
runPageBuilder (CurrentPage cur _bestPointAndCost) (x:xs)
    -- If the current vlist has no boxes, we discard a discardable item.
    | not $ any BL.isBox cur =
        let nextXs = if BL.isDiscardable x then cur else x:cur
        in runPageBuilder (CurrentPage nextXs _bestPointAndCost) xs
    -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
    -- the cost c of breaking at this point.
    | BL.isDiscardable x =
        do
        desiredH <- gets $ LenParamVal . lookupLengthParameter HP.VSize
        case BL.toBreakItem Adj{pre = headMay cur, val = x, post = headMay xs} of
            -- If we can't break here, just add it to the list and continue.
            Nothing ->
                usualContinue
            Just brk ->
                case (pageBreakJudgment cur brk desiredH, _bestPointAndCost) of
                    (DoNotBreak, _) ->
                        usualContinue
                    -- I don't think this condition will ever be satisfied, but if we
                    -- decide to break before any valid break-point has been considered,
                    -- just carry on.
                    (BreakPageAtBest, Nothing) ->
                        usualContinue
                    -- If c = ∞, we break at the best breakpoint so far.
                    -- The current vlist material following that best breakpoint is
                    -- returned to the recent contributions, to consider again.
                    (BreakPageAtBest, Just (iBest, _)) ->
                        -- the `reverse` will put both of these into reading order.
                        let (curNewPage, toReturn) = splitAt iBest $ reverse cur
                            newPage = setPage desiredH curNewPage
                        -- xs is also in reading order
                        -- We didn't actually split at x: x was just what made us compute
                        -- cost and notice we'd gone too far. So add it to the left-overs
                        -- to return.
                        in  (newPage :) <$> runPageBuilder newCurrentPage (toReturn ++ (x:xs))
                    -- If p ≤ −10000, we know the best breakpoint is this one, so break
                    -- here.
                    (BreakPageHere, _) ->
                        -- the `reverse` will put this into reading order.
                        let newPage = setPage desiredH $ reverse cur
                        in  (newPage :) <$> runPageBuilder newCurrentPage xs
                    -- If the resulting cost <= the smallest cost seen so far, remember
                    -- the current breakpoint as the best so far.
                    (TrackCost cHere, _) ->
                        let thisPointAndCost = Just (length cur, cHere)
                            newBestPointAndCost = case _bestPointAndCost of
                                Nothing ->
                                    thisPointAndCost
                                Just (_, cBest) ->
                                    if cHere > cBest then _bestPointAndCost else thisPointAndCost
                        in  runPageBuilder (CurrentPage (x:cur) newBestPointAndCost) xs
    -- If we can't break here, just add it to the list and continue.
    | otherwise = usualContinue
  where
    usualNextPage = CurrentPage (x:cur) _bestPointAndCost
    usualContinue = runPageBuilder usualNextPage xs

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
addVListElem
    :: MonadState Config m
    => [BL.BreakableVListElem]
    -> BL.BreakableVListElem
    -> m [BL.BreakableVListElem]
addVListElem acc e = case e of
    (BL.VListBaseElem (B.ElemBox b)) -> addVListBox b
    _                                -> pure $ e : acc
  where
    addVListBox :: MonadState Config m => B.Box -> m [BL.BreakableVListElem]
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

processVCommand
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.ExpandedStream
    -> [BL.BreakableVListElem]
    -> Bool -- internal?
    -> HP.VModeCommand
    -> ExceptT BuildError m ([BL.BreakableVListElem], Bool)
processVCommand oldStream acc inInternal = \case
    -- End recursion.
    HP.End ->
        if inInternal
            then throwConfigError "End not allowed in internal vertical mode"
        else
            do
            readOnConfState $ asks scopedConfig >>= \case
                (_, []) -> pure ()
                _ -> throwConfigError "Cannot end: not in global scope"
            gets HP.skipState >>= \case
                [] -> pure ()
                _skipState -> throwConfigError $ "Cannot end: in condition block: " ++ show _skipState
            readOnConfState (asks finaliseConfig) >>= liftIO
            pure (acc, False)
    HP.EnterHMode ->
        addParagraphToPage HP.Indent
    HP.VAllModesCommand aCom -> case aCom of
        HP.StartParagraph indentFlag ->
            addParagraphToPage indentFlag
        -- \par does nothing in vertical mode.
        HP.EndParagraph ->
            continueUnchanged
        -- <space token> has no effect in vertical modes.
        HP.AddSpace ->
            continueUnchanged
        HP.AddRule HP.Rule{..} ->
            do
            evalW <- case width of
                Nothing -> readOnConfState $ gets $ lookupLengthParameter HP.HSize
                Just ln -> liftReadOnConfState $ evaluateLength ln
            evalH <- case height of
                -- TODO.
                Nothing -> readOnConfState $ pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> liftReadOnConfState $ evaluateLength ln
            evalD <- case depth of
                Nothing -> readOnConfState $ pure 0
                Just ln -> liftReadOnConfState $ evaluateLength ln
            let rule = B.Rule{width = evalW, height = evalH, depth = evalD}
            modAccum $ (BL.VListBaseElem $ B.ElemRule rule) : acc
        HP.ModeIndependentCommand mcom ->
            handleModeIndep mcom >>= \case
                Nothing ->
                    pure (acc, False)
                Just extraAcc ->
                    modAccum $ extraAcc ++ acc
        oth -> error $ show oth
  where
    modAccum newAcc = pure (newAcc, True)

    continueUnchanged = pure (acc, True)

    addParagraphToPage indentFlag =
        do
        -- If the command shifts to horizontal mode, run '\indent', and re-read the
        -- stream as if the commands just seen hadn't been read.
        -- (Note that we set "oldStream", not "newStream".)
        -- Paraboxes are returned in reading order.
        put oldStream
        lineBoxes <- extractBreakAndSetHList indentFlag
        desiredW <- readOnConfState $ asks $ lookupLengthParameter HP.HSize
        let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
        newAcc <- HP.runConfState $ foldM addVListElem acc $ BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes
        -- Continue iff not in internal mode.
        pure (newAcc, not inInternal)

newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage [] Nothing

extractVList
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => Bool
    -> ExceptT BuildError m [BL.BreakableVListElem]
extractVList inInternal =
    extractVListInner []
  where
    extractVListInner acc =
        do
        oldStream <- get
        (PS.State {stateInput = newStream}, com) <- liftEither $ ParseError `mapLeft` HP.extractVModeCommand oldStream
        put newStream
        liftIO $ putStrLn $ (if inInternal then "Internal" else "Outer") ++ " vertical mode, processing command: " ++ show com
        (procAcc, continue) <- processVCommand oldStream acc inInternal com
        if continue
            then extractVListInner procAcc
            else pure procAcc

extractBreakAndSetVList
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => ExceptT BuildError m [B.Page]
extractBreakAndSetVList = do
    vList <- extractVList False
    liftIO $ print $ length vList
    HP.runConfState $ runPageBuilder newCurrentPage $ reverse vList
