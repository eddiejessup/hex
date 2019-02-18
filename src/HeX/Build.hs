{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HeX.Build where

import           Control.Monad                  ( foldM )
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
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , liftEither
                                                , throwError
                                                , withExceptT
                                                )
import           Data.Either.Combinators        ( mapLeft )
import qualified Data.HashMap.Strict           as HMap
import           Path
import           Safe                           ( headMay
                                                , toEnumMay )
import           System.IO                      ( Handle
                                                , hPutStrLn
                                                , hFlush
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
import           HeX.BreakList.Line             ( setParagraph )
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
    -> m B.FontDefinition
loadFont relPath =
    do
    fontInfo_ <- readOnState $ readRelPath relPath
    fontName <- extractFontName relPath
    fNr <- addFont fontInfo_
    pure B.FontDefinition
        { fontNr           = fNr
        -- TODO: Improve mapping of name and path.
        , fontPath         = fontName
        , fontName         = fontName
        , fontInfo         = fontMetrics fontInfo_
        , scaleFactorRatio = 1.0
        }
  where
    readRelPath p =
        findFilePath p <$> asks searchDirectories
        >>= liftThrow "Could not find font"
        >>= readFontInfo

    stripExtension p =
        liftThrow "Could not strip font extension" $ Path.setFileExtension "" p

    fileName = Path.filename >>> Path.toFilePath

    extractFontName p = stripExtension p <&> fileName

selectFont :: MonadState Config m => Int -> m B.FontSelection
selectFont n =
    do
    modify (\conf -> conf{currentFontNr = Just n})
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
    => HP.IntegerVariable -> IntVal -> m ()
setIntegerVariable v tgt = case v of
    HP.ParamVar p       -> setConfIntParam p tgt
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> setIntegerRegister i tgt)

setLengthVariable
    :: (MonadState Config m, MonadError String m)
    => HP.LengthVariable -> LenVal -> m ()
setLengthVariable v tgt = case v of
    HP.ParamVar p       -> setConfLenParam p tgt
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> setLengthRegister i tgt)

setGlueVariable
    :: (MonadState Config m, MonadError String m)
    => HP.GlueVariable -> BL.Glue -> m ()
setGlueVariable v tgt = case v of
    HP.ParamVar p       -> setConfGlueParam p tgt
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> setGlueRegister i tgt)

setTokenListVariable
    :: (MonadState Config m, MonadError String m)
    => HP.TokenListVariable -> HP.BalancedText -> m ()
setTokenListVariable v tgt = case v of
    HP.ParamVar p       -> setConfTokenListParam p tgt
    HP.RegisterVar iRaw -> readOnState (evaluateEightBitInt iRaw) >>= (\i -> setTokenListRegister i tgt)

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
    => HP.ModeIndependentCommand -> ExceptT String m [BL.BreakableVListElem]
handleModeIndep = \case
    HP.ChangeScope (HP.Sign isPos) trig ->
        do
        HP.runConfState $ if isPos
            then
                modify $ pushScope trig
            else
                gets (popScope trig) >>= (\case
                    Left err -> throwError err
                    Right conf' -> put conf')
        pure []
    HP.Message HP.Out eTxt ->
        do
        liftIO $ putStrLn $ showExpandedBalancedText eTxt
        pure []
    HP.Relax ->
        pure []
    HP.IgnoreSpaces ->
        pure []
    HP.AddPenalty n ->
        do
        p <- readOnConfState $ evaluatePenalty n
        pure [BL.ListPenalty p]
    HP.AddKern ln ->
        do
        k <- readOnConfState $ evaluateKern ln
        pure [BL.VListBaseElem $ B.ElemKern k]
    HP.AddGlue g ->
        do
        eG <- readOnConfState $ evaluateGlue g
        pure [BL.ListGlue eG]
    HP.Assign HP.Assignment{global = globalFlag, body = _body} ->
        case _body of
            HP.DefineControlSequence cs tgt ->
                do
                (acc, newCSTok) <- readOnConfState $ case tgt of
                    HP.MacroTarget macro ->
                        pure ([], HP.syntaxTok $ HP.MacroTok macro)
                    -- TODO: If a \let target is an active character, should we
                    -- treat it as a control sequence, or a char-cat pair?
                    HP.LetTarget (Lex.CharCatToken tgtCC) ->
                        pure ([], HP.primTok $ HP.LetCharCat tgtCC)
                    HP.LetTarget (Lex.ControlSequenceToken tgtCS) ->
                        do
                        resTok <-
                            asks (lookupCSProper tgtCS)
                            >>= liftMaybe ("Unknown control sequence: " ++ show tgtCS)
                        pure ([], resTok)
                    HP.ShortDefineTarget q n ->
                        do
                        en <- evaluateNumber n
                        pure ([], HP.primTok $ HP.IntRefTok q en)
                    HP.FontTarget HP.NaturalFont fPath ->
                        do
                        fontDef@B.FontDefinition{fontNr=fNr} <- loadFont fPath
                        let fontRefTok = HP.primTok $ HP.FontRefToken fNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure ([boxElem], fontRefTok)
                modify (\strm -> HP.insertControlSequence strm cs newCSTok globalFlag)
                pure acc
            HP.SetVariable ass ->
                do
                HP.runConfState $ case ass of
                    HP.IntegerVariableAssignment v tgt ->
                        readOnState (evaluateNumber tgt) >>= setIntegerVariable v
                    HP.LengthVariableAssignment v tgt  ->
                        readOnState (evaluateLength tgt) >>= setLengthVariable v
                    HP.GlueVariableAssignment v tgt    ->
                        readOnState (evaluateGlue tgt) >>= setGlueVariable v
                    HP.MathGlueVariableAssignment _ _  ->
                        error "math-glue assignment not implemented"
                    HP.TokenListVariableAssignment v tgt ->
                        do
                        eTgt <- readOnState $ case tgt of
                            HP.TokenListAssignmentVar tgtVar   -> evaluateTokenListVariable tgtVar
                            HP.TokenListAssignmentText tgtText -> pure tgtText
                        setTokenListVariable v eTgt
                pure []
            HP.ModifyVariable modCommand ->
                do
                HP.runConfState $ case modCommand of
                    HP.AdvanceIntegerVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ (+) <$> evaluateIntegerVariable var <*> evaluateNumber plusVal
                        setIntegerVariable var newVarVal
                    HP.AdvanceLengthVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ (+) <$> evaluateLengthVariable var <*> evaluateLength plusVal
                        setLengthVariable var newVarVal
                    HP.AdvanceGlueVariable var plusVal ->
                        do
                        newVarVal <- readOnState $ mappend <$> evaluateGlueVariable var <*> evaluateGlue plusVal
                        setGlueVariable var newVarVal
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
                                setIntegerVariable var $ op eVar eScaleVal
                            HP.LengthNumericVariable var ->
                                do
                                eVar <- readOnState $ evaluateLengthVariable var
                                let op = case vDir of
                                        Upward -> (*)
                                        Downward -> quot
                                setLengthVariable var $ op eVar eScaleVal
                            HP.GlueNumericVariable var ->
                                do
                                eVar <- readOnState $ evaluateGlueVariable var
                                let op = case vDir of
                                        Upward -> BL.multiplyGlue
                                        Downward -> BL.divGlue
                                setGlueVariable var $ op eVar eScaleVal
                pure []
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- readOnConfState $ evaluateNumber idx
                eVal <- readOnConfState $ evaluateNumber val
                liftIO $ putStrLn $ "Evaluated code table index " ++ show idx ++ " to " ++ show eIdx
                liftIO $ putStrLn $ "Evaluated code table value " ++ show val ++ " to " ++ show eVal
                idxChar <- liftMaybe ("Invalid character code index: " ++ show eIdx) (toEnumMay eIdx)
                liftIO $ putStrLn $ "Setting " ++ show codeType ++ "@" ++ show eIdx ++ " (" ++ show idxChar ++ ") to " ++ show eVal
                HP.runConfState $ updateCharCodeMap codeType idxChar eVal
                pure []
            HP.SelectFont fNr ->
                do
                fontSel <- HP.runConfState $ selectFont fNr
                pure [BL.VListBaseElem $ B.ElemFontSelection fontSel]
    HP.WriteToStream n (HP.ImmediateWriteText eTxt) ->
        readOnConfState $ do
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
                if en >= 0
                    -- Write to terminal.
                    then liftIO $ putStrLn txtStr
                    else pure ()
                -- Write to log
                logHandle <- asks logStream
                liftIO $ hPutStrLn logHandle txtStr
        pure []

getStream :: HMap.HashMap FourBitInt Handle -> IntVal -> Maybe Handle
getStream strms n =
    do
    fourBitn <- newFourBitInt n
    HMap.lookup fourBitn strms

processHCommand
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.ExpandedStream
    -> [BL.BreakableHListElem]
    -> HP.HModeCommand
    -> ExceptT String m ([BL.BreakableHListElem], Bool)
processHCommand oldStream acc = \case
    HP.LeaveHMode ->
        do
        -- Inner mode: forbidden. TODO.
        -- Outer mode: insert the rol sequence "\par" into the input. The control
        -- sequence's current meaning will be used, which might no longer be the \par
        -- primitive.
        -- (Note that we use oldStream.)
        let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
        put $ HP.insertLexToken oldStream parToken
        continueUnchanged
    HP.AddCharacter c ->
        do
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
            do
            indentBox <- HP.runConfState $ gets parIndentBox
            modAccum (indentBox : acc)
        -- \par: end the current paragraph.
        HP.EndParagraph ->
            pure (acc, False)
        HP.AddSpace ->
            do
            hGlue <- (BL.HVListElem . BL.ListGlue) <$> readOnConfState spaceGlue
            modAccum $ hGlue : acc
        HP.AddRule HP.Rule{..} ->
            readOnConfState $ do
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
            do
            extraAcc <- handleModeIndep mcom
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

extractParagraph
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.IndentFlag
    -> ExceptT BuildError m [BL.BreakableHListElem]
extractParagraph indentFlag =
    do
    indentBox <- HP.runConfState $ gets parIndentBox
    extractParagraphInner [indentBox | indentFlag == HP.Indent]
  where
    -- We build a paragraph list in reverse order.
    extractParagraphInner acc =
        do
        oldStream <- get
        (PS.State{stateInput = newStream}, com) <- liftEither $ ParseError `mapLeft` HP.extractHModeCommand oldStream
        put newStream
        (procAcc, continue) <- liftConfigError $ processHCommand oldStream acc com
        if continue
            then extractParagraphInner procAcc
            else pure procAcc

extractParagraphLineBoxes
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.IndentFlag
    -> ExceptT BuildError m [[B.HBoxElem]]
extractParagraphLineBoxes indentFlag =
    do
    hList <- extractParagraph indentFlag
    desiredW <- HP.runConfState $ gets (hSize . params)
    lineTol <- HP.runConfState $ gets (tolerance . params)
    linePen <- HP.runConfState $ gets (linePenalty . params)
    pure $ setParagraph desiredW lineTol linePen hList

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
    desiredH <- gets (vSize . params)
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
        desiredH <- gets (vSize . params)
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
        _prevDepth <- gets (unLenParam . prevDepth . params)
        BL.Glue blineLength blineStretch blineShrink <- gets (unGlueParam . baselineSkip . params)
        skipLimit <- gets (unLenParam . lineSkipLimit . params)
        skip <- gets (unGlueParam . lineSkip . params)
        setConfSpecialLen HP.PrevDepth $ naturalDepth e
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
    -> [B.Page]
    -> CurrentPage
    -> [BL.BreakableVListElem]
    -> HP.VModeCommand
    -> ExceptT BuildError m ([B.Page], CurrentPage, [BL.BreakableVListElem], Bool)
processVCommand oldStream pages curPage acc = \case
    -- End recursion.
    HP.End ->
        do
        lastPages <- HP.runConfState $ runPageBuilder curPage (reverse acc)
        let pagesFinal = pages ++ lastPages
        readOnConfState $ asks logStream >>= liftIO . hFlush
        pure (pagesFinal, curPage, acc, False)
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
                Nothing -> readOnConfState $ gets (unLenParam . hSize . params)
                Just ln -> liftConfigError $ readOnConfState $ evaluateLength ln
            evalH <- case height of
                -- TODO.
                Nothing -> readOnConfState $ pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> liftConfigError $ readOnConfState $ evaluateLength ln
            evalD <- case depth of
                Nothing -> readOnConfState $ pure 0
                Just ln -> liftConfigError $ readOnConfState $ evaluateLength ln
            let rule = B.Rule{width = evalW, height = evalH, depth = evalD}
            modAccum $ (BL.VListBaseElem $ B.ElemRule rule) : acc
        HP.ModeIndependentCommand mcom ->
            do
            extraAcc <- liftConfigError $ handleModeIndep mcom
            modAccum (extraAcc ++ acc)
        oth -> error $ show oth
  where
    modAccum newAcc = pure (pages, curPage, newAcc, True)

    continueUnchanged = pure (pages, curPage, acc, True)

    addParagraphToPage indentFlag =
        do
        -- If the command shifts to horizontal mode, run '\indent', and re-read the
        -- stream as if the commands just seen hadn't been read.
        -- (Note that we set "oldStream", not "newStream".)
        -- Paraboxes are returned in reading order.
        put oldStream
        lineBoxes <- extractParagraphLineBoxes indentFlag
        desiredW <- HP.runConfState $ gets (unLenParam . hSize . params)
        let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
        newAcc <- HP.runConfState $ foldM addVListElem acc $ BL.VListBaseElem . B.ElemBox . toBox <$> lineBoxes
        modAccum newAcc

newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage [] Nothing

extractPages
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => ExceptT BuildError m [B.Page]
extractPages =
    extractPagesInner [] newCurrentPage []
  where
    extractPagesInner pages curPage acc =
        do
        oldStream <- get
        (PS.State {stateInput = newStream}, com) <- liftEither $ ParseError `mapLeft` HP.extractVModeCommand oldStream
        put newStream
        (procPages, procCurPage, procAcc, continue) <- processVCommand oldStream pages curPage acc com
        if continue
            then extractPagesInner procPages procCurPage procAcc
            else pure procPages
