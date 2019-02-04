{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HeX.Build where

import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( MonadState
                                                , get
                                                , gets
                                                , put
                                                , liftIO
                                                , modify
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , liftEither
                                                , MonadError
                                                , withExceptT
                                                )
import           Data.Either.Combinators        ( mapLeft )
import qualified Data.HashMap.Strict           as HMap
import           Path
import           Safe                           ( headMay
                                                , toEnumMay )
import qualified Text.Megaparsec               as PS

import           Data.Adjacent                  ( Adj(..) )
import           Data.Path                      ( findFilePath )

import qualified TFM
import           TFM                            ( TexFont(..) )
import qualified TFM.Character                 as TFMC

import           HeXPrelude
import           HeX.Concept
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
    fontInfo_ <- readRelPath relPath
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
        searchDirectories
        & gets
        <&> findFilePath p
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

characterBox :: (MonadState Config m, MonadError String m) => CharCode -> m B.Character
characterBox char =
    do
    fontMetrics <- currentFontMetrics
    let toSP = TFM.designScaleSP fontMetrics
    TFMC.Character {width = w, height = h, depth = d} <-
        liftMaybe "No such character" $ (HMap.lookup char $ characters fontMetrics)
    pure
        B.Character {char = char, width = toSP w, height = toSP h, depth = toSP d}

spaceGlue :: (MonadState Config m, MonadError String m) => m BL.Glue
spaceGlue =
    do
    fontMetrics@TexFont{spacing = d, spaceStretch = str, spaceShrink = shr} <- currentFontMetrics
    let toSP = TFM.designScaleSP fontMetrics
        toFlex = toSP >>> fromIntegral >>> BL.finiteFlex
    pure BL.Glue {dimen = toSP d, stretch = toFlex str, shrink = toFlex shr}

assignVariable
    :: (MonadState Config m, MonadError String m)
    => HP.VariableAssignment
    -> m ()
assignVariable = \case
    HP.IntegerVariableAssignment v n ->
        do
        en <- evaluateNumber n
        case v of
            (HP.ParamVar p)    -> setConfIntParam p en
            (HP.RegisterVar _) -> error "int registers not implemented"
    HP.LengthVariableAssignment v d ->
        do
        ed <- evaluateLength d
        case v of
            (HP.ParamVar p)    -> setConfLenParam p ed
            (HP.RegisterVar _) -> error "length registers not implemented"
    HP.GlueVariableAssignment v g ->
        do
        eg <- evaluateGlue g
        case v of
            (HP.ParamVar p)    -> setConfGlueParam p eg
            (HP.RegisterVar _) -> error "glue registers not implemented"
    HP.MathGlueVariableAssignment _ _ ->
        error "math-glue assignment not implemented"
    HP.TokenListVariableAssignmentVar _ _ ->
        error "token-list-to-variable assignment not implemented"
    HP.TokenListVariableAssignmentText _ _ ->
        error "token-list-to-text assignment not implemented"

showMsg :: Lex.Token -> [CharCode]
showMsg (Lex.CharCatToken (Lex.CharCat {char = c, cat = Lex.Letter})) = [c]
showMsg (Lex.CharCatToken (Lex.CharCat {char = c, cat = Lex.Other})) = [c]
showMsg (Lex.CharCatToken (Lex.CharCat {char = c, cat = Lex.Space})) = [c]
showMsg (Lex.CharCatToken cc) = show cc
showMsg (Lex.ControlSequenceToken (Lex.ControlSequence cs)) = "\\" ++ cs

handleModeIndep
    :: (MonadState HP.ExpandedStream m, MonadIO m)
    => HP.ModeIndependentCommand
    -> ExceptT String m [BL.BreakableVListElem]
handleModeIndep = \case
    HP.Message HP.Out (HP.BalancedText txt) ->
        do
        liftIO $ putStrLn $ concatMap showMsg txt
        pure []
    HP.Relax ->
        pure []
    HP.IgnoreSpaces ->
        pure []
    HP.AddPenalty n ->
        do
        p <- HP.runConfState $ evaluatePenalty n
        pure [BL.ListPenalty $ p]
    HP.AddKern ln ->
        do
        k <- HP.runConfState $ evaluateKern ln
        pure [BL.VListBaseElem $ B.ElemKern $ k]
    HP.AddGlue g ->
        do
        eG <- HP.runConfState $ evaluateGlue g
        pure [BL.ListGlue eG]
    HP.Assign HP.Assignment{global = _, body = _body} ->
        case _body of
            HP.DefineControlSequence cs tgt ->
                do
                (acc, newCSTok) <- case tgt of
                    HP.MacroTarget macro ->
                        pure ([], HP.SyntaxCommandHeadToken $ HP.MacroTok macro)
                    HP.ShortDefineTarget q n ->
                        do
                        en <- HP.runConfState $ evaluateNumber n
                        pure ([], HP.PrimitiveToken $ HP.IntRefTok q en)
                    HP.FontTarget HP.NaturalFont fPath ->
                        do
                        fontDef@B.FontDefinition{fontNr=fNr} <- HP.runConfState $ loadFont fPath
                        let fontRefTok = HP.PrimitiveToken $ HP.FontRefToken fNr
                            boxElem = BL.VListBaseElem $ B.ElemFontDefinition fontDef
                        pure ([boxElem], fontRefTok)
                modify (\strm -> HP.insertControlSequence strm cs newCSTok)
                pure acc
            HP.SetVariable ass ->
                do
                HP.runConfState $ assignVariable ass
                pure []
            HP.AssignCode (HP.CodeAssignment (HP.CodeTableRef codeType idx) val) ->
                do
                eIdx <- HP.runConfState $ evaluateNumber idx
                eVal <- HP.runConfState $ evaluateNumber val
                idxChar <- liftMaybe ("Invalid character code index: " ++ show eIdx) (toEnumMay eIdx)
                liftIO $ putStrLn $ "Setting " ++ show codeType ++ "@" ++ show eIdx ++ " (" ++ show idxChar ++ ") to " ++ show eVal

                updateFunc <- case codeType of
                    HP.CategoryCode ->
                        do
                        valCat <- liftMaybe ("Invalid category code value: " ++ show eVal) (toEnumMay eVal)
                        liftIO $ putStrLn $ "parsed val to " ++ show valCat
                        pure (\c -> c{catCodeMap=HMap.insert idxChar valCat $ catCodeMap c})
                    HP.MathCode ->
                        do
                        valMathCode <- liftMaybe ("Invalid math code value: " ++ show eVal) $ intToMathCode eVal
                        liftIO $ putStrLn $ "parsed val to " ++ show valMathCode
                        pure (\c -> c{mathCodeMap=HMap.insert idxChar valMathCode $ mathCodeMap c})
                    HP.SpaceFactorCode ->
                        do
                        pure (\c -> c{spaceFactorMap=HMap.insert idxChar eVal $ spaceFactorMap c})
                    _ ->
                        error $ "Code type not implemented: " ++ show codeType
                HP.runConfState $ modify updateFunc
                pure []
            HP.SelectFont fNr ->
                do
                fontSel <- HP.runConfState $ selectFont fNr
                pure [BL.VListBaseElem $ B.ElemFontSelection fontSel]

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
        charCode <- HP.runConfState $ evaluateCharCodeRef c
        hCharBox <- charCode & characterBox & HP.runConfState <&> (B.ElemCharacter >>> BL.HListHBaseElem)
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
            hGlue <- (BL.HVListElem . BL.ListGlue) <$> HP.runConfState spaceGlue
            modAccum $ hGlue : acc
        HP.AddRule HP.Rule{..} ->
            do
            evalW <- case width of
                Nothing -> pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> HP.runConfState $ evaluateLength ln
            evalH <- case height of
                Nothing -> pure $ Unit.toScaledPointApprox (10 :: Int) Unit.Point
                Just ln -> HP.runConfState $ evaluateLength ln
            evalD <- case depth of
                Nothing -> pure 0
                Just ln -> HP.runConfState $ evaluateLength ln
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
                Nothing -> HP.runConfState $ gets (unLenParam . hSize . params)
                Just ln -> liftConfigError $ HP.runConfState $ evaluateLength ln
            evalH <- case height of
                -- TODO.
                Nothing -> HP.runConfState $ pure $ Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> liftConfigError $ HP.runConfState $ evaluateLength ln
            evalD <- case depth of
                Nothing -> HP.runConfState $ pure 0
                Just ln -> liftConfigError $ HP.runConfState $ evaluateLength ln
            let rule = B.Rule{width = evalW, height = evalH, depth = evalD}
            modAccum $ (BL.VListBaseElem $ B.ElemRule rule) : acc
        HP.ModeIndependentCommand mcom ->
            do
            extraAcc <- liftConfigError $ handleModeIndep mcom
            modAccum (extraAcc ++ acc)
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
