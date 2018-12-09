{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module HeX.Build where

import           Control.Monad                  ( foldM )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( MonadState
                                                , get
                                                , gets
                                                , lift
                                                , liftIO
                                                , modify
                                                )
import           Control.Monad.Trans.Maybe      ( MaybeT(..)
                                                , runMaybeT
                                                )
import           Control.Monad.Except           ( ExceptT, liftEither )
import           Control.Monad.Trans.Reader     ( Reader
                                                , asks
                                                , runReader
                                                )
import qualified Data.HashMap.Strict           as HMap
import           Path
import           Safe                           ( headMay )
import qualified Text.Megaparsec               as PS

import           Data.Adjacent                  ( Adjacency(..) )
import qualified TFM
import           TFM                            ( TexFont(..) )
import qualified TFM.Character                 as TFMC

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.BreakList.Line             ( bestRoute
                                                , setParagraph
                                                )

import           HeX.BreakList.Page             ( PageBreakJudgment(..)
                                                , pageBreakJudgment
                                                , setPage
                                                )
import           HeX.Config
import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex
import           HeX.Evaluate
import qualified HeX.Parse.Expanded            as E
import qualified HeX.Parse.Resolved            as R
import qualified HeX.Parse.Helpers             as PH
import qualified HeX.Unit                      as Unit

csToFontNr :: Lex.ControlSequenceLike -> Int
csToFontNr (Lex.ControlSequenceProper (Lex.ControlSequence "thefont")) =
  R.theFontNr

currentFontInfo :: Monad m => MaybeT (ConfReaderT m) TexFont
currentFontInfo
  -- Maybe font number isn't set.
 = do
  maybeFontNr <- lift $ asks currentFontNr
  -- Or maybe there's no font where there should be.
  -- TODO: I think I can make this case impossible, maybe by storing the
  -- current font info directly instead of a lookup.
  fInfo <-
    HMap.lookup <$> MaybeT (return maybeFontNr) <*> lift (asks fontInfoMap)
  MaybeT $ pure fInfo

defineFont ::
     (MonadState Config m, MonadIO m)
  => Lex.ControlSequenceLike
  -> Path Rel File
  -> m B.FontDefinition
defineFont cs fPath = do
  theFontDirectories <- gets fontDirectories
  maybeFontDef <- liftIO $ runMaybeT $ ioFontDef theFontDirectories
  case maybeFontDef of
    Just fontDef@B.FontDefinition {fontInfo = font} -> do
      modify
        (\conf -> conf {fontInfoMap = HMap.insert fNr font $ fontInfoMap conf})
      pure fontDef
    Nothing -> fail "Could not define font"
  where
    fNr = csToFontNr cs
    ioFontDef fontDirs = do
      fontPath <- findFilePath fPath fontDirs
      font <- liftIO $ TFM.readTFMFancy fontPath
      nonExtName <- Path.setFileExtension "" fPath
      let fontName = Path.toFilePath $ Path.filename nonExtName
      return
        B.FontDefinition
        { fontNr = fNr
        , fontPath = fontPath
        , fontName = fontName
        , fontInfo = font
        , scaleFactorRatio = 1.0
        }

selectFont :: Monad m => Int -> ConfStateT m B.FontSelection
selectFont n = do
  modify (\conf -> conf {currentFontNr = Just n})
  pure B.FontSelection {fontNr = n}

characterBox :: Monad m => CharCode -> MaybeT (ConfReaderT m) B.Character
characterBox char = do
  font <- currentFontInfo
  let toSP = TFM.designScaleSP font
  TFMC.Character {width = w, height = h, depth = d} <-
    MaybeT (return $ HMap.lookup char $ characters font)
  return
    B.Character {char = char, width = toSP w, height = toSP h, depth = toSP d}

spaceGlue :: Monad m => MaybeT (ConfReaderT m) BL.Glue
spaceGlue = do
  font@TexFont {spacing = d, spaceStretch = str, spaceShrink = shr} <-
    currentFontInfo
  let toSP = TFM.designScaleSP font
      toFlex = BL.finiteFlex . fromIntegral . toSP
  pure BL.Glue {dimen = toSP d, stretch = toFlex str, shrink = toFlex shr}

defineMacro :: E.ExpandedStream -> E.AssignmentBody -> E.ExpandedStream
defineMacro (E.ExpandedStream (R.ResolvedStream ls csMap)) (E.DefineMacro name macro False False) =
  E.ExpandedStream (R.ResolvedStream ls csMap')
  where
    newMacro = R.SyntaxCommandHead $ R.MacroToken macro
    csMap' = HMap.insert name newMacro csMap

runReaderOnState :: MonadState r f => Reader r b -> f b
runReaderOnState f = runReader f <$> get

type BuildMonad = ConfStateT (ExceptT (PH.ParseError E.ExpandedStream) IO)

processHCommand
  :: E.ExpandedStream
  -> E.ExpandedStream
  -> [BL.BreakableHListElem]
  -> E.HModeCommand
  -> BuildMonad ([BL.BreakableHListElem], E.ExpandedStream, Bool)
processHCommand oldStream newStream acc com =
  let
    modAccum newAcc = pure (newAcc, newStream, True)
    modStream mStream = pure (acc, mStream, True)
    continueUnchanged = pure (acc, newStream, True)
  in case com of
    E.LeaveHMode -> do
      -- Inner mode: forbidden. TODO.
      -- Outer mode: insert the rol sequence "\par" into the input. The control
      -- sequence's current meaning will be used, which might no longer be the \par
      -- primitive.
      -- (Note that we pass oldStream, not newStream.)
      let parToken = Lex.ControlSequenceToken $ Lex.ControlSequence "par"
      modStream $ E.insertLexTokenE oldStream parToken
    E.AddCharacter {char = c} -> do
      charBox <- runReaderOnState (runMaybeT (characterBox c))
      hCharBox <- case BL.HCharacter <$> charBox of
          Just char -> pure char
          Nothing -> fail "Could not get character info"
      modAccum $ hCharBox : acc
    E.HAllModesCommand aCom -> case aCom of
      -- Command to end recursion.
      -- \par: end the current paragraph.
      E.EndParagraph -> pure (acc, newStream, False)
      -- Commands to do nothing.
      E.Relax -> continueUnchanged
      E.IgnoreSpaces -> continueUnchanged
      -- Commands to modify the input stream.
      -- E.ChangeCase d bt ->
      --   modStream $ applyChangeCaseToStream newStream d bt
      -- Commands to modify the list.
      E.Assign E.Assignment {body = E.SelectFont fNr} -> do
        fontSel <- BL.HFontSelection <$> selectFont fNr
        modAccum $ fontSel : acc
      E.Assign E.Assignment {body = E.DefineFont cs fPath} -> do
        fontDef <- BL.HFontDefinition <$> defineFont cs fPath
        modAccum $ fontDef : acc
      E.AddPenalty n -> modAccum $ BL.HPenalty (evaluatePenalty n) : acc
      E.AddKern ln -> do
        mag <- gets magnification
        modAccum $ BL.HKern (evaluateKern mag ln) : acc
      E.AddGlue g -> do
        mag <- gets magnification
        modAccum $ BL.HGlue (evaluateGlue mag g) : acc
      E.AddSpace -> do
        glue <- runReaderOnState (runMaybeT spaceGlue)
        hGlue <-
          case glue of
            Just sg -> pure $ BL.HGlue sg
            Nothing -> fail "Could not get space glue"
        modAccum $ hGlue : acc
      E.AddRule {width = w, height = h, depth = d} -> do
        mag <- gets magnification
        let evalW =
              case w of
                Nothing ->
                  Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> evaluateLength mag ln
            evalH =
              case h of
                Nothing -> Unit.toScaledPointApprox (10 :: Int) Unit.Point
                Just ln -> evaluateLength mag ln
            evalD =
              case d of
                Nothing -> 0
                Just ln -> evaluateLength mag ln
            rule = B.Rule {width = evalW, height = evalH, depth = evalD}
        modAccum $ BL.HRule rule : acc
      -- \indent: An empty box of width \parindent is appended to the current
      -- list, and the space factor is set to 1000.
      -- TODO: Space factor.
      E.StartParagraph indent ->
        if indent
          then do
            indentBox <- gets parIndentBox
            modAccum (indentBox : acc)
          else continueUnchanged
      E.Assign E.Assignment {body = m@E.DefineMacro {}} ->
        modStream $ defineMacro newStream m

extractParagraph
  :: Bool
  -> E.ExpandedStream
  -> BuildMonad ([BL.BreakableHListElem], E.ExpandedStream)
extractParagraph indent stream = do
  indentBox <- gets parIndentBox
  extractParagraphInner [indentBox | indent] stream
  where
    -- We build a paragraph list in reverse order.
    extractParagraphInner
      :: [BL.BreakableHListElem]
      -> E.ExpandedStream
      -> BuildMonad ([BL.BreakableHListElem], E.ExpandedStream)
    extractParagraphInner acc oldStream = do
      (PS.State {stateInput = newStream}, com) <- liftEither $ E.extractHModeCommand oldStream
      (procAcc, procStream, continue) <- processHCommand oldStream newStream acc com
      if continue
        then extractParagraphInner procAcc procStream
        else pure (procAcc, procStream)

extractParagraphLineBoxes
  :: Bool
  -> E.ExpandedStream
  -> BuildMonad ([[B.HBoxElem]], E.ExpandedStream)
extractParagraphLineBoxes indent stream = do
  (hList, stream') <- extractParagraph indent stream
  desiredW <- gets desiredWidth
  lineTol <- gets lineTolerance
  linePen <- gets linePenalty
  let getRoute = bestRoute desiredW lineTol linePen
      elemLists = setParagraph getRoute hList
  pure (elemLists, stream')

data CurrentPage = CurrentPage { items :: [BL.BreakableVListElem]
                               , bestPointAndCost :: Maybe (Int, Int) }

runPageBuilder
  :: Monad m => CurrentPage -> [BL.BreakableVListElem] -> ConfStateT m [B.Page]
runPageBuilder (CurrentPage cur _) [] = do
  desiredH <- gets desiredHeight
  pure [setPage desiredH $ reverse cur]
runPageBuilder (CurrentPage cur _bestPointAndCost) (x:xs)
  -- If the current vlist has no boxes, we discard a discardable item.
  | not $ any BL.isBox cur =
    let nextXs = if BL.isDiscardable x then cur else x:cur
    in runPageBuilder (CurrentPage nextXs _bestPointAndCost) xs
  -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
  -- the cost c of breaking at this point.
  | BL.isDiscardable x = do
    desiredH <- gets desiredHeight
    case BL.toBreakItem (Adjacency (headMay cur, x, headMay xs)) of
      -- If we can't break here, just add it to the list and continue.
      Nothing -> usualContinue
      Just brk -> case (pageBreakJudgment cur brk desiredH, _bestPointAndCost) of
        (DoNotBreak, _) -> usualContinue
        -- I don't think this condition will ever be satisfied, but if we
        -- decide to break before any valid break-point has been considered,
        -- just carry on.
        (BreakPageAtBest, Nothing) -> usualContinue
        -- If c = ∞, we break at the best breakpoint so far.
        -- The current vlist material following that best breakpoint is
        -- returned to the recent contributions, to consider again.
        (BreakPageAtBest, Just (iBest, _)) ->
         -- the `reverse` will put both of these into reading order.
          let
            (curNewPage, toReturn) = splitAt iBest $ reverse cur
            newPage = setPage desiredH curNewPage
         -- xs is also in reading order
         -- We didn't actually split at x: x was just what made us compute
         -- cost and notice we'd gone too far. So add it to the left-overs
         -- to return.
          in
            (newPage :) <$> runPageBuilder newCurrentPage (toReturn ++ (x:xs))
        -- If p ≤ −10000, we know the best breakpoint is this one, so break
        -- here.
        (BreakPageHere, _) ->
         -- the `reverse` will put this into reading order.
          let newPage = setPage desiredH $ reverse cur
          in (newPage :) <$> runPageBuilder newCurrentPage xs
        -- If the resulting cost <= the smallest cost seen so far, remember
        -- the current breakpoint as the best so far.
        (TrackCost cHere, _) ->
          let
            thisPointAndCost = Just (length cur, cHere)
            newBestPointAndCost = case _bestPointAndCost of
              Nothing -> thisPointAndCost
              Just (_, cBest) ->
                if cHere > cBest then _bestPointAndCost else thisPointAndCost
          in
            runPageBuilder (CurrentPage (x:cur) newBestPointAndCost) xs
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
  :: Monad m
  => [BL.BreakableVListElem]
  -> BL.BreakableVListElem
  -> ConfStateT m [BL.BreakableVListElem]
addVListElem acc e = case e of
    (BL.VListBox b) -> addVListBox b
    _ -> pure $ e : acc
  where
    addVListBox b = do
      prevDepth <- gets $ unPreviousBoxDepth . previousBoxDepth
      -- TODO:
      BL.Glue baselineLength blineStretch blineShrink <- gets $ unBaselineGlue . baselineGlue
      blineLengthMin <- gets $ unBaselineLengthMin . baselineLengthMin
      minBlineGlue <- gets $ unMinBaselineGlue . minBaselineGlue
      modify (\conf -> conf { previousBoxDepth = PreviousBoxDepth $ B.naturalDepth e })
      pure $
        if prevDepth <= -Unit.oneKPt
          then e : acc
          else let proposedBaselineLength =
                     baselineLength - prevDepth - B.naturalHeight b
            -- Intuition: set the distance between baselines to \baselineskip, but no
            -- closer than \lineskiplimit [theBaselineLengthMin], in which case
            -- \lineskip [theMinBaselineGlue] is used.
                   glue =
                     BL.VGlue $
                     if proposedBaselineLength >= blineLengthMin
                       then BL.Glue proposedBaselineLength blineStretch blineShrink
                       else minBlineGlue
               in e : glue : acc

addVListElems
  :: Monad m
  => [BL.BreakableVListElem]
  -> [BL.BreakableVListElem]
  -> ConfStateT m [BL.BreakableVListElem]
addVListElems = foldM addVListElem

processVCommand
  :: E.ExpandedStream
  -> E.ExpandedStream
  -> [B.Page]
  -> CurrentPage
  -> [BL.BreakableVListElem]
  -> E.VModeCommand
  -> BuildMonad ([B.Page], CurrentPage, [BL.BreakableVListElem], E.ExpandedStream, Bool)
processVCommand oldStream newStream pages curPage acc com =
  let
    continueSamePage newAcc mStream  = pure (pages, curPage, newAcc, mStream, True)
    modAccum newAcc = continueSamePage newAcc newStream
    modStream mStream = continueSamePage acc mStream
    continueUnchanged = continueSamePage acc newStream

    addParagraphToPage indent = do
      -- If the command shifts to horizontal mode, run '\indent', and re-read the
      -- stream as if the commands just seen hadn't been read.
      -- (Note that we pass "oldStream", not "newStream".)
      -- Paraboxes are returned in reading order.
      (lineBoxes, mStream) <- extractParagraphLineBoxes indent oldStream
      desiredW <- gets $ unDesiredWidth . desiredWidth
      let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
      newAcc <- addVListElems acc $ BL.VListBox . toBox <$> lineBoxes
      continueSamePage newAcc mStream
  in case com of
    -- End recursion.
    E.End -> do
      lastPages <- runPageBuilder curPage (reverse acc)
      let pagesFinal = pages ++ lastPages
      pure (pagesFinal, curPage, acc, newStream, False)
    E.VAllModesCommand aCom ->
      case aCom of
        -- Commands to do nothing.
        E.Relax -> continueUnchanged
        E.IgnoreSpaces -> continueUnchanged
        -- <space token> has no effect in vertical modes.
        E.AddSpace -> continueUnchanged
        -- \par does nothing in vertical mode.
        E.EndParagraph -> continueUnchanged
        -- Commands to modify the input stream.
        -- E.ChangeCase d bt ->
        --   modStream $ applyChangeCaseToStream newStream d bt
        -- Commands to modify the list.
        E.Assign E.Assignment {body = E.SelectFont fNr} -> do
          fontSel <- BL.VFontSelection <$> selectFont fNr
          modAccum $ fontSel : acc
        E.Assign E.Assignment {body = E.DefineFont cs fPath} -> do
          fontDef <- BL.VFontDefinition <$> defineFont cs fPath
          modAccum $ fontDef : acc
        E.AddPenalty n ->
          modAccum $ BL.VPenalty (evaluatePenalty n) : acc
        E.AddKern ln -> do
          mag <- gets magnification
          modAccum $ BL.VKern (evaluateKern mag ln) : acc
        E.AddGlue g -> do
          mag <- gets magnification
          modAccum $ BL.VGlue (evaluateGlue mag g) : acc
        E.AddRule {width = w, height = h, depth = d} -> do
          mag <- gets magnification
          evalW <- case w of
            Nothing -> gets $ unDesiredWidth . desiredWidth
            Just ln -> pure $ evaluateLength mag ln
          let evalH = case h of
                -- TODO.
                Nothing -> Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
                Just ln -> evaluateLength mag ln
              evalD = case d of
                Nothing -> 0
                Just ln -> evaluateLength mag ln
          let rule = B.Rule {width = evalW, height = evalH, depth = evalD}
          modAccum (BL.VRule rule : acc)
        -- Commands to start horizontal mode.
        E.StartParagraph indent -> addParagraphToPage indent
        E.Assign E.Assignment {body = m@E.DefineMacro {}} ->
          modStream $ defineMacro newStream m
    E.EnterHMode -> addParagraphToPage True

newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage [] Nothing

extractPages :: E.ExpandedStream -> BuildMonad ([B.Page], E.ExpandedStream)
extractPages = extractPagesInner [] newCurrentPage []
  where

    extractPagesInner
      :: [B.Page]
      -> CurrentPage
      -> [BL.BreakableVListElem]
      -> E.ExpandedStream
      -> BuildMonad ([B.Page], E.ExpandedStream)
    extractPagesInner pages curPage acc oldStream = do
      (PS.State {stateInput = newStream}, com) <- liftEither $ E.extractVModeCommand oldStream
      (procPages, procCurPage, procAcc, procStream, continue) <- processVCommand oldStream newStream pages curPage acc com
      if continue
        then extractPagesInner procPages procCurPage procAcc procStream
        else pure (procPages, procStream)
