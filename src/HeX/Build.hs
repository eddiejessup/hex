{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module HeX.Build where

import qualified Data.HashMap.Strict as HMap
import qualified Text.Megaparsec as PS
import qualified Data.Char as C
import Safe (headMay)
import Control.Monad.State.Lazy (get, gets, modify, liftIO, lift, MonadState)
import Control.Monad.Trans.Reader (asks, runReader, Reader)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Path

import qualified TFM
import qualified TFM.Character as TFMC
import Adjacent (Adjacency(..))

import HeX.Config as Config
import qualified HeX.Box as B
import qualified HeX.Config as Conf
import qualified HeX.BreakList as BL
import HeX.BreakList.Line (bestRoute, setParagraph)
import HeX.BreakList.Page (pageBreakJudgment, PageBreakJudgment(..), setPage)
import qualified HeX.Lex as Lex
import qualified HeX.Unit as Unit
import qualified HeX.Expand as Expand
import qualified HeX.Parse as P
import HeX.Parse (Stream, insertLexToken, insertLexTokens)

csToFontNr :: Lex.ControlSequenceLike -> Int
csToFontNr (Lex.ControlSequenceProper (Lex.ControlWord "thefont")) = Expand.theFontNr

currentFontInfo :: Monad m => MaybeT (Conf.ConfReaderT m) TFM.TexFont
currentFontInfo = do
  -- Maybe font number isn't set.
  maybeFontNr <- lift $ asks currentFontNr
  -- Or maybe there's no font where there should be.
  -- TODO: I think I can make this case impossible, maybe by storing the
  -- current font info directly instead of a lookup.
  fInfo <- HMap.lookup <$> MaybeT (return maybeFontNr) <*> lift (asks fontInfoMap)
  MaybeT $ return fInfo

defineFont :: (MonadState Config m, MonadIO m) =>
       Lex.ControlSequenceLike
    -> Path Rel File
    -> m B.FontDefinition
defineFont cs fPath = do
  theFontDirectories <- gets fontDirectories
  maybeFontDef <- liftIO $ runMaybeT $ ioFontDef theFontDirectories
  case maybeFontDef of
    Just fontDef@B.FontDefinition{fontInfo=font} -> do
      modify (\conf -> conf{fontInfoMap=HMap.insert fNr font $ fontInfoMap conf})
      return fontDef
    Nothing -> fail "Could not define font"
  where
    fNr = csToFontNr cs
    ioFontDef fontDirs = do
      fontPath <- findFilePath fPath fontDirs
      font <- liftIO $ TFM.readTFMFancy fontPath
      nonExtName <- Path.setFileExtension "" fPath
      let fontName = Path.toFilePath $ Path.filename nonExtName
      return B.FontDefinition { fontNr = fNr
                              , fontPath = fontPath
                              , fontName = fontName
                              , fontInfo = font
                              , scaleFactorRatio = 1.0
                              }

selectFont :: Monad m => Int -> ConfStateT m B.FontSelection
selectFont n = do
  modify (\conf -> conf{currentFontNr=Just n})
  return B.FontSelection{fontNr = n}

characterBox :: Monad m => Int -> MaybeT (Conf.ConfReaderT m) B.Character
characterBox code = do
  font <- currentFontInfo
  let toSP = TFM.designScaleSP font
  TFMC.Character{width=w, height=h, depth=d} <- MaybeT (return $ HMap.lookup code $ TFM.characters font)
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

spaceGlue :: Monad m => MaybeT (Conf.ConfReaderT m) BL.Glue
spaceGlue = do
  font@TFM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr} <- currentFontInfo
  let
    toSP = TFM.designScaleSP font
    toFlex = BL.finiteFlex . fromIntegral . toSP
  return BL.Glue{dimen=toSP d, stretch=toFlex str, shrink=toFlex shr}

evaluateNormalInteger :: P.NormalInteger -> Integer
evaluateNormalInteger (P.IntegerConstant n) = n

evaluateUnsignedNumber :: P.UnsignedNumber -> Integer
evaluateUnsignedNumber (P.NormalIntegerAsUNumber n) = evaluateNormalInteger n

evaluateNumber :: P.Number -> Integer
evaluateNumber (P.Number True u) = evaluateUnsignedNumber u
evaluateNumber (P.Number False u) = -(evaluateUnsignedNumber u)

evaluateFactor :: P.Factor -> Rational
evaluateFactor (P.NormalIntegerFactor n) = fromIntegral $ evaluateNormalInteger n
evaluateFactor (P.RationalConstant r) = r

evaluateUnit :: P.Unit -> Rational
evaluateUnit (P.PhysicalUnit _ u) = Unit.inScaledPoint u
-- TODO:
evaluateUnit (P.InternalUnit P.Em) = 10
evaluateUnit (P.InternalUnit P.Ex) = 10

evaluateNormalLength :: Int -> P.NormalLength -> Int
evaluateNormalLength mag (P.LengthSemiConstant f u@(P.PhysicalUnit isTrue _))
  = round $ evalF isTrue * evaluateUnit u
  where
    evalF False = evaluateFactor f
    evalF True = evalF False * 1000 / fromIntegral mag
evaluateNormalLength _ (P.LengthSemiConstant f u)
  = round $ evaluateFactor f * evaluateUnit u

evaluateULength :: Int -> P.UnsignedLength -> Int
evaluateULength mag (P.NormalLengthAsULength nLn) = evaluateNormalLength mag nLn

evaluateLength :: Int -> P.Length -> Int
evaluateLength mag (P.Length True uLn) = evaluateULength mag uLn
evaluateLength mag (P.Length False uLn) = -(evaluateULength mag uLn)

evaluateFlex :: Int -> Maybe P.Flex -> BL.GlueFlex
evaluateFlex mag (Just (P.FiniteFlex ln)) = BL.GlueFlex{factor=fromIntegral $ evaluateLength mag ln, order=0}
evaluateFlex _ (Just (P.FilFlex (P.FilLength True f ord))) = BL.GlueFlex{factor=evaluateFactor f, order=ord}
evaluateFlex _ (Just (P.FilFlex (P.FilLength False f ord))) = BL.GlueFlex{factor= -(evaluateFactor f), order=ord}
evaluateFlex _ Nothing = BL.noFlex

evaluateGlue :: Int -> P.Glue -> BL.Glue
evaluateGlue mag (P.ExplicitGlue dim str shr) =
  BL.Glue {
    dimen=evaluateLength mag dim,
    stretch=evaluateFlex mag str,
    shrink=evaluateFlex mag shr
  }

evaluateKern :: Int -> P.Length -> B.Kern
evaluateKern mag = B.Kern . evaluateLength mag

evaluatePenalty :: P.Number -> BL.Penalty
evaluatePenalty = BL.Penalty . fromIntegral . evaluateNumber

applyChangeCaseToStream :: Stream -> Expand.VDirection -> Expand.BalancedText -> Stream
applyChangeCaseToStream s d (Expand.BalancedText ts) = insertLexTokens s $ changeCase d <$> ts
  where
    -- Set the character code of each character token to its
    -- \uccode or \lccode value, if that value is non-zero.
    -- Don't change the category code.
    changeCase _ t@(Lex.ControlSequence _) = t
    changeCase dir t@Lex.CharCat{char=c} = t{Lex.char=modFunc c}
      where
        modFunc = C.ord . switch dir . C.chr
        switch Expand.Upward = C.toUpper
        switch Expand.Downward = C.toLower

defineMacro :: Stream -> P.AssignmentBody -> Stream
defineMacro stream (P.DefineMacro name params conts False False) =
  stream{P.csMap=HMap.insert name (Expand.MacroToken $ Expand.Macro params conts) (P.csMap stream)}

runReaderOnState :: MonadState r f => Reader r b -> f b
runReaderOnState f = runReader f <$> get

-- We build a paragraph list in reverse order.
extractParagraph :: [BL.BreakableHListElem] -> Stream -> ConfStateT IO ([BL.BreakableHListElem], Stream)
extractParagraph acc stream = case eCom of
  Left x -> error $ show x
  Right com -> case com of
    P.LeaveHMode ->
        -- Inner mode: forbidden. TODO.
        -- Outer mode: insert the rol sequence "\par" into the input. The control
        -- sequence's current meaning will be used, which might no longer be the \par
        -- primitive.
      -- (Note that we pass stream, not stream'.)
        do
        let parToken = Lex.ControlSequence $ Lex.ControlWord "par"
        modStream $ insertLexToken stream parToken

    P.AddCharacter{code=i} -> do
      charBox <- runReaderOnState (runMaybeT (characterBox i))
      hCharBox <- case BL.HCharacter <$> charBox of
          Just c -> return c
          Nothing -> fail "Could not get character info"
      modAccum $ hCharBox:acc

    P.HAllModesCommand aCom -> case aCom of

      -- Command to end recursion.
      -- \par: end the current paragraph.
      P.EndParagraph ->
        return (acc, stream')

      -- Commands to do nothing.
      P.Relax ->
        continueUnchanged
      P.IgnoreSpaces ->
        continueUnchanged

      -- Commands to modify the input stream.
      P.ChangeCase d bt ->
        modStream $ applyChangeCaseToStream stream' d bt

      -- Commands to modify the list.
      P.Assign P.Assignment{body=P.SelectFont fNr} -> do
        fontSel <- BL.HFontSelection <$> selectFont fNr
        modAccum $ fontSel:acc
      P.Assign P.Assignment{body=P.DefineFont cs fPath} -> do
        fontDef <- BL.HFontDefinition <$> defineFont cs fPath
        modAccum $ fontDef:acc
      P.AddPenalty n ->
        modAccum $ BL.HPenalty (evaluatePenalty n):acc
      P.AddKern ln -> do
        mag <- gets (`integerParameter` Magnification)
        modAccum $ BL.HKern (evaluateKern mag ln):acc
      P.AddGlue g -> do
        mag <- gets (`integerParameter` Magnification)
        modAccum $ BL.HGlue (evaluateGlue mag g):acc
      P.AddSpace -> do
        glue <- runReaderOnState (runMaybeT spaceGlue)
        hGlue <- case glue of
          Just sg -> return $ BL.HGlue sg
          Nothing -> fail "Could not get space glue"
        modAccum $ hGlue:acc
      -- \indent: An empty box of width \parindent is appended to the current
      -- list, and the space factor is set to 1000.
      -- TODO: Space factor.
      P.AddRule{width=w, height=h, depth=d} -> do
        mag <- gets (`integerParameter` Magnification)
        let
          evalW = case w of
            Nothing -> Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
            Just ln -> evaluateLength mag ln
          evalH = case h of
            Nothing -> Unit.toScaledPointApprox (10 :: Int) Unit.Point
            Just ln -> evaluateLength mag ln
          evalD = case d of
            Nothing -> 0
            Just ln -> evaluateLength mag ln
          rule = B.Rule{width=evalW, height=evalH, depth=evalD}
        modAccum $ BL.HRule rule:acc
      P.StartParagraph indent ->
        if indent then do
          indentBox <- gets parIndentBox
          modAccum (indentBox:acc)
        -- \noindent: has no effect in horizontal modes.
        else
          continueUnchanged
      P.ExpandMacro (Expand.Macro [] (Expand.BalancedText ts)) ->
          modStream $ insertLexTokens stream' ts
      P.Assign P.Assignment{body=m@P.DefineMacro{} } ->
          modStream $ defineMacro stream' m
  where
    (PS.State{stateInput=stream'}, eCom) = P.extractHModeCommand stream
    modAccum ac = extractParagraph ac stream'
    modStream = extractParagraph acc
    continueUnchanged = extractParagraph acc stream'

extractParagraphLineBoxes :: Bool -> Stream -> ConfStateT IO ([[B.HBoxElem]], Stream)
extractParagraphLineBoxes indent stream = do
  desiredW <- gets (`lengthParameter` DesiredWidth)
  lineTol <- gets (`integerParameter` LineTolerance)
  linePen <- gets (`integerParameter` LinePenalty)
  indentBox <- gets parIndentBox
  (hList, stream') <- extractParagraph [indentBox | indent] stream
  let
    getRoute = bestRoute desiredW lineTol linePen
    elemLists = setParagraph getRoute hList
  return (elemLists, stream')

-- current items, best cost, breakpoint for that cost.
type CurrentPage = ([BL.BreakableVListElem], Maybe Int, Maybe Int)

newCurrentPage :: ([BL.BreakableVListElem], Maybe Int, Maybe Int)
newCurrentPage = ([], Nothing, Nothing)

runPageBuilder :: Monad m => CurrentPage -> [BL.BreakableVListElem] -> ConfStateT m [B.Page]
runPageBuilder (cur, _, _) [] = do
  desiredH <- gets (`lengthParameter` DesiredHeight)
  return [setPage desiredH $ reverse cur]
runPageBuilder (cur, costBest, iBest) (x:xs)
  -- If the current vlist has no boxes, we discard a discardable item.
  | not $ any BL.isBox cur =
    if BL.isDiscardable x
      then runPageBuilder (cur, costBest, iBest) xs
      else runPageBuilder (x:cur, costBest, iBest) xs
  -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
  -- the cost c of breaking at this point.
  | BL.isDiscardable x = do
    desiredH <- gets (`lengthParameter` DesiredHeight)
    case BL.toBreakItem (Adjacency (headMay cur, x, headMay xs)) of
      -- If we can't break here, just add it to the list and continue.
      Nothing -> runPageBuilder (x:cur, costBest, iBest) xs
      Just brk ->
        let breakStatus = pageBreakJudgment cur brk desiredH
        in case (breakStatus, iBest) of
          (DoNotBreak, _) ->
            runPageBuilder (x:cur, costBest, iBest) xs
          -- I don't think this condition will ever be satisfied, but if we
          -- decide to break before any valid break-point has been considered,
          -- just carry on.
          (BreakPageAtBest, Nothing) ->
            runPageBuilder (x:cur, costBest, iBest) xs
          -- If c = ∞, we break at the best breakpoint so far.
          -- The current vlist material following that best breakpoint is
          -- returned to the recent contributions, to consider again.
          (BreakPageAtBest, Just iB) ->
            let
              -- the `reverse` will put both of these into reading order.
              (curNewPage, toReturn) = splitAt iB $ reverse cur
              newPage = setPage desiredH curNewPage
            in
              -- xs is also in reading order
              -- We didn't actually split at x: x was just what made us compute
              -- cost and notice we'd gone too far. So add it to the left-overs
              -- to return.
              (newPage:) <$> runPageBuilder ([], Nothing, Nothing) (toReturn ++ (x:xs))
          -- If p ≤ −10000, we know the best breakpoint is this one, so break
          -- here.
          (BreakPageHere, _) ->
            let
              -- the `reverse` will put this into reading order.
              newPage = setPage desiredH $ reverse cur
            in
              (newPage:) <$> runPageBuilder ([], Nothing, Nothing) xs
          -- If the resulting cost <= the smallest cost seen so far, remember
          -- the current breakpoint as the best so far.
          (TrackCost cHere, _) ->
            let
              thisCostAndI = (Just cHere, Just $ length cur)
              (costBestNew, iBestNew) =
                case costBest of
                  Nothing -> thisCostAndI
                  Just cBest ->
                    if cHere > cBest
                      then (costBest, iBest)
                      else thisCostAndI
            in runPageBuilder (x:cur, costBestNew, iBestNew) xs
  -- If we can't break here, just add it to the list and continue.
  | otherwise = runPageBuilder (x:cur, costBest, iBest) xs

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
addVListElem :: Monad m
             => [BL.BreakableVListElem]
             -> BL.BreakableVListElem
             -> ConfStateT m [BL.BreakableVListElem]
addVListElem acc e = case e of
  (BL.VListBox b) -> addVListBox b
  _ -> return $ e:acc
  where
    addVListBox b = do
      prevDepth <- gets (`specialIntegerParameter` PreviousBoxDepth)
      -- TODO:
      (BL.Glue baselineLength blineStretch blineShrink) <- gets (`glueParameter` BaselineGlue)
      blineLengthMin <- gets (`lengthParameter` BaselineLengthMin)
      minBlineGlue <- gets (`glueParameter` MinBaselineGlue)

      modify (\conf -> conf{specialIntegerParameter=updateFuncMap (specialIntegerParameter conf) PreviousBoxDepth (B.naturalDepth e)})
      return $
        if prevDepth <= -Unit.oneKPt then
          e:acc
        else
          let
            proposedBaselineLength = baselineLength - prevDepth - B.naturalHeight b
            -- Intuition: set the distance between baselines to \baselineskip, but no
            -- closer than \lineskiplimit [theBaselineLengthMin], in which case
            -- \lineskip [theMinBaselineGlue] is used.
            glue = BL.VGlue $ if proposedBaselineLength >= blineLengthMin
              then BL.Glue proposedBaselineLength blineStretch blineShrink
              else minBlineGlue
          in e:glue:acc

addVListElems :: Monad m
              => [BL.BreakableVListElem]
              -> [BL.BreakableVListElem]
              -> ConfStateT m [BL.BreakableVListElem]
addVListElems = foldM addVListElem

extractPages :: [B.Page] -> CurrentPage -> [BL.BreakableVListElem] -> Stream -> ConfStateT IO ([B.Page], Stream)
extractPages pages cur acc stream = case eCom of
  Left x -> error $ show x
  Right com -> case com of
    -- Command to end recursion.
    P.End -> do
      lastPage <- runPageBuilder cur (reverse acc)
      let pagesFinal = pages ++ lastPage
      return (pagesFinal, stream')

    P.VAllModesCommand aCom -> case aCom of

      -- Commands to do nothing.
      P.Relax ->
        continueUnchanged
      P.IgnoreSpaces ->
        continueUnchanged
      -- <space token> has no effect in vertical modes.
      P.AddSpace ->
        continueUnchanged
      -- \par does nothing in vertical mode.
      P.EndParagraph ->
        continueUnchanged

      -- Commands to modify the input stream.
      P.ChangeCase d bt ->
        modStream $ applyChangeCaseToStream stream' d bt

      -- Commands to modify the list.
      P.Assign P.Assignment{body=P.SelectFont fNr} -> do
        fontSel <- BL.VFontSelection <$> selectFont fNr
        modAccum $ fontSel:acc
      P.Assign P.Assignment{body=P.DefineFont cs fPath} -> do
        fontDef <- BL.VFontDefinition <$> defineFont cs fPath
        modAccum $ fontDef:acc
      P.AddPenalty n ->
        modAccum $ BL.VPenalty (evaluatePenalty n):acc
      P.AddKern ln -> do
        mag <- gets (`integerParameter` Magnification)
        modAccum $ BL.VKern (evaluateKern mag ln):acc
      P.AddGlue g -> do
        mag <- gets (`integerParameter` Magnification)
        modAccum $ BL.VGlue (evaluateGlue mag g):acc
      P.AddRule{width=w, height=h, depth=d} -> do
        desiredW <- gets (`lengthParameter` DesiredWidth)
        mag <- gets (`integerParameter` Magnification)
        let
          evalW = case w of
            Nothing -> desiredW
            Just ln -> evaluateLength mag ln
          evalH = case h of
            Nothing -> Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
            Just ln -> evaluateLength mag ln
          evalD = case d of
            Nothing -> 0
            Just ln -> evaluateLength mag ln
          rule = B.Rule{width=evalW, height=evalH, depth=evalD}
        modAccum (BL.VRule rule:acc)

      -- Commands to start horizontal mode.
      P.StartParagraph indent ->
        addParagraphToPage indent
      P.ExpandMacro (Expand.Macro [] (Expand.BalancedText ts)) ->
          modStream $ insertLexTokens stream' ts
      P.Assign P.Assignment{body=m@P.DefineMacro{} } ->
          modStream $ defineMacro stream' m
    P.EnterHMode ->
      addParagraphToPage True
  where
    (PS.State{stateInput=stream'}, eCom) = P.extractVModeCommand stream
    continueSamePage = extractPages pages cur
    modAccum ac = continueSamePage ac stream'
    modStream = continueSamePage acc
    continueUnchanged = continueSamePage acc stream'
    -- If the command shifts to horizontal mode, run '\indent', and re-read the
    -- stream as if the commands just seen hadn't been read.
    -- (Note that we pass "stream", not "stream'".)
    addParagraphToPage indent = do
      -- Paraboxes returned in reading order.
      (lineBoxes, stream'') <- extractParagraphLineBoxes indent stream
      desiredW <- gets (`lengthParameter` DesiredWidth)
      let toBox elemList = B.Box (B.HBoxContents elemList) (B.To desiredW)
      acc' <- addVListElems acc $ BL.VListBox . toBox <$> lineBoxes
      continueSamePage acc' stream''
