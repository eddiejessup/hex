{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Build where

import qualified Data.HashMap.Strict as HMap
import System.Directory (doesFileExist)
import Path ((</>))
import qualified Path
import qualified Text.Megaparsec as PS
import qualified Data.Char as C
import Safe (headMay)
import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified BoxDraw as B
import Adjacent (Adjacency(..))
import qualified Arrange as A
import qualified Lex
import qualified Unit
import qualified Expand
import Control.Monad.State.Lazy (StateT, get, gets, modify, liftIO, lift, MonadState)
import Control.Monad.Trans.Reader (ReaderT, asks, runReader, Reader)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad (foldM)
import Control.Monad.Extra (findM)

import qualified Parse as P
import Parse (Stream, insertLexToken, insertLexTokens)

type FontInfoMap = HMap.HashMap Int TFMM.TexFont
-- type IntegerParameterMap = HMap.HashMap Int TFMM.TexFont

data Config = Config { currentFontNr :: Maybe Int
                     , fontInfoMap :: FontInfoMap
                     , desiredWidth :: Int
                     , lineTolerance :: Int
                     , linePenalty :: Int
                     , desiredHeight :: Int
                     , magnification :: Int
                       -- Minimum distance between baselines.
                     , baselineLengthMin :: Int
                       -- Aimed actual distance between baselines.
                     , baselineGlue :: A.Glue
                     , minBaselineGlue :: A.Glue
                     , parIndent :: B.HBox
                     , previousBoxDepth :: Int } deriving Show

newConfig :: Config
newConfig = Config { currentFontNr=Nothing
                   , fontInfoMap=HMap.empty
                   , desiredWidth=30750000
                   , lineTolerance=500
                   , linePenalty=10
                   , desiredHeight=37500000
                   , magnification=1000
                   , baselineLengthMin=0
                   , baselineGlue=A.Glue (Unit.toScaledPointApprox (12 :: Int) Unit.Point) A.noFlex A.noFlex
                   , minBaselineGlue=A.Glue (Unit.toScaledPointApprox (1 :: Int) Unit.Point) A.noFlex A.noFlex
                   , parIndent=B.HBox{contents=[]
                                     , desiredLength=B.To $ Unit.toScaledPointApprox (20 :: Int) Unit.Point}
                   , previousBoxDepth= -Unit.oneKPt }

csToFontNr :: P.ControlSequenceLike -> Int
csToFontNr (P.ControlSequence (Lex.ControlWord "thefont")) = Expand.theFontNr

fontDir1 :: AbsPathToDir
(Just fontDir1) = Path.parseAbsDir "/Users/ejm/projects/hex"
fontDir2 :: AbsPathToDir
(Just fontDir2) = Path.parseAbsDir "/Users/ejm/projects/hex/support"
theFontDirectories :: [AbsPathToDir]
theFontDirectories = [fontDir1, fontDir2]

type ConfStateT = StateT Config
type ConfReaderT = ReaderT Config

currentFontInfo :: Monad m => MaybeT (ConfReaderT m) TFMM.TexFont
currentFontInfo = do
  -- Maybe font number isn't set.
  maybeFontNr <- lift $ asks currentFontNr
  -- Or maybe there's no font where there should be.
  -- TODO: I think I can make this case impossible, maybe by storing the
  -- current font info directly instead of a lookup.
  fInfo <- HMap.lookup <$> MaybeT (return maybeFontNr) <*> lift (asks fontInfoMap)
  MaybeT $ return fInfo

type PathToFile b = Path.Path b Path.File
type RelPathToFile = PathToFile Path.Rel
type AbsPathToDir = Path.Path Path.Abs Path.Dir

firstExistingPath :: [PathToFile b] -> MaybeT IO (PathToFile b)
firstExistingPath ps =
  -- Make a MaybeT of...
  MaybeT $
    -- The result of an IO function, lifted to our MaybeT IO monad.
    liftIO $
      -- namely, 'find', but using a predicate which acts in the IO monad,
      -- and which tests if a file exists.
      findM (doesFileExist . Path.toFilePath) ps

findFilePath :: RelPathToFile -> [AbsPathToDir] -> MaybeT IO (PathToFile Path.Abs)
findFilePath name dirs = firstExistingPath $ fmap (</> name) dirs

defineFont :: RelPathToFile -> Int -> MaybeT IO B.FontDefinition
defineFont fontRelPath nr = do
    fontPath <- findFilePath fontRelPath theFontDirectories
    font <- liftIO $ TFMM.readTFMFancy fontPath
    nonExtName <- Path.setFileExtension "" fontRelPath
    let fontName = Path.toFilePath $ Path.filename nonExtName
    return B.FontDefinition { fontNr = nr
                            , fontPath = fontPath
                            , fontName = fontName
                            , fontInfo = font
                            , scaleFactorRatio = 1.0
                            }

selectFont :: Monad m => Int -> ConfStateT m B.FontSelection
selectFont n = do
  modify (\conf -> conf{currentFontNr=Just n})
  return B.FontSelection{fontNr = n}

characterBox :: Monad m => Int -> MaybeT (ConfReaderT m) B.Character
characterBox code = do
  font <- currentFontInfo
  let toSP = TFMM.designScaleSP font
  TFMC.Character{width=w, height=h, depth=d} <- MaybeT (return $ HMap.lookup code $ TFMM.characters font)
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

spaceGlue :: Monad m => MaybeT (ConfReaderT m) A.Glue
spaceGlue = do
  font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr} <- currentFontInfo
  let
    toSP = TFMM.designScaleSP font
    toFlex = A.finiteFlex . fromIntegral . toSP
  return A.Glue{dimen=toSP d, stretch=toFlex str, shrink=toFlex shr}

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

evaluateFlex :: Int -> Maybe P.Flex -> A.GlueFlex
evaluateFlex mag (Just (P.FiniteFlex ln)) = A.GlueFlex{factor=fromIntegral $ evaluateLength mag ln, order=0}
evaluateFlex _ (Just (P.FilFlex (P.FilLength True f ord))) = A.GlueFlex{factor=evaluateFactor f, order=ord}
evaluateFlex _ (Just (P.FilFlex (P.FilLength False f ord))) = A.GlueFlex{factor= -(evaluateFactor f), order=ord}
evaluateFlex _ Nothing = A.noFlex

evaluateGlue :: Int -> P.Glue -> A.Glue
evaluateGlue mag (P.ExplicitGlue dim str shr) =
  A.Glue {
    dimen=evaluateLength mag dim,
    stretch=evaluateFlex mag str,
    shrink=evaluateFlex mag shr
  }

evaluateKern :: Int -> P.Length -> B.Kern
evaluateKern mag = B.Kern . evaluateLength mag

evaluatePenalty :: P.Number -> A.Penalty
evaluatePenalty = A.Penalty . fromIntegral . evaluateNumber

applyChangeCaseToStream :: Stream -> Expand.VDirection -> P.BalancedText -> Stream
applyChangeCaseToStream s d (P.BalancedText ts) = insertLexTokens s $ changeCase d <$> ts
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

runReaderOnState :: MonadState r f => Reader r b -> f b
runReaderOnState f = runReader f <$> get

-- We build a paragraph list in reverse order.
extractParagraph :: [A.BreakableHListElem] -> Stream -> ConfStateT IO ([A.BreakableHListElem], Stream)
extractParagraph acc stream =
  case eCom of
    Left x -> error $ show x
    Right com -> case com of
      P.LeaveHMode ->
          -- Inner mode: forbidden. TODO.
          -- Outer mode: insert the control sequence "\par" into the input. The control
          -- sequence's current meaning will be used, which might no longer be the \par
          -- primitive.
        -- (Note that we pass stream, not stream'.)
          do
          let parToken = Lex.ControlSequence $ Lex.ControlWord "par"
          modStream $ insertLexToken stream parToken

      P.AddCharacter{code=i} -> do
        charBox <- runReaderOnState (runMaybeT (characterBox i))
        hCharBox <- case A.HCharacter <$> charBox of
            Just c -> return c
            Nothing -> fail "Could not get character info"
        modAccum $ hCharBox:acc

      (P.HAllModesCommand aCom) -> case aCom of

        -- \par: end the current paragraph.
        P.EndParagraph ->
          return (acc, stream')

        (P.ChangeCase d bt) ->
          modStream $ applyChangeCaseToStream stream' d bt

        P.Relax ->
          continueUnchanged
        P.IgnoreSpaces ->
          continueUnchanged
        P.Assign P.Assignment{body=P.SelectFont fNr} ->
          do
          fontSel <- selectFont fNr
          modAccum $ A.HFontSelection fontSel:acc
        P.Assign P.Assignment{body=P.DefineFont cs fPath} ->
          do
          let fNr = csToFontNr cs
          ret <- liftIO $ runMaybeT $ defineFont fPath fNr
          case ret of
            Just fontDef@B.FontDefinition{fontInfo=font} -> do
              modify (\conf -> conf{fontInfoMap=HMap.insert fNr font $ fontInfoMap conf})
              modAccum $ A.HFontDefinition fontDef:acc
            Nothing -> fail "Could not define font"
        P.AddPenalty n ->
          modAccum $ (A.HPenalty $ evaluatePenalty n):acc
        P.AddKern ln -> do
          mag <- gets magnification
          modAccum $ (A.HKern $ evaluateKern mag ln):acc
        P.AddGlue g -> do
          mag <- gets magnification
          modAccum $ A.HGlue (evaluateGlue mag g):acc
        P.AddSpace -> do
          glue <- runReaderOnState (runMaybeT spaceGlue)
          hGlue <- case glue of
            Just sg -> return $ A.HGlue sg
            Nothing -> fail "Could not get space glue"
          modAccum $ hGlue:acc
        -- \indent: An empty box of width \parindent is appended to the current
        -- list, and the space factor is set to 1000.
        -- TODO: Space factor.
        P.AddRule{width=w, height=h, depth=d} -> do
          mag <- gets magnification
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
          modAccum $ A.HRule rule:acc
        P.StartParagraph indent ->
          if indent then do
            parInd <- gets parIndent
            modAccum (A.HHBox parInd:acc)
          -- \noindent: has no effect in horizontal modes.
          else
            continueUnchanged
  where
    (PS.State{stateInput=stream'}, eCom) = P.extractHModeCommand stream
    modAccum ac = extractParagraph ac stream'
    modStream = extractParagraph acc
    continueUnchanged = extractParagraph acc stream'


extractParagraphLineBoxes :: Bool -> Stream -> ConfStateT IO ([B.HBox], Stream)
extractParagraphLineBoxes indent stream = do
  desiredW <- gets desiredWidth
  lineTol <- gets lineTolerance
  linePen <- gets linePenalty
  parInd <- gets parIndent
  let
    initial True = [A.HHBox parInd]
    initial False = []

  (hList, streamNext) <- extractParagraph (initial indent) stream
  let
    lineBoxes = A.setParagraph desiredW lineTol linePen hList
  return (lineBoxes, streamNext)

-- current items, best cost, breakpoint for that cost.
type CurrentPage = ([A.BreakableVListElem], Maybe Int, Maybe Int)

newCurrentPage :: ([A.BreakableVListElem], Maybe Int, Maybe Int)
newCurrentPage = ([], Nothing, Nothing)

runPageBuilder :: Monad m => CurrentPage -> [A.BreakableVListElem] -> ConfStateT m [B.Page]
runPageBuilder (cur, _, _) [] = do
  desiredH <- gets desiredHeight
  return [A.setPage desiredH $ reverse cur]
runPageBuilder (cur, costBest, iBest) (x:xs)
  -- If the current vlist has no boxes, we discard a discardable item.
  | not $ any A.isBox cur =
    if A.isDiscardable x
      then runPageBuilder (cur, costBest, iBest) xs
      else runPageBuilder (x:cur, costBest, iBest) xs
  -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
  -- the cost c of breaking at this point.
  | A.isDiscardable x = do
    desiredH <- gets desiredHeight
    case A.toBreakItem (Adjacency (headMay cur, x, headMay xs)) of
      -- If we can't break here, just add it to the list and continue.
      Nothing -> runPageBuilder (x:cur, costBest, iBest) xs
      Just brk ->
        let breakStatus = A.pageBreakJudgment cur brk desiredH
        in case (breakStatus, iBest) of
          (A.DoNotBreak, _) ->
            runPageBuilder (x:cur, costBest, iBest) xs
          -- I don't think this condition will ever be satisfied, but if we
          -- decide to break before any valid break-point has been considered,
          -- just carry on.
          (A.BreakPageAtBest, Nothing) ->
            runPageBuilder (x:cur, costBest, iBest) xs
          -- If c = ∞, we break at the best breakpoint so far.
          -- The current vlist material following that best breakpoint is
          -- returned to the recent contributions, to consider again.
          (A.BreakPageAtBest, Just iB) ->
            let
              -- the `reverse` will put both of these into reading order.
              (curNewPage, toReturn) = splitAt iB $ reverse cur
              newPage = A.setPage desiredH curNewPage
            in
              -- xs is also in reading order
              -- We didn't actually split at x: x was just what made us compute
              -- cost and notice we'd gone too far. So add it to the left-overs
              -- to return.
              (newPage:) <$> runPageBuilder ([], Nothing, Nothing) (toReturn ++ (x:xs))
          -- If p ≤ −10000, we know the best breakpoint is this one, so break
          -- here.
          (A.BreakPageHere, _) ->
            let
              -- the `reverse` will put this into reading order.
              newPage = A.setPage desiredH $ reverse cur
            in
              (newPage:) <$> runPageBuilder ([], Nothing, Nothing) xs
          -- If the resulting cost <= the smallest cost seen so far, remember
          -- the current breakpoint as the best so far.
          (A.TrackCost cHere, _) ->
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
             => [A.BreakableVListElem]
             -> A.BreakableVListElem
             -> ConfStateT m [A.BreakableVListElem]
addVListElem acc e = case e of
  (A.VHBox b) -> addVListBox b
  (A.VVBox b) -> addVListBox b
  _ -> return $ e:acc
  where
    addVListBox b = do
      prevDepth <- gets previousBoxDepth
      -- TODO:
      (A.Glue baselineLength blineStretch blineShrink) <- gets baselineGlue
      blineLengthMin <- gets baselineLengthMin
      minBlineGlue <- gets minBaselineGlue

      modify (\conf -> conf{previousBoxDepth=B.naturalDepth e})
      return $
        if prevDepth <= -Unit.oneKPt then
          e:acc
        else
          let
            proposedBaselineLength = baselineLength - prevDepth - B.naturalHeight b
            -- Intuition: set the distance between baselines to \baselineskip, but no
            -- closer than \lineskiplimit [theBaselineLengthMin], in which case
            -- \lineskip [theMinBaselineGlue] is used.
            glue = A.VGlue $ if proposedBaselineLength >= blineLengthMin
              then A.Glue proposedBaselineLength blineStretch blineShrink
              else minBlineGlue
          in e:glue:acc

addVListElems :: Monad m
              => [A.BreakableVListElem]
              -> [A.BreakableVListElem]
              -> ConfStateT m [A.BreakableVListElem]
addVListElems = foldM addVListElem

addParagraphToPage :: [B.Page]
                   -> CurrentPage
                   -> [A.BreakableVListElem]
                   -> Stream
                   -> Bool
                   -> ConfStateT IO ([B.Page], Stream)
addParagraphToPage pages cur acc stream indent
  = do
    -- Paraboxes returned in reading order.
    (lineBoxes, streamNext) <- extractParagraphLineBoxes indent stream
    acc' <- addVListElems acc $ A.VHBox <$> lineBoxes
    extractPages pages cur acc' streamNext

extractPages :: [B.Page] -> CurrentPage -> [A.BreakableVListElem] -> Stream -> ConfStateT IO ([B.Page], Stream)
extractPages pages cur acc stream =
  let (PS.State{stateInput=streamNext}, com) = P.extractVModeCommand stream
  in case com of
    Left x -> error $ show x
    -- If the command shifts to horizontal mode, run '\indent', and re-read the
    -- stream as if the commands just seen hadn't been read.
    -- (Note that we pass 'stream', not 'streamNext'.)
    Right P.EnterHMode ->
      addParagraphToPage pages cur acc stream True
    Right P.End -> do
      lastPage <- runPageBuilder cur (reverse acc)
      let pagesFinal = pages ++ lastPage
      return (pagesFinal, streamNext)
    Right (P.VAllModesCommand aCom) ->
      case aCom of
        P.Relax ->
          extractPages pages cur acc streamNext
        P.IgnoreSpaces ->
          extractPages pages cur acc streamNext
        P.ChangeCase d bt -> do
          let streamNext' = applyChangeCaseToStream streamNext d bt
          extractPages pages cur acc streamNext'
        P.Assign P.Assignment{body=P.SelectFont fNr} ->
          do
          fontSel <- selectFont fNr
          extractPages pages cur (A.VFontSelection fontSel:acc) streamNext
        -- \par does nothing in vertical mode.
        P.Assign P.Assignment{body=P.DefineFont cs fPath} ->
          do
          let fNr = csToFontNr cs
          ret <- liftIO $ runMaybeT $ defineFont fPath fNr
          case ret of
            Just fontDef@B.FontDefinition{fontInfo=font} -> do
              modify (\conf -> conf{fontInfoMap=HMap.insert fNr font $ fontInfoMap conf})
              extractPages pages cur (A.VFontDefinition fontDef:acc) streamNext
            Nothing -> fail "Could not define font"
        P.AddPenalty n ->
          extractPages pages cur ((A.VPenalty $ evaluatePenalty n):acc) streamNext
        P.AddKern ln -> do
          mag <- gets magnification
          extractPages pages cur ((A.VKern $ evaluateKern mag ln):acc) streamNext
        P.AddGlue g -> do
          mag <- gets magnification
          extractPages pages cur (A.VGlue (evaluateGlue mag g):acc) streamNext
        -- <space token> has no effect in vertical modes.
        P.AddSpace ->
          extractPages pages cur acc streamNext
        P.AddRule{width=w, height=h, depth=d} -> do
          desiredW <- gets desiredWidth
          mag <- gets magnification
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
          extractPages pages cur (A.VRule rule:acc) streamNext
        P.StartParagraph indent ->
          addParagraphToPage pages cur acc stream indent
        P.EndParagraph ->
          extractPages pages cur acc streamNext
