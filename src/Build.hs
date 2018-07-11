{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Build where

import qualified Data.IntMap.Strict as IMap
import Data.List (intersperse)
import System.Directory (doesFileExist)
import Path ((</>))
import qualified Path
import Data.Foldable (asum)
import qualified Text.Megaparsec as PS
import qualified Data.Char as C

import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified BoxDraw as B
import Adjacent (Adjacency(..))
import qualified Arrange as A
import qualified Lex
import qualified Unit
import qualified Expand

import qualified Parse as P
import Parse (Stream, insertLexToken, insertLexTokens)

type FontInfoMap = IMap.IntMap TFMM.TexFont

data State = State { currentFontNr :: Maybe Int
                   , fontInfoMap :: FontInfoMap } deriving Show

newState :: State
newState = State {currentFontNr=Nothing, fontInfoMap=IMap.empty}

theParIndent :: A.BreakableHListElem
theParIndent = A.HHBox B.HBox{contents=[]
                             , desiredLength=B.To $ fromIntegral $ Unit.toScaledPointApprox (20 :: Int) Unit.Point}

theDesiredWidth :: Int
theDesiredWidth = 30750000

theLineTolerance :: Int
theLineTolerance = 500

theLinePenalty :: Int
theLinePenalty = 10

theDesiredHeight :: Int
theDesiredHeight = 45000000

theInterLineGlue :: A.Glue
theInterLineGlue = A.Glue{dimen = 400000, stretch=A.noFlex, shrink=A.noFlex}

csToFontNr :: P.ControlSequenceLike -> Int
csToFontNr (P.ControlSequence (Lex.ControlWord "thefont")) = Expand.theFontNr

fontDir1 :: AbsPathToDir
(Just fontDir1) = Path.parseAbsDir "/Users/ejm/projects/hex"
fontDir2 :: AbsPathToDir
(Just fontDir2) = Path.parseAbsDir "/Users/ejm/projects/hex/support"
theFontDirectories :: [AbsPathToDir]
theFontDirectories = [fontDir1, fontDir2]

currentFontInfo :: State -> Maybe TFMM.TexFont
currentFontInfo state = do
  -- Maybe font number isn't set.
  fontNr <- currentFontNr state
  -- Or maybe there's no font where there should be.
  -- TODO: I think I can make this case impossible, maybe by storing the
  -- current font info directly instead of a lookup.
  IMap.lookup fontNr $ fontInfoMap state

type PathToFile b = Path.Path b Path.File
type RelPathToFile = PathToFile Path.Rel
type AbsPathToDir = Path.Path Path.Abs Path.Dir

pathIfExists :: PathToFile b -> IO (Maybe (PathToFile b))
pathIfExists p = do
  exists <- doesFileExist $ Path.toFilePath p
  return $ if exists then Just p else Nothing

firstExistingPath :: [PathToFile b] -> IO (Maybe (PathToFile b))
firstExistingPath ps = asum <$> mapM pathIfExists ps

findFilePath :: RelPathToFile -> [AbsPathToDir] -> IO (Maybe (PathToFile Path.Abs))
findFilePath name dirs = firstExistingPath $ fmap (</> name) dirs

defineFont :: State -> RelPathToFile -> Int -> IO (State, B.FontDefinition)
defineFont state fontRelPath nr = do
    fontPath <- findFilePath fontRelPath theFontDirectories
    case fontPath of
      Just p -> do
        font <- TFMM.readTFMFancy p
        nonExtName <- Path.setFileExtension "" fontRelPath
        let fontName = Path.toFilePath $ Path.filename nonExtName

        let
          fontDef = B.FontDefinition { fontNr = nr
                                   , fontPath = p
                                   , fontName = fontName
                                   , fontInfo = font
                                   , scaleFactorRatio = 1.0
                                   }
          stateNext = state{fontInfoMap=IMap.insert nr font $ fontInfoMap state}
        return (stateNext, fontDef)
      Nothing ->
        fail "No font found"

selectFont :: State -> Int -> (State, B.FontSelection)
selectFont state n =
  (state{currentFontNr=Just n}, B.FontSelection{fontNr = n})

characterBox :: State -> Int -> Maybe B.Character
characterBox state code = do
  font <- currentFontInfo state
  let toSP = TFMM.designScaleSP font
  TFMC.Character{width=w, height=h, depth=d} <- IMap.lookup code $ TFMM.characters font
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

spaceGlue :: State -> Maybe A.Glue
spaceGlue state = do
  font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr} <- currentFontInfo state
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

theMag :: Int
theMag = 1000

evaluateUnit :: P.Unit -> Rational
evaluateUnit (P.PhysicalUnit _ u) = Unit.inScaledPoint u
-- TODO:
evaluateUnit (P.InternalUnit P.Em) = 10
evaluateUnit (P.InternalUnit P.Ex) = 10

evaluateNormalLength :: P.NormalLength -> Int
evaluateNormalLength (P.LengthSemiConstant f u@(P.PhysicalUnit isTrue _))
  = round $ evalF isTrue * evaluateUnit u
  where
    evalF False = evaluateFactor f
    evalF True = evalF False * 1000 / fromIntegral theMag
evaluateNormalLength (P.LengthSemiConstant f u)
  = round $ evaluateFactor f * evaluateUnit u

evaluateULength :: P.UnsignedLength -> Int
evaluateULength (P.NormalLengthAsULength nLn) = evaluateNormalLength nLn

evaluateLength :: P.Length -> Int
evaluateLength (P.Length True uLn) = evaluateULength uLn
evaluateLength (P.Length False uLn) = -(evaluateULength uLn)

evaluateFlex :: Maybe P.Flex -> A.GlueFlex
evaluateFlex (Just (P.FiniteFlex ln)) = A.GlueFlex{factor=fromIntegral $ evaluateLength ln, order=0}
evaluateFlex (Just (P.FilFlex (P.FilLength True f ord))) = A.GlueFlex{factor=evaluateFactor f, order=ord}
evaluateFlex (Just (P.FilFlex (P.FilLength False f ord))) = A.GlueFlex{factor= -(evaluateFactor f), order=ord}
evaluateFlex Nothing = A.noFlex

evaluateGlue :: P.Glue -> A.Glue
evaluateGlue (P.ExplicitGlue dim str shr) =
  A.Glue {
    dimen=evaluateLength dim,
    stretch=evaluateFlex str,
    shrink=evaluateFlex shr
  }

evaluateKern :: P.Length -> B.Kern
evaluateKern = B.Kern . evaluateLength

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


-- We build a paragraph list in reverse order.
extractParagraph :: State -> [A.BreakableHListElem] -> Stream -> IO (State, [A.BreakableHListElem], Stream)
extractParagraph state acc stream =
  let (PS.State{stateInput=streamNext}, com) = P.extractHModeCommand stream
  in case com of
    Left x -> error $ show x
    Right (P.HAllModesCommand aCom) ->
      case aCom of
        P.Relax ->
          extractParagraph state acc streamNext
        P.IgnoreSpaces ->
          extractParagraph state acc streamNext
        P.ChangeCase d bt -> do
          let streamNext' = applyChangeCaseToStream streamNext d bt
          extractParagraph state acc streamNext'
        P.Assign P.Assignment{body=P.SelectFont fNr} ->
          do
          let (stateNext, fontSel) = selectFont state fNr
          extractParagraph stateNext (A.HFontSelection fontSel:acc) streamNext
        P.Assign P.Assignment{body=P.DefineFont cs fPath} ->
          do
          let fNr = csToFontNr cs
          (stateNext, fontDef) <- defineFont state fPath fNr
          extractParagraph stateNext (A.HFontDefinition fontDef:acc) streamNext
        P.AddPenalty n ->
          extractParagraph state ((A.HPenalty $ evaluatePenalty n):acc) streamNext
        P.AddKern ln ->
          extractParagraph state ((A.HKern $ evaluateKern ln):acc) streamNext
        P.AddGlue g ->
          extractParagraph state (A.HGlue (evaluateGlue g):acc) streamNext
        P.AddSpace ->
          do
          glue <- case spaceGlue state of
            Just sg -> return $ A.HGlue sg
            Nothing -> fail "Could not get space glue"
          extractParagraph state (glue:acc) streamNext
        -- \indent: An empty box of width \parindent is appended to the current
        -- list, and the space factor is set to 1000.
        -- TODO: Space factor.
        P.AddRule{width=w, height=h, depth=d} -> do
          let
            evalW = case w of
              Nothing -> Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
              Just ln -> evaluateLength ln
            evalH = case h of
              Nothing -> Unit.toScaledPointApprox (10 :: Int) Unit.Point
              Just ln -> evaluateLength ln
            evalD = case d of
              Nothing -> 0
              Just ln -> evaluateLength ln
            rule = B.Rule{width=evalW, height=evalH, depth=evalD}
          extractParagraph state (A.HRule rule:acc) streamNext
        P.StartParagraph True ->
          extractParagraph state (theParIndent:acc) streamNext
        -- \noindent: has no effect in horizontal modes.
        P.StartParagraph False ->
          extractParagraph state acc streamNext
        -- \par: end the current paragraph.
        P.EndParagraph ->
          return (state, acc, streamNext)
    Right P.AddCharacter{code=i} ->
      do
      charBox <- case characterBox state i of
        Just c -> return $ A.HCharacter c
        Nothing -> fail "Could not get character info"
      extractParagraph state (charBox:acc) streamNext
    Right P.LeaveHMode ->
      -- Inner mode: forbidden. TODO.
      -- Outer mode: insert the control sequence "\par" into the input. The control
      -- sequence's current meaning will be used, which might no longer be the \par
      -- primitive.
    -- (Note that we pass 'stream', not 'streamNext'.)
      do
      let parToken = Lex.ControlSequence $ Lex.ControlWord "par"
      extractParagraph state acc $ insertLexToken stream parToken

extractBoxedParagraph :: Bool -> Int -> Int -> Int -> A.Glue -> State -> Stream -> IO (State, [A.BreakableVListElem], Stream)
extractBoxedParagraph indent desiredWidth lineTolerance linePenalty interLineGlue state stream = do
  let
    initial True = [theParIndent]
    initial False = []
  (stateNext, hList, streamNext) <- extractParagraph state (initial indent) stream
  let
    lineBoxes = A.setParagraph desiredWidth lineTolerance linePenalty hList
    paraBoxes = intersperse (A.VGlue interLineGlue) lineBoxes
  return (stateNext, paraBoxes, streamNext)

-- current items, best cost, breakpoint for that cost.
type CurrentPage = ([A.BreakableVListElem], Maybe Int, Maybe Int)

newCurrentPage :: ([A.BreakableVListElem], Maybe Int, Maybe Int)
newCurrentPage = ([], Nothing, Nothing)

headOrNothing :: [a] -> Maybe a
headOrNothing ts = case ts of
  [] -> Nothing
  t:_ -> Just t

runPageBuilder :: CurrentPage -> [A.BreakableVListElem] -> [B.Page]
runPageBuilder (cur, _, _) [] = [A.setPage theDesiredHeight $ reverse cur]
runPageBuilder (cur, costBest, iBest) (x:xs)
  -- If the current vlist has no boxes, we discard a discardable item.
  | not $ any A.isBox cur =
    if A.isDiscardable x
      then runPageBuilder (cur, costBest, iBest) xs
      else runPageBuilder (x:cur, costBest, iBest) xs
  -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
  -- the cost c of breaking at this point.
  | A.isDiscardable x =
    case A.toBreakItem (Adjacency (headOrNothing cur, x, headOrNothing xs)) of
      -- If we can't break here, just add it to the list and continue.
      Nothing -> runPageBuilder (x:cur, costBest, iBest) xs
      Just brk ->
        let breakStatus = A.pageBreakJudgment cur brk theDesiredHeight
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
              newPage = A.setPage theDesiredHeight curNewPage
            in
              -- xs is also in reading order
              -- We didn't actually split at x: x was just what made us compute
              -- cost and notice we'd gone too far. So add it to the left-overs
              -- to return.
              newPage:runPageBuilder ([], Nothing, Nothing) (toReturn ++ (x:xs))
          -- If p ≤ −10000, we know the best breakpoint is this one, so break
          -- here.
          (A.BreakPageHere, _) ->
            let
              -- the `reverse` will put this into reading order.
              newPage = A.setPage theDesiredHeight $ reverse cur
            in
              newPage:runPageBuilder ([], Nothing, Nothing) xs
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

addParagraphToPage :: State
                   -> [B.Page]
                   -> CurrentPage
                   -> [A.BreakableVListElem]
                   -> Stream
                   -> Bool
                   -> IO (State, [B.Page], Stream)
addParagraphToPage state pages cur acc stream indent
  = do
    -- Paraboxes returned in normal order.
    -- TODO: Insert inter-line glue properly, between lines, instead of just
    -- between paragraphs.
    (stateNext, paraBoxes, streamNext) <- extractBoxedParagraph indent theDesiredWidth theLineTolerance theLinePenalty theInterLineGlue state stream
    -- But acc accumulates in reverse order.
    let accNext = reverse paraBoxes ++ (A.VGlue theInterLineGlue:acc)
    extractPages stateNext pages cur accNext streamNext

extractPages :: State -> [B.Page] -> CurrentPage -> [A.BreakableVListElem] -> Stream -> IO (State, [B.Page], Stream)
extractPages state pages cur acc stream =
  let (PS.State{stateInput=streamNext}, com) = P.extractVModeCommand stream
  in case com of
    Left x -> error $ show x
    -- If the command shifts to horizontal mode, run '\indent', and re-read the
    -- stream as if the commands just seen hadn't been read.
    -- (Note that we pass 'stream', not 'streamNext'.)
    Right P.EnterHMode ->
      addParagraphToPage state pages cur acc stream True
    Right P.End -> do
      let pagesFinal = pages ++ runPageBuilder cur (reverse acc)
      return (state, pagesFinal, streamNext)
    Right (P.VAllModesCommand aCom) ->
      case aCom of
        P.Relax ->
          extractPages state pages cur acc streamNext
        P.IgnoreSpaces ->
          extractPages state pages cur acc streamNext
        P.ChangeCase d bt -> do
          let streamNext' = applyChangeCaseToStream streamNext d bt
          extractPages state pages cur acc streamNext'
        P.Assign P.Assignment{body=P.SelectFont fNr} ->
          do
          let (stateNext, fontSel) = selectFont state fNr
          extractPages stateNext pages cur (A.VFontSelection fontSel:acc) streamNext
        -- \par does nothing in vertical mode.
        P.Assign P.Assignment{body=P.DefineFont cs fPath} ->
          do
          let fNr = csToFontNr cs
          (stateNext, fontDef) <- defineFont state fPath fNr
          extractPages stateNext pages cur (A.VFontDefinition fontDef:acc) streamNext
        P.AddPenalty n ->
          extractPages state pages cur ((A.VPenalty $ evaluatePenalty n):acc) streamNext
        P.AddKern ln ->
          extractPages state pages cur ((A.VKern $ evaluateKern ln):acc) streamNext
        P.AddGlue g ->
          extractPages state pages cur (A.VGlue (evaluateGlue g):acc) streamNext
        -- <space token> has no effect in vertical modes.
        P.AddSpace ->
          extractPages state pages cur acc streamNext
        P.AddRule{width=w, height=h, depth=d} -> do
          let
            evalW = case w of
              Nothing -> theDesiredWidth
              Just ln -> evaluateLength ln
            evalH = case h of
              Nothing -> Unit.toScaledPointApprox (0.4 :: Rational) Unit.Point
              Just ln -> evaluateLength ln
            evalD = case d of
              Nothing -> 0
              Just ln -> evaluateLength ln
            rule = B.Rule{width=evalW, height=evalH, depth=evalD}
          extractPages state pages cur (A.VRule rule:acc) streamNext
        P.StartParagraph indent ->
          addParagraphToPage state pages cur acc stream indent
        P.EndParagraph ->
          extractPages state pages cur acc streamNext
