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

import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified BoxDraw as B
import qualified Arrange as A
import qualified Lex
import qualified Unit
import qualified Expand

import qualified Parse as P
import Parse (Stream, insertLexToken)

type FontInfoMap = IMap.IntMap TFMM.TexFont

data State = State { currentFontNr :: Maybe Int
                   , fontInfoMap :: FontInfoMap } deriving Show

newState :: State
newState = State {currentFontNr=Nothing, fontInfoMap=IMap.empty}

theParIndent :: A.BreakableHListElem
theParIndent = A.HHBox B.HBox{contents=[], desiredLength=B.To $ fromIntegral $ Unit.toScaledPointApprox (20 :: Int) Unit.Point}

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
    toFlex = A.finiteFlex . toSP
  return A.Glue{dimen=toSP d, stretch=toFlex str, shrink=toFlex shr}

evaluateNormalInteger :: P.NormalInteger -> Int
evaluateNormalInteger (P.IntegerConstant n) = n

evaluateUNr :: P.UnsignedNumber -> Int
evaluateUNr (P.NormalIntegerAsUNumber n) = evaluateNormalInteger n

evaluateFactor :: P.Factor -> Int
evaluateFactor (P.NormalIntegerFactor n) = evaluateNormalInteger n

evaluateNormalLengthToSP :: P.NormalLength -> Int
evaluateNormalLengthToSP (P.LengthSemiConstant f (P.PhysicalUnit False u)) = Unit.toScaledPointApprox (evaluateFactor f) u

evaluateULnToSP :: P.UnsignedLength -> Int
evaluateULnToSP (P.NormalLengthAsULength nLn) = evaluateNormalLengthToSP nLn

evaluateLnToSp :: P.Length -> Int
evaluateLnToSp (P.Length True uLn) = evaluateULnToSP uLn
evaluateLnToSp (P.Length False uLn) = -(evaluateULnToSP uLn)

-- We build a paragraph list in reverse order.
extractParagraph :: State -> [A.BreakableHListElem] -> Stream -> IO (State, [A.BreakableHListElem], Stream)
extractParagraph state acc stream =
  let (PS.State{stateInput=streamNext}, com) = P.extractHModeCommand stream
  in case com of
    Left x -> error $ show x
    Right (P.HAllModesCommand aCom) ->
      case aCom of
        P.Assign P.Assignment{body=P.DefineFont cs fPath} ->
          do
          let fNr = csToFontNr cs
          (stateNext, fontDef) <- defineFont state fPath fNr
          extractParagraph stateNext (A.HFontDefinition fontDef:acc) streamNext
        P.Assign P.Assignment{body=P.SelectFont fNr} ->
          do
          let (stateNext, fontSel) = selectFont state fNr
          extractParagraph stateNext (A.HFontSelection fontSel:acc) streamNext
        P.Relax ->
          extractParagraph state acc streamNext
        -- \par: end the current paragraph.
        P.EndParagraph ->
          return (state, acc, streamNext)
        P.AddKern ln ->
          extractParagraph state ((A.HKern $ B.Kern $ evaluateLnToSp ln):acc) streamNext
        -- \indent: An empty box of width \parindent is appended to the current
        -- list, and the space factor is set to 1000.
        -- TODO: Space factor.
        P.StartParagraph True ->
          extractParagraph state (theParIndent:acc) streamNext
        -- \noindent: has no effect in horizontal modes.
        P.StartParagraph False ->
          extractParagraph state acc streamNext
        P.AddSpace ->
          do
          glue <- case spaceGlue state of
            Just sg -> return $ A.HGlue sg
            Nothing -> fail "Could not get space glue"
          extractParagraph state (glue:acc) streamNext
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

addParagraphToPage :: State -> [B.Page] -> [A.BreakableVListElem] -> Stream -> Bool -> IO (State, [B.Page], [A.BreakableVListElem], Stream)
addParagraphToPage state pages acc stream indent
  = do
    -- Paraboxes returned in normal order.
    (stateNext, paraBoxes, streamNext) <- extractBoxedParagraph indent theDesiredWidth theLineTolerance theLinePenalty theInterLineGlue state stream
    let
      breakItem = A.GlueBreak theInterLineGlue
      accBreak = A.VGlue theInterLineGlue:reverse paraBoxes
      accNoBreak = accBreak ++ acc
      -- TODO: Discard when adding to empty page.
      -- TODO: Keep best rather than taking last.
      pen = A.breakPenalty breakItem
      -- Expects normal order.
      stat = A.listGlueSetRatio theDesiredHeight $ reverse accNoBreak
      bad = A.listStatusBadness stat
      cost = A.pageCost pen bad 0
      -- Expects normal order.
      page = B.Page $ reverse $ A.setListElems stat acc
    if (cost == A.oneMillion) || (pen <= -A.tenK)
      then extractPages stateNext (page:pages) accBreak streamNext
      else extractPages stateNext pages accNoBreak streamNext

extractPages :: State -> [B.Page] -> [A.BreakableVListElem] -> Stream -> IO (State, [B.Page], [A.BreakableVListElem], Stream)
extractPages state pages acc stream =
  let (PS.State{stateInput=streamNext}, com) = P.extractVModeCommand stream
  in case com of
    Left x -> error $ show x
    -- If the command shifts to horizontal mode, run '\indent', and re-read the
    -- stream as if the commands just seen hadn't been read.
    -- (Note that we pass 'stream', not 'streamNext'.)
    Right P.EnterHMode ->
      addParagraphToPage state pages acc stream True
    Right P.End ->
      do
      let lastPage = B.Page $ reverse $ A.setListElems A.NaturallyGood acc
      return (state, lastPage:pages, acc, streamNext)
    Right (P.VAllModesCommand aCom) ->
      case aCom of
        P.Assign P.Assignment{body=P.DefineFont cs fPath} ->
          do
          let fNr = csToFontNr cs
          (stateNext, fontDef) <- defineFont state fPath fNr
          extractPages stateNext pages (A.VFontDefinition fontDef:acc) streamNext
        P.Assign P.Assignment{body=P.SelectFont fNr} ->
          do
          let (stateNext, fontSel) = selectFont state fNr
          extractPages stateNext pages (A.VFontSelection fontSel:acc) streamNext
        P.Relax ->
          extractPages state pages acc streamNext
        -- \par does nothing in vertical mode.
        P.EndParagraph ->
          extractPages state pages acc streamNext
        P.AddKern ln ->
          extractPages state pages ((A.VKern $ B.Kern $ evaluateLnToSp ln):acc) streamNext
        P.StartParagraph indent ->
          addParagraphToPage state pages acc streamNext indent
        -- <space token> has no effect in vertical modes.
        P.AddSpace ->
          extractPages state pages acc streamNext
