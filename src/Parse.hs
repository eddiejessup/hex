{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse where

import qualified Data.Char as C
import Data.IntMap.Strict ((!?))
import Data.List (intersperse, minimumBy)
import Data.Maybe (isNothing)
import qualified Debug.Trace as T

import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified Cat
import qualified Lex
import Box (Dimensioned, stretchability, shrinkability, naturalWidth)
import qualified Box as B

-- Can't have these things in a box, only a list.
data Glue = Glue
  { dimen :: Int
  , stretch :: Int
  , shrink :: Int
  } deriving (Show)

data Penalty = Penalty
  { size :: Int
  } deriving (Show)
             -- TODO: WhatsIt, Leaders, Mark, Insertion
              -- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

data HListElement
  = HVBox B.VBox
  | HHBox B.HBox
  | HRule B.Rule
  | HGlue Glue
  | HKern B.Kern
  | HPenalty Penalty
  | HFontDefinition B.FontDefinition
  | HFontSelection B.FontSelection
  | HCharacter B.Character
  deriving (Show)

data VListElement
  = VVBox B.VBox
  | VHBox B.HBox
  | VRule B.Rule
  | VGlue Glue
  | VKern B.Kern
  | VPenalty Penalty
  | VFontDefinition B.FontDefinition
  | VFontSelection B.FontSelection
  deriving (Show)

isDiscardable :: HListElement -> Bool
isDiscardable a =
  case a of
    HGlue _ -> True
    HKern _ -> True
  -- HLeaders _ -> True
    HPenalty _ -> True
  -- HMathOn _ -> True
  -- HMathOff _ -> True
    _ -> False

glueDiff :: LineStatus -> Glue -> Int
glueDiff NaturallyGood _ = 0
-- Note: I made this logic up.
glueDiff OverFull Glue{shrink=shr} = -shr
glueDiff (TooFull InfiniteGlueRatio) Glue{shrink=shr} = -shr
glueDiff (TooBare InfiniteGlueRatio) Glue{stretch=str} = str
glueDiff (TooFull (FiniteGlueRatio r)) Glue{shrink=shr} = round $ -(r * fromIntegral shr)
glueDiff (TooBare (FiniteGlueRatio r)) Glue{stretch=str} = round $ r * fromIntegral str

setGlue :: LineStatus -> Glue -> B.SetGlue
setGlue ls g@Glue{dimen=d} = B.SetGlue $ d + glueDiff ls g

setHElem :: LineStatus -> HListElement -> [B.HBoxElement]
setHElem ls (HGlue g) = [B.HGlue $ setGlue ls g]
setHElem _ (HPenalty _) = []
setHElem _ (HVBox v) = [B.HVBox v]
setHElem _ (HHBox h) = [B.HHBox h]
setHElem _ (HRule a) = [B.HRule a]
setHElem _ (HKern a) = [B.HKern a]
setHElem _ (HFontDefinition a) = [B.HFontDefinition a]
setHElem _ (HFontSelection a) = [B.HFontSelection a]
setHElem _ (HCharacter a) = [B.HCharacter a]

simpleHSet :: Int -> [HListElement] -> VListElement
simpleHSet desiredWidth cs =
  let
    lineStatus = lineGlueSetRatio desiredWidth cs
    setContents = concatMap (setHElem lineStatus) cs
  in VHBox $ B.HBox{contents=setContents, desiredLength=B.To desiredWidth}

setVList :: VListElement -> [B.VBoxElement]
setVList (VVBox v) = [B.VVBox v]
setVList (VHBox h) = [B.VHBox h]
setVList (VGlue Glue {dimen = d}) = [B.VGlue $ B.SetGlue d]
setVList (VFontDefinition a) = [B.VFontDefinition a]
setVList (VFontSelection a) = [B.VFontSelection a]

simpleVSet :: [VListElement] -> [B.VBoxElement]
simpleVSet = concatMap setVList

data Adjacency a = Adjacency (Maybe a, a, Maybe a) deriving Show

toAdjacents :: Maybe a -> [a] -> [Adjacency a]
toAdjacents _ [] = []
toAdjacents before [this] = [Adjacency (before, this, Nothing)]
toAdjacents before (this:after:rest) = Adjacency (before, this, Just after):toAdjacents (Just this) (after:rest)

fromAdjacent :: Adjacency a -> a
fromAdjacent (Adjacency (_, a, _)) = a

fromAdjacents :: [Adjacency a] -> [a]
fromAdjacents = fmap fromAdjacent

data BreakItem = GlueBreak Glue
                | KernBreak B.Kern
                | PenaltyBreak Penalty
                | NoBreak
                deriving Show

data Break = Break { before :: [HListElement],
                     after :: [HListElement],
                     item :: BreakItem }
           deriving Show

toBreakItem :: Adjacency HListElement -> Maybe BreakItem
  -- TODO: Add math formula conditions.
toBreakItem (Adjacency a) = case a of
  (Just x, HGlue g, _) -> if isDiscardable x then Nothing else Just $ GlueBreak g
  (_, HKern k, Just HGlue{}) -> Just $ KernBreak k
  (_, HPenalty p, _) -> Just $ PenaltyBreak p
  -- TODO: Discretionary break and Math-off.
  _ -> Nothing

isBreakItem :: Adjacency HListElement -> Bool
isBreakItem = isNothing . toBreakItem

breakPenalty :: BreakItem -> Int
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
breakPenalty NoBreak = 0
breakPenalty (PenaltyBreak (Penalty p)) = p

trimBeforeBreak :: [HListElement] -> HListElement -> [HListElement]
trimBeforeBreak before b = case b of
  -- Include the break item in the pre-break list, unless it is glue.
  HGlue _ -> before
  x -> before ++ [x]

trimAfterBreak :: [Adjacency HListElement] -> [HListElement]
trimAfterBreak = fromAdjacents .
  dropWhile
    (\ adj ->
     isDiscardable (fromAdjacent adj) && not (isBreakItem adj))

allBreaksInner :: [Break] -> [HListElement] -> [Adjacency HListElement] -> [Break]
allBreaksInner breaksAccum seen [] = Break{before=seen, after=[], item=NoBreak}:breaksAccum
allBreaksInner breaksAccum seen ((thisAdj@(Adjacency (_, this, _))):rest) =
  let
    newBreaksAccum = case toBreakItem thisAdj of
      Nothing -> breaksAccum
      Just b -> Break{before=trimBeforeBreak seen this, after=trimAfterBreak rest, item=b}:breaksAccum
  in
    allBreaksInner newBreaksAccum (seen ++ [this]) rest

allBreaks :: [HListElement] -> [Break]
allBreaks cs = allBreaksInner [] [] (toAdjacents Nothing cs)

instance Dimensioned [VListElement] where
  naturalWidth cs = maximum $ fmap naturalWidth cs
  stretchability cs = sum $ fmap stretchability cs
  shrinkability cs = sum $ fmap shrinkability cs

instance Dimensioned [HListElement] where
  naturalWidth cs = sum $ fmap naturalWidth cs
  stretchability cs = sum $ fmap stretchability cs
  shrinkability cs = sum $ fmap shrinkability cs

instance Dimensioned HListElement where
  naturalWidth (HVBox v) = naturalWidth v
  naturalWidth (HHBox h) = naturalWidth h
  naturalWidth (HRule B.Rule{width=w}) = w
  naturalWidth (HGlue Glue{dimen=d}) = d
  naturalWidth (HKern B.Kern{dimen=d}) = d
  naturalWidth (HPenalty _) = 0
  naturalWidth (HFontDefinition _) = 0
  naturalWidth (HFontSelection _) = 0
  naturalWidth (HCharacter B.Character{width=w}) = w

  stretchability a = case a of
    (HGlue Glue{stretch=s}) -> s
    _ -> 0

  shrinkability a = case a of
    (HGlue Glue{shrink=s}) -> s
    _ -> 0

instance Dimensioned VListElement where
  naturalWidth (VVBox v) = naturalWidth v
  naturalWidth (VHBox h) = naturalWidth h
  naturalWidth (VRule B.Rule{width=w}) = w
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VPenalty _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0

  stretchability a = case a of
    (VGlue Glue{stretch=s}) -> s
    _ -> 0

  shrinkability a = case a of
    (VGlue Glue{shrink=s}) -> s
    _ -> 0

data GlueRatio = FiniteGlueRatio Double | InfiniteGlueRatio deriving Show
data LineStatus = NaturallyGood | TooFull GlueRatio | TooBare GlueRatio | OverFull deriving Show

glueSetRatio :: Int -> Int -> Int -> LineStatus
glueSetRatio excessLength stretch shrink =
  case compare excessLength 0 of
    EQ -> NaturallyGood
    LT -> TooBare $ case stretch of
      0 -> InfiniteGlueRatio
      str -> FiniteGlueRatio $ -((fromIntegral excessLength) / (fromIntegral str))
    GT -> case shrink of
      0 -> TooFull InfiniteGlueRatio
      shr -> case compare excessLength shr of
        GT -> OverFull
        _ -> TooFull $ FiniteGlueRatio $ (fromIntegral excessLength) / (fromIntegral shr)

badness :: LineStatus -> Int
badness g = case g of
  -- TODO: This should actually be infinity, not 1 million.
  OverFull -> 1000000
  NaturallyGood -> 0
  TooFull (FiniteGlueRatio r) -> min 10000 $ round $ (r^3) * 100
  TooBare (FiniteGlueRatio r) -> min 10000 $ round $ (r^3) * 100
  TooFull InfiniteGlueRatio -> 10000
  TooBare InfiniteGlueRatio -> 10000

lineGlueSetRatio :: Int -> [HListElement] -> LineStatus
lineGlueSetRatio desiredWidth cs = glueSetRatio (naturalWidth cs - desiredWidth) (stretchability cs) (shrinkability cs)

lineBadness :: Int -> [HListElement] -> Int
lineBadness desiredWidth cs = badness $ lineGlueSetRatio desiredWidth cs

isConsiderableAsLine :: Int -> Int -> Break -> Bool
isConsiderableAsLine tolerance desiredWidth Break{before=bef, item=br} = (breakPenalty br < 10000) && (lineBadness desiredWidth bef <= tolerance)

data Line = Line { contents :: [HListElement],
                   breakItem :: BreakItem }
           deriving Show

lineDemerit :: Int -> Int -> Line -> Int
lineDemerit linePenalty desiredWidth Line{contents=cs, breakItem=br} = (breakPenalty br)^2 + (linePenalty + (lineBadness desiredWidth cs))^2

bestRoute :: Int -> Int -> Int -> [HListElement] -> [Line]
bestRoute _ _ _ [] = []
bestRoute desiredWidth tolerance linePenalty cs =
  let
    csTrimmed = case last cs of
      HGlue _ -> init cs
      _ -> cs

    finisher = [HPenalty $ Penalty 10000, HGlue Glue{dimen=0, stretch=10000000, shrink=0}, HPenalty $ Penalty $ -10000]
    csFinished = cs ++ finisher
    breaks = allBreaks cs
    badnesses = fmap (\Break{before=bef, after=aft, item=br} -> lineBadness desiredWidth bef) breaks
    ratios = fmap (\Break{before=bef, after=aft, item=br} -> lineGlueSetRatio desiredWidth bef) breaks
    considerables = fmap (isConsiderableAsLine tolerance desiredWidth) breaks
    mmm = zip3 badnesses ratios considerables

    -- considerableBreaks = T.traceShow mmm $ filter (isConsiderableAsLine tolerance desiredWidth) breaks
    considerableBreaks = filter (isConsiderableAsLine tolerance desiredWidth) breaks
    subBestRoute Break{before=bef, after=aft, item=br} = Line{contents=bef, breakItem=br}:bestRoute desiredWidth tolerance linePenalty aft
    bestConsiderableRoutes = fmap subBestRoute considerableBreaks

    routeDemerit = sum . fmap (lineDemerit linePenalty desiredWidth)
    compareRoutes a b = compare (routeDemerit a) (routeDemerit b)

    best = minimumBy compareRoutes $ [Line{contents=cs, breakItem=NoBreak}]:bestConsiderableRoutes
  in best

superRoute :: Int -> Int -> Int -> [HListElement] -> [VListElement]
superRoute _ _ _ [] = []
superRoute desiredWidth tolerance linePenalty cs =
  let breaks = allBreaks cs
      breakRoutes = fmap (\Break{before=bef, after=aft, item=br} -> [simpleHSet desiredWidth bef, VGlue Glue{dimen=400000, stretch=0, shrink=0}, simpleHSet desiredWidth aft]) breaks
      spacedRoutes = intersperse [VGlue Glue {dimen = 1600000, stretch=0, shrink=0}] breakRoutes
  in concat spacedRoutes

data State = State { currentFontInfo :: Maybe TFMM.TexFont } deriving Show

character :: State -> Int -> Maybe B.Character
character state code = do
  font <- currentFontInfo state
  let toSP = TFMM.toScaledPoint font
  c <- TFMM.characters font !? code
  let (TFMC.Character{width=w, height=h, depth=d}) = c
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

extractHElems :: State -> [Lex.LexToken] -> IO (State, [HListElement], [Lex.LexToken])
extractHElems s [] = return (s, [], [])
extractHElems s (Lex.CharCat Cat.CharCat {char = char, cat = cat}:rest)
  | cat `elem` [Cat.Letter, Cat.Other] = do
    extra <- case character s $ C.ord char of
      Just c -> return [HCharacter c]
      Nothing -> fail "Could not get character info"
    (s, nextResult, nextRemain) <- extractHElems s rest
    return (s, extra ++ nextResult, nextRemain)
  | cat == Cat.Space = do
    extra <- case currentFontInfo s of
      (Just (font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr})) -> do
        let toSP = TFMM.toScaledPoint font
        return [HGlue Glue {dimen = toSP d, stretch = toSP str, shrink = toSP shr}]
      Nothing ->
        fail "No font selected"
    (s, nextResult, nextRemain) <- extractHElems s rest
    return (s, extra ++ nextResult, nextRemain)
  | otherwise = extractHElems s rest
extractHElems s (Lex.ControlSequenceCall {name = name}:rest)
  | name == "par" = return (s, [], rest)
  | otherwise = extractHElems s rest

extractVElems :: State -> [Lex.LexToken] -> IO (State, [VListElement], [Lex.LexToken])
extractVElems s [] = return (s, [], [])
extractVElems s (a@Lex.CharCat {}:rest) = do
  (s, hConts, hRemain) <- extractHElems s (a : rest)
  let 
    width = 28000000
    tolerance = 200
    linePenalty = 10
    bestRouteLists = contents <$> bestRoute width tolerance linePenalty hConts
    bestRouteBoxes = fmap (simpleHSet width) bestRouteLists
    route = intersperse (VGlue Glue {dimen = 400000, stretch=0, shrink=0}) bestRouteBoxes
    -- route = superRoute width tolerance linePenalty cs

    down = VGlue Glue {dimen = 1000000, stretch = 0, shrink = 0}
  (s, nextResult, vRemain) <- extractVElems s hRemain
  return (s, route ++ [down] ++ nextResult, vRemain)
extractVElems s (Lex.ControlSequenceCall {name = name}:rest)
  | name == "font" = do
    fontInfo <- TFMM.readTFM "cmr10.tfm"
    let fontDef =
          VFontDefinition
            B.FontDefinition
            { fontNr = 1
            , fontPath = "cmr10.tfm"
            , fontName = "cmr10"
            , fontInfo = fontInfo
            , scaleFactorRatio = 1.0
            }
        extra = [fontDef, fontSel]
        fontSel = VFontSelection B.FontSelection {fontNr = 1}
        sNew = s{currentFontInfo=Just fontInfo}
    (sNew, nextResult, nextRemain) <- extractVElems sNew rest
    return (sNew, extra ++ nextResult, nextRemain)
  | name == "par" = extractVElems s rest
  | otherwise = extractVElems s rest
