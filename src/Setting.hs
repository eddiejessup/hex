{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Setting where

import Data.List (intersperse, minimumBy)
import Data.List.Index (imap)
import Data.Maybe (isNothing)

import Box (Dimensioned, naturalWidth)
import qualified Box as B

data GlueFlex = GlueFlex {factor :: Int, order :: Int} deriving Show

noGlueFlex :: GlueFlex
noGlueFlex = GlueFlex{factor=0, order=0}

finiteGlueFlex :: Int -> GlueFlex
finiteGlueFlex f = GlueFlex f 0

data ListFlex = ListFlex { stretch :: [Int], shrink :: [Int] } deriving Show

-- Can't have these things in a box, only a list.
data Glue = Glue
  { dimen :: Int
  , stretch :: GlueFlex
  , shrink :: GlueFlex
  } deriving (Show)

newtype Penalty = Penalty
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

data Adjacency a = Adjacency (Maybe a, a, Maybe a) deriving Show

toAdjacents :: Maybe a -> [a] -> [Adjacency a]
toAdjacents _ [] = []
toAdjacents _before [this] = [Adjacency (_before, this, Nothing)]
toAdjacents _before (this:_after:rest) = Adjacency (_before, this, Just _after):toAdjacents (Just this) (_after:rest)

fromAdjacent :: Adjacency a -> a
fromAdjacent (Adjacency (_, a, _)) = a

fromAdjacents :: [Adjacency a] -> [a]
fromAdjacents = fmap fromAdjacent

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
trimBeforeBreak _before b = case b of
  -- Include the break item in the pre-break list, unless it is glue.
  HGlue _ -> _before
  x -> _before ++ [x]

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

vGlues :: [VListElement] -> [Glue]
vGlues cs = [ g | (VGlue g) <- cs]

hGlues :: [HListElement] -> [Glue]
hGlues cs = [ g | (HGlue g) <- cs]

addGlueFlex :: GlueFlex -> [Int] -> [Int]
addGlueFlex GlueFlex{factor=f, order=_order} fs =
  let extraElems = length fs - (_order + 1)
      fsPad = if extraElems < 0 then fs else fs ++ replicate extraElems 0
  in imap (\i x -> if i == _order then x + f else x) fsPad

sumGlueFlexes :: [GlueFlex] -> [Int]
sumGlueFlexes = foldr addGlueFlex [0]

listFlex :: [Glue] -> ListFlex
listFlex gs =
  let (strs, shrs) = unzip [ (str, shr) | Glue{stretch=str, shrink=shr} <- gs]
  in ListFlex {stretch=sumGlueFlexes strs, shrink=sumGlueFlexes shrs}

class ElementList a where
  flex :: a -> ListFlex

instance ElementList [VListElement] where
  flex = listFlex . vGlues

instance ElementList [HListElement] where
  flex = listFlex . hGlues

instance Dimensioned [VListElement] where
  naturalWidth cs = maximum $ fmap naturalWidth cs

instance Dimensioned [HListElement] where
  naturalWidth cs = sum $ fmap naturalWidth cs

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

instance Dimensioned VListElement where
  naturalWidth (VVBox v) = naturalWidth v
  naturalWidth (VHBox h) = naturalWidth h
  naturalWidth (VRule B.Rule{width=w}) = w
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VPenalty _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0

glueSetRatio :: Int -> ListFlex -> LineStatus
glueSetRatio excessLength ListFlex{stretch=stretches, shrink=shrinks} =
  case compare excessLength 0 of
    EQ -> NaturallyGood
    LT -> TooBare $ case head stretches of
      0 -> InfiniteGlueRatio
      str -> FiniteGlueRatio $ -((fromIntegral excessLength) / (fromIntegral str))
    GT -> case head shrinks of
      0 -> TooFull InfiniteGlueRatio
      shr -> case compare excessLength shr of
        GT -> OverFull
        _ -> TooFull $ FiniteGlueRatio $ (fromIntegral excessLength) / (fromIntegral shr)

badness :: LineStatus -> Int
badness g = case g of
  -- TODO: This should actually be infinity, not 1 million.
  OverFull -> 1000000
  NaturallyGood -> 0
  TooFull (FiniteGlueRatio r) -> min 10000 $ round $ (r^(3 :: Int)) * 100
  TooBare (FiniteGlueRatio r) -> min 10000 $ round $ (r^(3 :: Int)) * 100
  TooFull InfiniteGlueRatio -> 10000
  TooBare InfiniteGlueRatio -> 10000

data GlueRatio = FiniteGlueRatio Double | InfiniteGlueRatio deriving Show

data LineStatus = NaturallyGood | TooFull GlueRatio | TooBare GlueRatio | OverFull deriving Show

lineGlueSetRatio :: Int -> [HListElement] -> LineStatus
lineGlueSetRatio desiredWidth cs = glueSetRatio (naturalWidth cs - desiredWidth) (flex cs)

lineBadness :: Int -> [HListElement] -> Int
lineBadness desiredWidth cs = badness $ lineGlueSetRatio desiredWidth cs

isConsiderableAsLine :: Int -> Int -> Break -> Bool
isConsiderableAsLine tolerance desiredWidth Break{before=bef, item=br} = (breakPenalty br < 10000) && (lineBadness desiredWidth bef <= tolerance)

data Line = Line { contents :: [HListElement],
                   breakItem :: BreakItem }
           deriving Show

lineDemerit :: Int -> Int -> Line -> Int
lineDemerit linePenalty desiredWidth Line{contents=cs, breakItem=br} = (breakPenalty br)^(2 :: Int) + (linePenalty + (lineBadness desiredWidth cs))^(2 :: Int)

bestRoute :: Int -> Int -> Int -> [HListElement] -> [Line]
bestRoute _ _ _ [] = []
bestRoute desiredWidth tolerance linePenalty cs =
  let
    csTrimmed = case last cs of
      HGlue _ -> init cs
      _ -> cs

    -- finisher = []
    finisher = [HPenalty $ Penalty 10000, HGlue Glue{dimen=0, stretch=GlueFlex{factor=10000000, order=0}, shrink=noGlueFlex}, HPenalty $ Penalty $ -10000]
    csFinished = csTrimmed ++ finisher
    breaks = allBreaks csFinished
    -- badnesses = fmap (\Break{before=bef, after=aft, item=br} -> lineBadness desiredWidth bef) breaks
    -- ratios = fmap (\Break{before=bef, after=aft, item=br} -> lineGlueSetRatio desiredWidth bef) breaks
    -- considerables = fmap (isConsiderableAsLine tolerance desiredWidth) breaks
    -- mmm = zip3 badnesses ratios considerables

    -- considerableBreaks = T.traceShow mmm $ filter (isConsiderableAsLine tolerance desiredWidth) breaks
    considerableBreaks = filter (isConsiderableAsLine tolerance desiredWidth) breaks
    subBestRoute Break{before=bef, after=aft, item=br} = Line{contents=bef, breakItem=br}:bestRoute desiredWidth tolerance linePenalty aft
    bestConsiderableRoutes = fmap subBestRoute considerableBreaks

    routeDemerit = sum . fmap (lineDemerit linePenalty desiredWidth)
    compareRoutes a b = compare (routeDemerit a) (routeDemerit b)

    best = minimumBy compareRoutes $ [Line{contents=cs, breakItem=NoBreak}]:bestConsiderableRoutes
  in best

superRoute :: Int -> [HListElement] -> [VListElement]
superRoute _ [] = []
superRoute desiredWidth cs =
  let breaks = allBreaks cs
      breakRoutes = fmap (\Break{before=bef, after=aft} -> [simpleHSet desiredWidth bef, VGlue Glue{dimen=400000, stretch=noGlueFlex, shrink=noGlueFlex}, simpleHSet desiredWidth aft]) breaks
      spacedRoutes = intersperse [VGlue Glue {dimen = 1600000, stretch=noGlueFlex, shrink=noGlueFlex}] breakRoutes
  in concat spacedRoutes

glueDiff :: LineStatus -> Glue -> Int
glueDiff NaturallyGood _ = 0
-- Note: I made this logic up.
glueDiff OverFull Glue{shrink=GlueFlex{factor=shr}} = -shr
glueDiff (TooFull InfiniteGlueRatio) Glue{shrink=GlueFlex{factor=shr}} = -shr
glueDiff (TooBare InfiniteGlueRatio) Glue{stretch=GlueFlex{factor=str}} = str
glueDiff (TooFull (FiniteGlueRatio r)) Glue{shrink=GlueFlex{factor=shr}} = round $ -(r * fromIntegral shr)
glueDiff (TooBare (FiniteGlueRatio r)) Glue{stretch=GlueFlex{factor=str}} = round $ r * fromIntegral str

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
  in VHBox B.HBox{contents=setContents, desiredLength=B.To desiredWidth}

setVList :: VListElement -> [B.VBoxElement]
setVList (VVBox v) = [B.VVBox v]
setVList (VHBox h) = [B.VHBox h]
setVList (VGlue Glue {dimen = d}) = [B.VGlue $ B.SetGlue d]
setVList (VFontDefinition a) = [B.VFontDefinition a]
setVList (VFontSelection a) = [B.VFontSelection a]

simpleVSet :: [VListElement] -> [B.VBoxElement]
simpleVSet = concatMap setVList
