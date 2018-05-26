{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Setting where

import Data.List (intersperse, minimumBy, zip4)
import Data.List.Index (imap)
import Data.Maybe (isNothing, mapMaybe)
import qualified Debug.Trace as T
import Box (Dimensioned, naturalWidth)
import qualified Box as B
import Control.Applicative (liftA2)

tenK :: Int
tenK = 10000
oneMillion = 1000000

data GlueFlex = GlueFlex {factor :: Int, order :: Int} deriving Show

noGlueFlex :: GlueFlex
noGlueFlex = GlueFlex{factor=0, order=0}

finiteGlueFlex :: Int -> GlueFlex
finiteGlueFlex f = GlueFlex f 0

filFlex :: GlueFlex
filFlex = GlueFlex{factor=1, order=1}

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

filGlue :: Glue
filGlue = Glue{dimen=0, stretch=filFlex, shrink=noGlueFlex}

hFilGlue :: HListElement
hFilGlue = HGlue filGlue

newtype Adjacency a = Adjacency (Maybe a, a, Maybe a) deriving Show

toAdjacents :: Maybe a -> [a] -> [Adjacency a]
toAdjacents _ [] = []
toAdjacents _before [this] = [Adjacency (_before, this, Nothing)]
toAdjacents _before (this:_after:rest) = Adjacency (_before, this, Just _after):toAdjacents (Just this) (_after:rest)

fromAdjacent :: Adjacency a -> a
fromAdjacent (Adjacency (_, a, _)) = a

fromAdjacents :: [Adjacency a] -> [a]
fromAdjacents = fmap fromAdjacent

data BreakItem = GlueBreak Glue
                | KernBreak B.Kern
                | PenaltyBreak Penalty
                | NoBreak
                deriving Show

data Break a = Break { before :: [a],
                       after :: [a],
                       item :: BreakItem }
           deriving Show

breakPenalty :: BreakItem -> Int
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
breakPenalty NoBreak = 0
breakPenalty (PenaltyBreak (Penalty p)) = p

class ListElement a where
  toGlue :: a -> Maybe Glue
  isDiscardable :: a -> Bool
  toBreakItem :: Adjacency a -> Maybe BreakItem

isGlue :: ListElement a => a -> Bool
isGlue = isNothing . toGlue

isBreakItem :: ListElement a => Adjacency a -> Bool
isBreakItem = isNothing . toBreakItem

allBreaksInner :: ListElement a => [Break a] -> [a] -> [Adjacency a] -> [Break a]
allBreaksInner breaksAccum seen [] = Break{before=seen, after=[], item=NoBreak}:breaksAccum
allBreaksInner breaksAccum seen ((thisAdj@(Adjacency (_, this, _))):rest) =
  let
    -- Bit pretentious: fAnd maps two functions 'f' and 'g' into one
    -- function whose result is 'f x && g x'.
    fAnd = liftA2 (&&)
    discardAfterBreak = fAnd (isDiscardable . fromAdjacent) (not . isBreakItem)
    trimAfterBreak = fromAdjacents . dropWhile discardAfterBreak
    newBreaksAccum = case toBreakItem thisAdj of
      Nothing -> breaksAccum
      Just b -> Break{before=if isGlue this then seen else this:seen, after=trimAfterBreak rest, item=b}:breaksAccum
  in
    allBreaksInner newBreaksAccum (this:seen) rest

allBreaks :: ListElement a => [a] -> [Break a]
allBreaks cs =
  let
    res = allBreaksInner [] [] (toAdjacents Nothing cs)
  in fmap (\b@Break{before=bef} -> b{before=reverse bef}) res

addGlueFlex :: GlueFlex -> [Int] -> [Int]
addGlueFlex GlueFlex{factor=f, order=_order} fs =
  let extraElems = (_order + 1) - length fs
      fsPad = if extraElems < 0 then fs else fs ++ replicate extraElems 0
  in imap (\i x -> if i == _order then x + f else x) fsPad

listFlex :: [Glue] -> ListFlex
listFlex gs =
  let
    (strs, shrs) = unzip [ (str, shr) | Glue{stretch=str, shrink=shr} <- gs]
    sumGlueFlexes = foldr addGlueFlex []
  in ListFlex {stretch=sumGlueFlexes strs, shrink=sumGlueFlexes shrs}

flex :: (ListElement a) => [a] -> ListFlex
flex = listFlex . (mapMaybe toGlue)

instance ListElement VListElement where
  toGlue (VGlue g) = Just g
  toGlue _ = Nothing

  isDiscardable (VGlue _) = True
  isDiscardable (VKern _) = True
  isDiscardable (VPenalty _) = True
  isDiscardable _ = False

  toBreakItem (Adjacency a) = case a of
    (Just x, VGlue g, _) -> if isDiscardable x then Nothing else Just $ GlueBreak g
    (_, VKern k, Just VGlue{}) -> Just $ KernBreak k
    (_, VPenalty p, _) -> Just $ PenaltyBreak p
    _ -> Nothing

instance ListElement HListElement where
  toGlue (HGlue g) = Just g
  toGlue _ = Nothing

  isDiscardable (HGlue _) = True
  isDiscardable (HKern _) = True
  isDiscardable (HPenalty _) = True
  isDiscardable _ = False

    -- TODO: Add math formula conditions.
  toBreakItem (Adjacency a) = case a of
    (Just x, HGlue g, _) -> if isDiscardable x then Nothing else Just $ GlueBreak g
    (_, HKern k, Just (HGlue _)) -> Just $ KernBreak k
    (_, HPenalty p, _) -> Just $ PenaltyBreak p
    -- TODO: Discretionary break and Math-off.
    _ -> Nothing

contentWidths :: (ListElement a, Dimensioned a) => [a] -> [Int]
contentWidths = fmap naturalWidth

instance Dimensioned [VListElement] where
  naturalWidth = maximum . contentWidths

instance Dimensioned [HListElement] where
  naturalWidth = sum . contentWidths

instance Dimensioned HListElement where
  naturalWidth (HVBox v) = naturalWidth v
  naturalWidth (HHBox h) = naturalWidth h
  naturalWidth (HRule r) = naturalWidth r
  naturalWidth (HGlue g) = dimen g
  naturalWidth (HKern k) = B.kernDimen k
  naturalWidth (HPenalty _) = 0
  naturalWidth (HFontDefinition _) = 0
  naturalWidth (HFontSelection _) = 0
  naturalWidth (HCharacter B.Character{width=w}) = w

instance Dimensioned VListElement where
  naturalWidth (VVBox v) = naturalWidth v
  naturalWidth (VHBox h) = naturalWidth h
  naturalWidth (VRule r) = naturalWidth r
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VPenalty _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0

data GlueRatio = FiniteGlueRatio {factor :: Double, order :: Int} | InfiniteGlueRatio deriving Show

data LineStatus = NaturallyGood | TooFull GlueRatio | TooBare GlueRatio | OverFull deriving Show

glueSetRatio :: Int -> ListFlex -> LineStatus
glueSetRatio excessLength ListFlex{stretch=stretches, shrink=shrinks} =
  let
    regime = compare excessLength 0
    flexes = if regime == GT then shrinks else stretches
    _flex = last flexes
    _order = length flexes - 1
    finiteFlex = _order == 0
    gRatio = fromIntegral excessLength / fromIntegral _flex
  in case regime of
    EQ -> NaturallyGood
    LT -> case stretches of
      [] -> TooBare InfiniteGlueRatio
      _ -> TooBare FiniteGlueRatio{factor= -gRatio, order=_order}
    GT -> case shrinks of
      [] -> TooFull InfiniteGlueRatio
      _ -> if finiteFlex && excessLength > _flex
        then OverFull
        else
          let gRatioShrink = if finiteFlex then min gRatio 1.0 else gRatio
          in TooFull FiniteGlueRatio{factor=gRatioShrink, order=_order}

lineStatusBadness :: LineStatus -> Int
lineStatusBadness g = case g of
    -- TODO: This should actually be infinity, not 1 million.
    OverFull -> oneMillion
    NaturallyGood -> 0
    TooFull FiniteGlueRatio{factor=r, order=_order} -> eq r
    TooBare FiniteGlueRatio{factor=r, order=_order} -> eq r
    TooFull InfiniteGlueRatio -> tenK
    TooBare InfiniteGlueRatio -> tenK
  where eq r = min tenK $ round $ (r^(3 :: Int)) * 100

lineGlueSetRatio :: Int -> [HListElement] -> LineStatus
lineGlueSetRatio desiredWidth cs = glueSetRatio (naturalWidth cs - desiredWidth) (flex cs)

isConsiderableAsLine :: Int -> Int -> (Line, [HListElement], Int) -> Bool
isConsiderableAsLine tolerance desiredWidth (Line{breakItem=br}, aft, bad) =
  breakPenalty br < tenK && bad <= tolerance

data Line = Line {contents :: [HListElement], breakItem :: BreakItem, status :: LineStatus}

data Route = Route {lines :: [Line], demerit :: Int }

lineDemerit :: Int -> Int -> BreakItem -> Int
lineDemerit linePenalty bad br =
  let
    breakDemerit = (breakPenalty br)^(2 :: Int)
    lineDemerit = (linePenalty + bad)^(2 :: Int)
  in
    breakDemerit + lineDemerit

breakToLine :: Int -> Break HListElement -> (Line, [HListElement])
breakToLine desiredWidth Break{before=bef, item=br, after=aft} =
  let
    stat = lineGlueSetRatio desiredWidth bef
  in (Line{contents=bef, status=stat, breakItem=br}, aft)

bestRoute :: Int -> Int -> Int -> [HListElement] -> Route
bestRoute _ _ _ [] = Route {lines=[], demerit=0}
bestRoute desiredWidth tolerance linePenalty cs =
  let
    -- Add extra bits to finish the list.
    csTrimmed = case last cs of
      HGlue _ -> init cs
      _ -> cs
    suffix = [HPenalty $ Penalty tenK, hFilGlue, HPenalty $ Penalty $ -tenK]
    csFinished = csTrimmed ++ suffix

    breaks = allBreaks csFinished
    linedBreaks = fmap (breakToLine desiredWidth) breaks
    linedBaddedBreaks = fmap (\(ln, aft) -> (ln, aft, lineStatusBadness $ status ln)) linedBreaks
    goodLinedBaddedBreaks = filter (isConsiderableAsLine tolerance desiredWidth) linedBaddedBreaks
    addDemerit (ln@Line{breakItem=br}, aft, bad) = (ln, aft, lineDemerit linePenalty bad br)
    goodLinedDemeredBreaks = fmap addDemerit goodLinedBaddedBreaks

    subBestRoute (ln, aft, thisDemerit) =
      let
        Route{lines=subLines, demerit=subDemerit} = bestRoute desiredWidth tolerance linePenalty aft
      in
        Route{lines=ln:subLines, demerit=subDemerit + thisDemerit}

    bestGoodRoutes = fmap subBestRoute goodLinedDemeredBreaks
    -- TODO: Can we avoid this repetition using lift?
    compareRoutes a b = compare (demerit a) (demerit b)
  in case goodLinedBaddedBreaks of
    -- Should this be cs or csFinished?
    [] -> Route{lines=[Line{contents=cs, status=OverFull, breakItem=NoBreak}], demerit= -1}
    _ -> minimumBy compareRoutes bestGoodRoutes

setParagraph :: Int -> Int -> Int -> [HListElement] -> [VListElement]
setParagraph desiredWidth tolerance linePenalty cs =
  let
    Route{lines=lns} = bestRoute desiredWidth tolerance linePenalty cs
    setLine Line{contents=lncs, status=stat} =
      let
        setContents = concatMap (setHElem stat) lncs
      in VHBox B.HBox{contents=setContents, desiredLength=B.To desiredWidth}
  in
    fmap setLine lns

superRoute :: Int -> [HListElement] -> [VListElement]
superRoute _ [] = []
superRoute desiredWidth cs =
  let breaks = allBreaks cs
      breakRoutes = fmap (\Break{before=bef, after=aft} -> [simpleHSet desiredWidth bef, VGlue Glue{dimen=400000, stretch=noGlueFlex, shrink=noGlueFlex}, simpleHSet desiredWidth aft]) breaks
      spacedRoutes = intersperse [VGlue Glue {dimen = 1600000, stretch=noGlueFlex, shrink=noGlueFlex}] breakRoutes
  in concat spacedRoutes

factMod :: Int -> Int -> Double -> Int -> Int
factMod setOrder glueOrder r f = if setOrder == glueOrder then round (r * fromIntegral f) else 0

glueDiff :: LineStatus -> Glue -> Int
glueDiff NaturallyGood _ = 0
-- Note: I made this logic up.
-- Note: Not checking for order or stretch/shrink direction here yet, broken.
glueDiff OverFull Glue{shrink=GlueFlex{factor=shr}} = -shr
-- No flexibility implies no flex. Not that there should be anything to flex!
glueDiff (TooFull InfiniteGlueRatio) _ = 0
glueDiff (TooBare InfiniteGlueRatio) _ = 0
glueDiff (TooFull FiniteGlueRatio{factor=r, order=setOrder}) Glue{shrink=GlueFlex{factor=f, order=glueOrder}} = -(factMod setOrder glueOrder r f)
glueDiff (TooBare FiniteGlueRatio{factor=r, order=setOrder}) Glue{stretch=GlueFlex{factor=f, order=glueOrder}} = factMod setOrder glueOrder r f

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
