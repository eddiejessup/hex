{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Setting where

import Data.List.Index (imap)
import Data.Maybe (isJust, mapMaybe)
import Control.Applicative (liftA2)

import Box (Dimensioned, naturalWidth)
import qualified Unit
import qualified Box as B
import qualified Adjacent as A

import qualified Debug.Trace as T

tenK :: Int
tenK = 10000
hunK :: Int
hunK = 100000
oneMillion :: Int
oneMillion = 1000000

data GlueFlex = GlueFlex {factor :: Int, order :: Int}

noFlex :: GlueFlex
noFlex = GlueFlex 0 0

finiteFlex :: Int -> GlueFlex
finiteFlex f = GlueFlex f 0

filFlex :: GlueFlex
filFlex = GlueFlex 1 1

filGlue :: Glue
filGlue = Glue{dimen=0, stretch=filFlex, shrink=noFlex}

hFilGlue :: BreakableHListElem
hFilGlue = HGlue filGlue

instance Show GlueFlex where
  show (GlueFlex 0 0) = "0"
  show (GlueFlex f 0) = Unit.showSP f
  show (GlueFlex f n) = show f ++ " fil" ++ show n

data ListFlex = ListFlex { stretch :: [Int], shrink :: [Int] } deriving Show

data BreakItem = GlueBreak Glue
                | KernBreak B.Kern
                | PenaltyBreak Penalty
                | NoBreak
                deriving Show

data Break a = Break { before :: [a],
                       after :: [a],
                       item :: BreakItem }
           deriving Show

-- Can't have these things in a box, only a list.
data Glue = Glue
  { dimen :: Int
  , stretch :: GlueFlex
  , shrink :: GlueFlex
  }

instance Show Glue where
  show (Glue d (GlueFlex 0 0) (GlueFlex 0 0)) = "{- " ++ Unit.showSP d ++ " -}"
  show (Glue d str shr) = "{" ++ Unit.showSP d ++ ("+" ++ show str) ++ ("-" ++ show shr) ++ "}"

newtype Penalty = Penalty
  { size :: Int
  }

instance Show Penalty where
  show (Penalty p) = "|p" ++ show p ++ "|"

-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

data BreakableHListElem
  = HVBox B.VBox
  | HHBox B.HBox
  | HRule B.Rule
  | HGlue Glue
  | HKern B.Kern
  | HPenalty Penalty
  | HFontDefinition B.FontDefinition
  | HFontSelection B.FontSelection
  | HCharacter B.Character

instance Show BreakableHListElem where
  show (HVBox e) = show e
  show (HHBox e) = show e
  show (HRule e) = show e
  show (HGlue e) = show e
  show (HKern e) = show e
  show (HPenalty e) = show e
  show (HFontDefinition e) = show e
  show (HFontSelection e) = show e
  show (HCharacter e) = show e

data BreakableVListElem
  = VVBox B.VBox
  | VHBox B.HBox
  | VRule B.Rule
  | VGlue Glue
  | VKern B.Kern
  | VPenalty Penalty
  | VFontDefinition B.FontDefinition
  | VFontSelection B.FontSelection
  deriving (Show)

data GlueRatio
  = FiniteGlueRatio { factor :: Double
                    , order :: Int }
  | InfiniteGlueRatio
  deriving (Show)

data ListStatus
  = NaturallyGood
  | TooFull GlueRatio
  | TooBare GlueRatio
  | OverFull
  deriving (Show)

data Line = Line
  { contents :: [BreakableHListElem]
  , breakItem :: BreakItem
  , status :: ListStatus
  } deriving (Show)

data Route = Route {lines :: [Line], demerit :: Int } deriving (Show)

instance Eq Route where
  (==) a b = demerit a == demerit b

instance Ord Route where
  compare a b = compare (demerit a) (demerit b)

data Direction = Horizontal | Vertical

breakPenalty :: BreakItem -> Int
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
breakPenalty NoBreak = 0
breakPenalty (PenaltyBreak (Penalty p)) = p

class BreakableListElem a where
  toGlue :: a -> Maybe Glue
  isDiscardable :: a -> Bool
  toBreakItem :: A.Adjacency a -> Maybe BreakItem

class SettableListElem a b where
  setElem :: ListStatus -> a -> [b]

class BreakableList a where
  naturalLength :: a -> Int

instance BreakableList [BreakableVListElem] where
  naturalLength = sum . fmap B.naturalHeight

instance BreakableList [BreakableHListElem] where
  naturalLength = sum . fmap B.naturalWidth

isGlue :: BreakableListElem a => a -> Bool
isGlue = isJust . toGlue

isBreakItem :: BreakableListElem a => A.Adjacency a -> Bool
isBreakItem = isJust . toBreakItem

allBreaksInner :: BreakableListElem a => [Break a] -> [a] -> [A.Adjacency a] -> [Break a]
allBreaksInner acc seen [] = Break{before=reverse seen, after=[], item=NoBreak}:acc
allBreaksInner acc seen ((thisAdj@(A.Adjacency (_, this, _))):rest) =
  let
    -- Bit pretentious: fAnd maps two functions 'f' and 'g' into one
    -- function whose result is 'f x && g x'.
    fAnd = liftA2 (&&)
    discardAfterBreak = fAnd (isDiscardable . A.fromAdjacent) (not . isBreakItem)
    trimAfterBreak = A.fromAdjacents . dropWhile discardAfterBreak
    newBreaksAccum = case toBreakItem thisAdj of
      Nothing -> acc
      Just b ->
        let bef = if isGlue this then seen else this:seen
        in Break{before=reverse bef, after=trimAfterBreak rest, item=b}:acc
  in
    allBreaksInner newBreaksAccum (this:seen) rest

allBreaks :: BreakableListElem a => [a] -> [Break a]
allBreaks = allBreaksInner [] [] . A.toAdjacents

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

flex :: (BreakableListElem a) => [a] -> ListFlex
flex = listFlex . (mapMaybe toGlue)

instance BreakableListElem BreakableVListElem where
  toGlue (VGlue g) = Just g
  toGlue _ = Nothing

  isDiscardable (VGlue _) = True
  isDiscardable (VKern _) = True
  isDiscardable (VPenalty _) = True
  isDiscardable _ = False

  toBreakItem (A.Adjacency a) = case a of
    (Just x, VGlue g, _) -> if isDiscardable x then Nothing else Just $ GlueBreak g
    (_, VKern k, Just VGlue{}) -> Just $ KernBreak k
    (_, VPenalty p, _) -> Just $ PenaltyBreak p
    _ -> Nothing

instance SettableListElem BreakableHListElem B.HBoxElem where
  setElem ls (HGlue g) = [B.HGlue $ setGlue ls g]
  setElem _ (HPenalty _) = []
  setElem _ (HVBox v) = [B.HVBox v]
  setElem _ (HHBox h) = [B.HHBox h]
  setElem _ (HRule a) = [B.HRule a]
  setElem _ (HKern a) = [B.HKern a]
  setElem _ (HFontDefinition a) = [B.HFontDefinition a]
  setElem _ (HFontSelection a) = [B.HFontSelection a]
  setElem _ (HCharacter a) = [B.HCharacter a]

instance SettableListElem BreakableVListElem B.VBoxElem where
  setElem ls (VGlue g) = [B.VGlue $ setGlue ls g]
  setElem _ (VPenalty _) = []
  setElem _ (VVBox v) = [B.VVBox v]
  setElem _ (VHBox h) = [B.VHBox h]
  setElem _ (VRule a) = [B.VRule a]
  setElem _ (VKern a) = [B.VKern a]
  setElem _ (VFontDefinition a) = [B.VFontDefinition a]
  setElem _ (VFontSelection a) = [B.VFontSelection a]

instance BreakableListElem BreakableHListElem where
  toGlue (HGlue g) = Just g
  toGlue _ = Nothing

  isDiscardable (HGlue _) = True
  isDiscardable (HKern _) = True
  isDiscardable (HPenalty _) = True
  isDiscardable _ = False

    -- TODO: Add math formula conditions.
  toBreakItem (A.Adjacency a) = case a of
    (Just x, HGlue g, _) -> if isDiscardable x then Nothing else Just $ GlueBreak g
    (_, HKern k, Just (HGlue _)) -> Just $ KernBreak k
    (_, HPenalty p, _) -> Just $ PenaltyBreak p
    -- TODO: Discretionary break and Math-off.
    _ -> Nothing

instance Dimensioned BreakableHListElem where
  naturalWidth (HVBox v) = B.naturalWidth v
  naturalWidth (HHBox h) = B.naturalWidth h
  naturalWidth (HRule r) = B.naturalWidth r
  naturalWidth (HGlue g) = dimen g
  naturalWidth (HKern k) = B.kernDimen k
  naturalWidth (HPenalty _) = 0
  naturalWidth (HFontDefinition _) = 0
  naturalWidth (HFontSelection _) = 0
  naturalWidth (HCharacter c) = naturalWidth c

  naturalHeight (HVBox v) = B.naturalHeight v
  naturalHeight (HHBox h) = B.naturalHeight h
  naturalHeight (HRule r) = B.naturalHeight r
  naturalHeight (HGlue _) = 0
  naturalHeight (HKern _) = 0
  naturalHeight (HPenalty _) = 0
  naturalHeight (HFontDefinition _) = 0
  naturalHeight (HFontSelection _) = 0
  naturalHeight (HCharacter c) = B.naturalHeight c

instance Dimensioned BreakableVListElem where
  naturalWidth (VVBox v) = B.naturalWidth v
  naturalWidth (VHBox h) = B.naturalWidth h
  naturalWidth (VRule r) = B.naturalWidth r
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VPenalty _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0

  naturalHeight (VVBox v) = B.naturalHeight v
  naturalHeight (VHBox h) = B.naturalHeight h
  naturalHeight (VRule r) = B.naturalHeight r
  naturalHeight (VGlue g) = dimen g
  naturalHeight (VKern k) = B.kernDimen k
  naturalHeight (VPenalty _) = 0
  naturalHeight (VFontDefinition _) = 0
  naturalHeight (VFontSelection _) = 0

glueSetRatio :: Int -> ListFlex -> ListStatus
glueSetRatio excessLength ListFlex{stretch=stretches, shrink=shrinks} =
  let
    regime = compare excessLength 0
    flexes = if regime == GT then shrinks else stretches
    _flex = last flexes
    _order = length flexes - 1
    flexIsFinite = _order == 0
    gRatio = fromIntegral excessLength / fromIntegral _flex
  in case regime of
    EQ -> NaturallyGood
    LT -> case stretches of
      [] -> TooBare InfiniteGlueRatio
      _ -> TooBare FiniteGlueRatio{factor= -gRatio, order=_order}
    GT -> case shrinks of
      [] -> TooFull InfiniteGlueRatio
      _ -> if flexIsFinite && excessLength > _flex
        then OverFull
        else
          let gRatioShrink = if flexIsFinite then min gRatio 1.0 else gRatio
          in TooFull FiniteGlueRatio{factor=gRatioShrink, order=_order}

listStatusBadness :: ListStatus -> Int
listStatusBadness g = case g of
    -- TODO: This should actually be infinity, not 1 million.
    OverFull -> oneMillion
    NaturallyGood -> 0
    TooFull FiniteGlueRatio{factor=r, order=_order} -> eq r
    TooBare FiniteGlueRatio{factor=r, order=_order} -> eq r
    TooFull InfiniteGlueRatio -> tenK
    TooBare InfiniteGlueRatio -> tenK
  where eq r = min tenK $ round $ (r^(3 :: Int)) * 100

listGlueSetRatio :: (BreakableList [a], BreakableListElem a) => Int -> [a] -> ListStatus
listGlueSetRatio desiredLength cs =
  glueSetRatio (naturalLength cs - desiredLength) (flex cs)
  -- T.trace ("nat: " ++ show (naturalLength cs) ++ " des: " ++ show desiredLength ++ "glues: " ++ show (mapMaybe toGlue cs) ++ " flex: " ++ show (flex cs) ++ "ratio: " ++ show (glueSetRatio (naturalLength cs - desiredLength) (flex cs)) ++ "\n") glueSetRatio (naturalLength cs - desiredLength) (flex cs)

isConsiderableAsLine :: Int -> (Line, [BreakableHListElem], Int) -> Bool
isConsiderableAsLine tolerance (Line{breakItem=br}, _, bad) =
  breakPenalty br < tenK && bad <= tolerance

lineDemerit :: Int -> Int -> BreakItem -> Int
lineDemerit linePenalty bad br =
  let
    breakDemerit = (breakPenalty br)^(2 :: Int)
    listDemerit = (linePenalty + bad)^(2 :: Int)
  in
    breakDemerit + listDemerit

breakToLine :: Int -> Break BreakableHListElem -> (Line, [BreakableHListElem])
breakToLine desiredWidth Break{before=bef, item=br, after=aft} =
  let
    stat = listGlueSetRatio desiredWidth bef
  in (Line{contents=bef, status=stat, breakItem=br}, aft)

bestRoute :: Int -> Int -> Int -> [BreakableHListElem] -> Route
bestRoute _ _ _ [] = Route {lines=[], demerit=0}
bestRoute desiredWidth tolerance linePenalty cs =
  let
    breaks = allBreaks cs
    linedBreaks = fmap (breakToLine desiredWidth) breaks
    linedBaddedBreaks = fmap (\(ln, aft) -> (ln, aft, listStatusBadness $ status ln)) linedBreaks
    -- linedBaddedBreaks = fmap (\(ln, aft) -> (ln, aft, T.traceShow (listStatusBadness $ status ln, status ln) listStatusBadness $ status ln)) linedBreaks
    goodLinedBaddedBreaks = filter (isConsiderableAsLine tolerance) linedBaddedBreaks
    addDemerit (ln@Line{breakItem=br}, aft, bad) = (ln, aft, lineDemerit linePenalty bad br)
    goodLinedDemeredBreaks = fmap addDemerit goodLinedBaddedBreaks

    subBestRoute (ln, aft, thisDemerit) =
      let
        Route{lines=subLines, demerit=subDemerit} = bestRoute desiredWidth tolerance linePenalty aft
      in
        Route{lines=ln:subLines, demerit=subDemerit + thisDemerit}
  in case goodLinedBaddedBreaks of
    [] -> Route{lines=[Line{contents=cs, status=OverFull, breakItem=NoBreak}], demerit= -1}
    _ -> minimum $ fmap subBestRoute goodLinedDemeredBreaks

setListElems :: SettableListElem a b => ListStatus -> [a] -> [b]
setListElems stat = concatMap (setElem stat)

-- Note: contents are expected to be in reverse order.
setParagraph :: Int -> Int -> Int -> [BreakableHListElem] -> [BreakableVListElem]
setParagraph _ _ _ [] = []
setParagraph desiredWidth tolerance linePenalty cs@(end:rest) =
  let
    -- Remove the final item if it's glue.
    csTrimmed = case end of
      HGlue _ -> rest
      _ -> cs
    -- Append \penalty10k \hfil \penalty-10k.
    -- Add extra bits to finish the list.
    csFinished = (HPenalty $ Penalty $ -tenK):hFilGlue:(HPenalty $ Penalty tenK):csTrimmed

    Route{lines=lns} = bestRoute desiredWidth tolerance linePenalty $ reverse csFinished
    setLine Line{contents=lncs, status=stat} = VHBox B.HBox{contents=setListElems stat lncs, desiredLength=B.To desiredWidth}
  in
    fmap setLine lns
    -- fmap setLine $ T.traceShow (lns !! 0) lns

factMod :: Int -> Int -> Double -> Int -> Int
factMod setOrder glueOrder r f = if setOrder == glueOrder then round (r * fromIntegral f) else 0

glueDiff :: ListStatus -> Glue -> Int
glueDiff NaturallyGood _ = 0
-- Note: I made this logic up.
-- Note: Not checking for order or stretch/shrink direction here yet, broken.
glueDiff OverFull Glue{shrink=GlueFlex{factor=shr}} = -shr
-- No flexibility implies no flex. Not that there should be anything to flex!
glueDiff (TooFull InfiniteGlueRatio) _ = 0
glueDiff (TooBare InfiniteGlueRatio) _ = 0
glueDiff (TooFull FiniteGlueRatio{factor=r, order=setOrder}) Glue{shrink=GlueFlex{factor=f, order=glueOrder}} = -(factMod setOrder glueOrder r f)
glueDiff (TooBare FiniteGlueRatio{factor=r, order=setOrder}) Glue{stretch=GlueFlex{factor=f, order=glueOrder}} = factMod setOrder glueOrder r f

setGlue :: ListStatus -> Glue -> B.SetGlue
setGlue ls g@Glue{dimen=d} = B.SetGlue $ d + glueDiff ls g








-- Page breaking.

pageCost :: Int -> Int -> Int -> Int
-- Break penalty, badness, 'insert penalties'.
pageCost p b q
  | (not infiniteB) && vNegP && normalQ = p
  | normalParams && b < tenK = b + p + q
  | normalParams && b == tenK = hunK
  | (infiniteB || (not normalQ)) && (not vPosP) = oneMillion
  where
    normalQ = q < tenK
    infiniteB = b == oneMillion
    vNegP = p <= -tenK
    vPosP = p >= tenK
    normalParams = (not vNegP) && (not vPosP) && normalQ
