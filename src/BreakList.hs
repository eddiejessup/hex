{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BreakList where

import Data.Maybe (isJust, mapMaybe)
import qualified Data.Char as C

import qualified Box as B
import Box (Dimensioned, naturalWidth)
import qualified Unit
import qualified Adjacent as A

tenK :: Int
tenK = 10000
hunK :: Int
hunK = 100000

class BreakableListElem a where
  toGlue :: a -> Maybe Glue
  isDiscardable :: a -> Bool
  isBox :: a -> Bool
  toBreakItem :: A.Adjacency a -> Maybe BreakItem
  naturalLength :: a -> Int
  naturalListLength :: [a] -> Int
  naturalListLength = sum . fmap naturalLength

  isGlue :: BreakableListElem a => a -> Bool
  isGlue = isJust . toGlue
  isBreakItem :: BreakableListElem a => A.Adjacency a -> Bool
  isBreakItem = isJust . toBreakItem

class SettableListElem a b where
  setElem :: ListStatus -> a -> [b]

data GlueFlex = GlueFlex {factor :: Rational, order :: Int}

instance Show GlueFlex where
  show (GlueFlex 0 0) = "0"
  show (GlueFlex f 0) = Unit.showSP f
  show (GlueFlex f n) = show f ++ " fil" ++ show n

data Glue = Glue
  { dimen :: Int
  , stretch :: GlueFlex
  , shrink :: GlueFlex
  }

instance Show Glue where
  show (Glue d (GlueFlex 0 0) (GlueFlex 0 0)) = "{- " ++ Unit.showSP d ++ " -}"
  show (Glue d str shr) = "{" ++ Unit.showSP d ++ ("+" ++ show str) ++ ("-" ++ show shr) ++ "}"

newtype Penalty = Penalty Int

instance Show Penalty where
  show (Penalty p) = "|p" ++ show p ++ "|"

-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

data BreakableHListElem
  = HListBox B.Box
  | HRule B.Rule
  | HGlue Glue
  | HKern B.Kern
  | HPenalty Penalty
  | HFontDefinition B.FontDefinition
  | HFontSelection B.FontSelection
  | HCharacter B.Character

-- Just for the purposes of showing the list more compactly.
data CondensedHListElem
  = Sentence String
  | NonSentence BreakableHListElem

instance Show CondensedHListElem where
  show (Sentence s) = s
  show (NonSentence x) = show x

instance Show BreakableHListElem where
  show (HListBox e) = show e
  show (HRule e) = show e
  show (HGlue e) = show e
  show (HKern e) = show e
  show (HPenalty e) = show e
  show (HFontDefinition e) = show e
  show (HFontSelection e) = show e
  show (HCharacter e) = show e

  showList a = (show (foldr append [] a) ++)
    where
      append (HCharacter B.Character{code=n}) [] = [Sentence [C.chr n]]
      append x [] = [NonSentence x]
      append y r@(x:xs)
        | HCharacter B.Character{code=n} <- y, (Sentence cs) <- x = Sentence (C.chr n:cs):xs
        | HCharacter B.Character{code=n} <- y = Sentence [C.chr n]:r
        | otherwise = NonSentence y:r

data BreakableVListElem
  = VListBox B.Box
  | VRule B.Rule
  | VGlue Glue
  | VKern B.Kern
  | VPenalty Penalty
  | VFontDefinition B.FontDefinition
  | VFontSelection B.FontSelection

instance Show BreakableVListElem where
  show (VListBox e) = show e
  show (VRule e) = show e
  show (VGlue e) = show e
  show (VKern e) = show e
  show (VPenalty e) = show e
  show (VFontDefinition e) = show e
  show (VFontSelection e) = show e

instance Dimensioned BreakableHListElem where
  naturalWidth (HListBox b) = B.naturalWidth b
  naturalWidth (HRule r) = B.naturalWidth r
  naturalWidth (HGlue g) = dimen g
  naturalWidth (HKern k) = B.kernDimen k
  naturalWidth (HPenalty _) = 0
  naturalWidth (HFontDefinition _) = 0
  naturalWidth (HFontSelection _) = 0
  naturalWidth (HCharacter c) = naturalWidth c

  naturalHeight (HListBox b) = B.naturalHeight b
  naturalHeight (HRule r) = B.naturalHeight r
  naturalHeight (HGlue _) = 0
  naturalHeight (HKern _) = 0
  naturalHeight (HPenalty _) = 0
  naturalHeight (HFontDefinition _) = 0
  naturalHeight (HFontSelection _) = 0
  naturalHeight (HCharacter c) = B.naturalHeight c

  naturalDepth (HListBox b) = B.naturalDepth b
  naturalDepth (HRule r) = B.naturalDepth r
  naturalDepth (HGlue _) = 0
  naturalDepth (HKern _) = 0
  naturalDepth (HPenalty _) = 0
  naturalDepth (HFontDefinition _) = 0
  naturalDepth (HFontSelection _) = 0
  naturalDepth (HCharacter c) = B.naturalDepth c

instance Dimensioned BreakableVListElem where
  naturalWidth (VListBox b) = B.naturalWidth b
  naturalWidth (VRule r) = B.naturalWidth r
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VPenalty _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0

  naturalHeight (VListBox b) = B.naturalHeight b
  naturalHeight (VRule r) = B.naturalHeight r
  naturalHeight (VGlue g) = dimen g
  naturalHeight (VKern k) = B.kernDimen k
  naturalHeight (VPenalty _) = 0
  naturalHeight (VFontDefinition _) = 0
  naturalHeight (VFontSelection _) = 0

  naturalDepth (VListBox b) = B.naturalDepth b
  naturalDepth (VRule r) = B.naturalDepth r
  naturalDepth (VGlue _) = 0
  naturalDepth (VKern _) = 0
  naturalDepth (VPenalty _) = 0
  naturalDepth (VFontDefinition _) = 0
  naturalDepth (VFontSelection _) = 0

instance BreakableListElem BreakableHListElem where
  toGlue (HGlue g) = Just g
  toGlue _ = Nothing

  isDiscardable (HGlue _) = True
  isDiscardable (HKern _) = True
  isDiscardable (HPenalty _) = True
  isDiscardable _ = False

  isBox (HListBox _) = True
  isBox (HRule _) = True
  isBox (HCharacter _) = True
  isBox _ = False

    -- TODO: Add math formula conditions.
  toBreakItem (A.Adjacency a) = case a of
    (Just x, HGlue g, _) -> if isDiscardable x then Nothing else Just $ GlueBreak g
    (_, HKern k, Just (HGlue _)) -> Just $ KernBreak k
    (_, HPenalty p, _) -> Just $ PenaltyBreak p
    -- TODO: Discretionary break and Math-off.
    _ -> Nothing

  naturalLength = B.naturalWidth

instance BreakableListElem BreakableVListElem where
  toGlue (VGlue g) = Just g
  toGlue _ = Nothing

  isDiscardable (VGlue _) = True
  isDiscardable (VKern _) = True
  isDiscardable (VPenalty _) = True
  isDiscardable _ = False

  isBox (VListBox _) = True
  isBox (VRule _) = True
  isBox _ = False

  toBreakItem (A.Adjacency a) = case a of
    (Just x, VGlue g, _) -> if isDiscardable x then Nothing else Just $ GlueBreak g
    (_, VKern k, Just VGlue{}) -> Just $ KernBreak k
    (_, VPenalty p, _) -> Just $ PenaltyBreak p
    _ -> Nothing

  naturalLength = B.naturalHeight

instance SettableListElem BreakableHListElem B.HBoxElem where
  setElem ls (HGlue g) = [B.HGlue $ setGlue ls g]
  setElem _ (HPenalty _) = []
  setElem _ (HListBox b) = [B.HChild b]
  setElem _ (HRule a) = [B.HRule a]
  setElem _ (HKern a) = [B.HKern a]
  setElem _ (HFontDefinition a) = [B.HFontDefinition a]
  setElem _ (HFontSelection a) = [B.HFontSelection a]
  setElem _ (HCharacter a) = [B.HCharacter a]

instance SettableListElem BreakableVListElem B.VBoxElem where
  setElem ls (VGlue g) = [B.VGlue $ setGlue ls g]
  setElem _ (VPenalty _) = []
  setElem _ (VListBox b) = [B.VChild b]
  setElem _ (VRule a) = [B.VRule a]
  setElem _ (VKern a) = [B.VKern a]
  setElem _ (VFontDefinition a) = [B.VFontDefinition a]
  setElem _ (VFontSelection a) = [B.VFontSelection a]

noFlex :: GlueFlex
noFlex = GlueFlex 0 0

finiteFlex :: Rational -> GlueFlex
finiteFlex f = GlueFlex f 0

filFlex :: GlueFlex
filFlex = GlueFlex 1 1

filGlue :: Glue
filGlue = Glue{dimen=0, stretch=filFlex, shrink=noFlex}

hFilGlue :: BreakableHListElem
hFilGlue = HGlue filGlue

-- Judgment.

data Fixable
  -- In principle we could use a GlueFlex instead of this almost identical
  -- record. But GlueFlex stores scaled points (order 2^16), while this stores
  -- ratios (order 1). At the very least, their show instances should look
  -- different.
  = Fixable { ratio :: Rational, order :: Int }
  | Unfixable

instance Show Fixable where
  show Fixable{ratio=r, order=0} = "Fixable, finite scale: " ++ Unit.showFrac r
  show Fixable{ratio=r, order=1} = "Fixable, fil ratio: " ++ Unit.showSP r
  show Fixable{ratio=r, order=n} = "Fixable, fil order " ++ show n ++ " ratio: " ++ Unit.showSP r
  show Unfixable = "Unfixable"

data LengthJudgment = Full | Bare
  deriving (Show)

data ListStatus
  = NaturallyGood
  | NaturallyBad LengthJudgment Fixable
  deriving (Show)

data BreakItem = GlueBreak Glue
               | KernBreak B.Kern
               | PenaltyBreak Penalty
               | NoBreak
               deriving Show

breakPenalty :: BreakItem -> Int
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
breakPenalty NoBreak = 0
breakPenalty (PenaltyBreak (Penalty p)) = p

addGlueFlex :: GlueFlex -> GlueFlex -> GlueFlex
addGlueFlex g1@GlueFlex{factor=f1, order=o1} g2@GlueFlex{factor=f2, order=o2}
  = case compare o1 o2 of
    GT -> g1
    LT -> g2
    EQ -> g2{factor=f1 + f2}

flex :: (BreakableListElem a) => [a] -> (GlueFlex, GlueFlex)
flex = listFlex . mapMaybe toGlue
  where
    listFlex :: [Glue] -> (GlueFlex, GlueFlex)
    listFlex gs =
      let
        (strs, shrs) = unzip [ (str, shr) | Glue{stretch=str, shrink=shr} <- gs]
        sumGlueFlexes = foldr addGlueFlex GlueFlex{factor=0, order=0}
      in (sumGlueFlexes strs, sumGlueFlexes shrs)

-- Suppose the line order is i, and the glue has natural width u, and
-- flexibility f_j, corresponding to its amount and order of stretch or shrink
-- as appropriate.
-- The glue's width is u, plus: rf if j = i; otherwise 0.
glueDiff :: ListStatus -> Glue -> Int
glueDiff NaturallyGood _ = 0
-- Note: I made this logic up. Take a 'do your best' approach.
glueDiff (NaturallyBad Full Unfixable) Glue{shrink=GlueFlex{factor=f}} = -(round f)
glueDiff (NaturallyBad Bare Unfixable) Glue{stretch=GlueFlex{factor=f}} = round f
glueDiff (NaturallyBad a Fixable{ratio=r, order=setOrder}) Glue{shrink=shr, stretch=str}
  | Full <- a = -(scaleFactor shr)
  | Bare <- a = scaleFactor str
  where
    scaleFactor GlueFlex{factor=f, order=glueOrder} =
      if setOrder == glueOrder then round (r * f) else 0

setGlue :: ListStatus -> Glue -> B.SetGlue
setGlue ls g@Glue{dimen=d} = B.SetGlue $ d + glueDiff ls g

glueSetRatio :: Int -> (GlueFlex, GlueFlex) -> ListStatus
glueSetRatio excessLength (_stretch, _shrink) =
  let
    regime = compare excessLength 0
    GlueFlex{factor=flexFactor, order=_order} = if regime == GT then _shrink else _stretch
    flexIsFinite = _order == 0
    -- The glue ratio is r = [excess length]/[flex]i.
    gRatio = fromIntegral excessLength / flexFactor
  -- The natural width x is compared to the desired width w.
  in case regime of
    -- If x = w, all glue gets its natural width.
    EQ -> NaturallyGood
    -- Otherwise the glue will be modified, by computing a “glue set ratio” r
    -- and a “glue set order” i
    LT -> case flexFactor of
      -- If y0 = y1 = y2 = y3 = 0, there’s no stretchability.
      0 -> NaturallyBad Bare Unfixable
      _ -> NaturallyBad Bare Fixable{ratio= -gRatio, order=_order}
    GT -> case flexFactor of
      0 -> NaturallyBad Full Unfixable
      _ ->
        if fromIntegral excessLength > flexFactor
        then NaturallyBad Full Unfixable
        else
          -- r is set to 1 if i = 0 and x − w > z0, because the maximum
          -- shrinkability must not be exceeded.
          let gRatioShrink = if flexIsFinite then min gRatio 1.0 else gRatio
          in NaturallyBad Full Fixable{ratio=gRatioShrink, order=_order}

-- TODO: Use types to ensure number is within bounds, such as <= tenK.
data Badness = FiniteBadness Int | InfiniteBadness

listStatusBadness :: ListStatus -> Badness
listStatusBadness NaturallyGood
  = FiniteBadness 0
listStatusBadness (NaturallyBad Full Unfixable)
  = InfiniteBadness
listStatusBadness (NaturallyBad Bare Unfixable)
  = FiniteBadness tenK
-- if i != 0, there is infinite stretchability or shrinkability, so the badness
-- is zero. Otherwise the badness is approximately min(100r3,10000).
listStatusBadness (NaturallyBad _ Fixable{ratio=r, order=0})
  = FiniteBadness $ min tenK $ round $ (r^(3 :: Int)) * 100
listStatusBadness (NaturallyBad _ Fixable{order=_})
  = FiniteBadness 0

listGlueSetRatio :: BreakableListElem a => Int -> [a] -> ListStatus
listGlueSetRatio desiredLength cs =
  glueSetRatio (naturalListLength cs - desiredLength) (flex cs)

setListElems :: SettableListElem a b => ListStatus -> [a] -> [b]
setListElems _status = concatMap (setElem _status)