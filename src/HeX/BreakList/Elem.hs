module Hex.BreakList.Elem where

import Data.Adjacent
import Hex.Box
import Hex.BreakList.BreakList
  ( BreakItem (..)
  , BreakableListElem (..)
  , Penalty
  )
import Hex.BreakList.Glue
import Hex.Quantity
import Hexlude

class BreakableList a where

  naturalSpan :: a -> Length

  totalGlue :: a -> Glue Length

newtype HList = HList (Seq HListElem)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

instance Describe HList where

  describe (HList elems) =
    describeNamedRelFoldable 0 "HList" elems

instance Dimensioned HList where

  naturalLength dim (HList elemSeq) = case dim of
    BoxWidth -> sumLength BoxWidth elemSeq
    BoxHeight -> maxLength BoxHeight elemSeq
    BoxDepth -> maxLength BoxDepth elemSeq

subLengths :: Dimensioned a => BoxDim -> Seq a -> Seq Length
subLengths dim cs = naturalLength dim <$> cs

maxLength :: Dimensioned a => BoxDim -> Seq a -> Length
maxLength dim cs = max 0 $ maximumDef 0 (toList $ subLengths dim cs)

hPlusD :: Dimensioned a => a -> Length
hPlusD e = naturalLength BoxHeight e + naturalLength BoxDepth e

sumLength :: Dimensioned a => BoxDim -> Seq a -> Length
sumLength dim cs = sum (subLengths dim cs)

instance BreakableList HList where

  naturalSpan = naturalWidth

  totalGlue (HList elemSeq) = fold $ mapMaybe toGlue $ toList elemSeq

newtype VList = VList (Seq VListElem)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

instance BreakableList VList where

  naturalSpan = naturalHeight

  totalGlue (VList elemSeq) = fold $ mapMaybe toGlue $ toList elemSeq

instance Dimensioned VList where

  naturalLength dim (VList elemSeq) =
    let elemList = toList elemSeq
    in case dim of
         BoxWidth ->
           -- The width of a \vbox is the maximum distance by which an
           -- enclosed box extends to the right of the reference point, taking
           -- possible shifting into account. This width is always nonnegative.
           maxLength BoxWidth elemSeq
         BoxHeight ->
           -- h + d for all but last elements, plus the last element's height.
           let allButLast = case initMay elemList of
                 Nothing -> 0
                 Just _inits -> sum $ hPlusD <$> _inits
               lastElem = case lastMay elemList of
                 Nothing -> 0
                 Just lst -> naturalLength BoxHeight lst
           in allButLast + lastElem
         -- TODO: All this logic:
         -- When wrapping a vertical list via \vbox, to compute its
         -- depth,
         -- - If the list contains no boxes, the depth is zero.
         -- - If there's at least one box, consider the final box.
         --     - if the box is followed by kerning or glue,
         --       possibly with intervening penalties or other
         --       things, the depth is zero.
         --     - otherwise, the depth is that box's depth. If the
         --       computed depth exceeds \boxmaxdepth,
         --     - the depth is \boxmaxdepth
         --     - add the excess depth to the box's natural height,
         --       essentially moving the reference point down to
         --       reduce the depth to the stated maximum.
         BoxDepth -> case lastMay elemList of
           Nothing -> 0
           Just lst -> naturalLength BoxDepth lst

instance Describe VList where

  describe (VList elems) =
    describeNamedRelFoldable 0 "VList" elems

data VBoxAlignType
  = DefaultAlign -- \vbox
  | TopAlign -- \vtop
  deriving stock (Show, Eq)

-- Vertical list.
data VListElem
  = VListBaseElem BaseElem
  | ListGlue (Glue Length)
  | ListPenalty Penalty
  deriving stock Show

instance BreakableListElem VListElem where

  toGlue (ListGlue g) = Just g
  toGlue _ = Nothing

  isDiscardable e = case e of
    ListGlue _ -> True
    ListPenalty _ -> True
    VListBaseElem (ElemKern _) -> True
    _ -> False

  isBox e = case e of
    VListBaseElem (ElemBox _) -> True
    VListBaseElem (ElemRule _) -> True
    _ -> False

  toBreakItem adj = case adj of
    Adj (Just x) (ListGlue g) _
      | not (isDiscardable x) -> Just $ GlueBreak g
    Adj _ (VListBaseElem (ElemKern k)) (Just (ListGlue _)) ->
      Just $ KernBreak k
    Adj _ (ListPenalty p) _ -> Just $ PenaltyBreak p
    _ -> Nothing

axisVListElemNaturalLength :: Axis -> BoxDim -> VListElem -> Length
axisVListElemNaturalLength ax dim e = case e of
  VListBaseElem be -> axisBaseElemNaturalLength ax dim be
  ListGlue g -> spacerNaturalLength ax dim $ dimen g
  ListPenalty _ -> Length 0

instance Dimensioned VListElem where

  naturalLength = axisVListElemNaturalLength Vertical

-- Horizontal list.
-- TODO: WhatsIt, Leaders, Mark, Insertion
-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HListElem
  = HVListElem VListElem
  | HListHBaseElem HBaseElem
  deriving stock Show

instance BreakableListElem HListElem where

  toGlue (HVListElem e) = toGlue e
  toGlue _ = Nothing

  isDiscardable (HVListElem e) = isDiscardable e
  isDiscardable (HListHBaseElem (ElemCharacter _)) = False

  isBox (HVListElem e) = isBox e
  isBox (HListHBaseElem (ElemCharacter _)) = True

  -- TODO: Add math formula conditions.
  -- TODO: Discretionary break and Math-off.
  toBreakItem adj = case adj of
    Adj (Just x) (HVListElem (ListGlue g)) _
      | not (isDiscardable x) -> Just $ GlueBreak g
    Adj
      _
      (HVListElem (VListBaseElem (ElemKern k)))
      (Just (HVListElem (ListGlue _))) -> Just $ KernBreak k
    Adj _ (HVListElem (ListPenalty p)) _ -> Just $ PenaltyBreak p
    _ -> Nothing

instance Dimensioned HListElem where

  naturalLength dim (HVListElem e) =
    axisVListElemNaturalLength Horizontal dim e
  naturalLength dim (HListHBaseElem e) = naturalLength dim e

-- Display.
instance Describe VListElem where

  describe = \case
    VListBaseElem baseElem ->
      describeNamedRel 0 "VListElem/Base" baseElem
    ListGlue g ->
      describeNamedRel 0 "VListElem/Glue" g
    ListPenalty p ->
      describeNamedRel 0 "VListElem/Penalty" p

instance Describe HListElem where

  describe = \case
    HVListElem vListElem ->
      describeNamedRel 0 "HListElem/V" vListElem
    HListHBaseElem hBaseElem ->
      describeNamedRel 0 "HListElem/HBase" hBaseElem
