{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module HeX.BreakList.Line where

import           Prelude                 hiding ( lines )

import           Data.List.Extra         hiding ( lines )
import           Control.Applicative            ( empty )
import           Control.Monad                  ( guard )

import qualified Data.Adjacent                 as A
import qualified HeX.Box                       as B
import qualified HeX.Unit                      as UN
import           HeX.Config
import           HeX.BreakList

newtype BadnessSize = BadnessSize { unBadnessSize :: Int } deriving (Eq, Show, Num)

data Line = Line
  { contents :: [BreakableHListElem]
  , status :: GlueStatus
  }
  deriving Show

type Entry = A.Adjacency BreakableHListElem
newtype Demerit = Demerit Int
  deriving (Show, Eq, Ord, Num)

type EdgeDistance = Demerit
type EdgeStatus = GlueStatus
type EdgeValue = [Entry]

data Node = Root | Branch Int
  deriving Show

newtype IsDiscarding = IsDiscarding Bool

data InEdge = InEdge EdgeValue Node IsDiscarding

data WithSummary a b = WithSummary {value :: a, summary :: b}
  deriving Show
type WithDistance a = WithSummary a (EdgeStatus, EdgeDistance)
type WithStatus a = WithSummary a EdgeStatus

data Route = Route [WithStatus EdgeValue] EdgeDistance
  deriving Show

instance Eq Route where
  (Route _ distA) == (Route _ distB) = distA == distB

instance Ord Route where
  compare (Route _ distA) (Route _ distB) = compare distA distB

withStatus :: HSize -> InEdge -> WithStatus InEdge
withStatus (HSize dw) e@(InEdge v _ _)
  = WithSummary e (listGlueStatus dw $ A.fromAdjacencies v)

lineDemerit :: LinePenalty -> BadnessSize -> BreakItem -> Demerit
lineDemerit lp b br =
  let breakDemerit = breakPenalty br ^ (2 :: Int)
      listDemerit = (unLinePenalty lp + unBadnessSize b) ^ (2 :: Int)
  in Demerit $ breakDemerit + listDemerit

toOnlyAcceptables :: Tolerance -> LinePenalty -> BreakItem -> [WithStatus InEdge] -> [WithDistance InEdge]
toOnlyAcceptables (Tolerance tol) lp br ds = do
    WithSummary y st <- ds
    case badness st of
      InfiniteBadness -> empty
      FiniteBadness b -> do
        guard $ breakPenalty br < UN.tenK && b <= tol
        let _demerit = lineDemerit lp (BadnessSize b) br
        pure $ WithSummary y (st, _demerit)

toOnlyPromisings :: [WithStatus a] -> [a]
toOnlyPromisings ds = [y | (WithSummary y d) <- ds, isPromising d]
  where
    isPromising (NaturallyBad Full Unfixable) = False
    isPromising _ = True

inEdgeCons :: Entry -> InEdge -> InEdge
inEdgeCons c e@(InEdge cs n (IsDiscarding discarding))
  | discarding && isDiscardable (A.fromAdjacency c) = e
  | otherwise = InEdge (c : cs) n (IsDiscarding False)

inEdgeConcat :: [Entry] -> InEdge -> InEdge
inEdgeConcat xs e = foldr inEdgeCons e xs

finaliseInEdge :: Entry -> InEdge -> InEdge
-- If the break-point is at glue, then the line doesn't include that glue.
finaliseInEdge A.Adjacency { v = HVListElem (ListGlue _) } e = e
-- If the break is at some other type of break, the line includes it.
finaliseInEdge x (InEdge cs n discard) = InEdge (x:cs) n discard

-- Notation:
-- - An 'Acceptable' line is one that can be realistically considered as being
--   part of an acceptable paragraph.
-- - A 'Promising' line is one that is bare enough that it is either
--   'acceptable' now, or may be acceptable in the future. Conversely, a line
--   that is not promising is one that is so full that it isn't acceptable now,
--   and will never become acceptable. We can remove such edges from
--   consideration, because we know that once a line is not promising, it will
--   never become acceptable.
appendEntry :: HSize -> Tolerance -> LinePenalty -> ([InEdge], [Route], [Entry]) -> Entry -> ([InEdge], [Route], [Entry])
appendEntry dw tol lp (prevInEdges, rs, chunk) x@(toBreakItem -> Just br) =
  let
    -- Extend the accumulating edges with the normal-items 'chunk' we have
    -- accumulated since seeing the last break item.
    inEdges = inEdgeConcat chunk <$> prevInEdges
    -- Here, we just consider the in-edges 'unfinalised', i.e. without each
    -- edge's lines stripped to the form used in an actual break. This is
    -- because the edges may be passed on to subsequent calls, in such form, to
    -- build up longer lines.
    promisingInEdges = toOnlyPromisings $ withStatus dw <$> inEdges
    -- Here, we 'finalise' the promising edges, then must get the status again,
    -- to get 'candidate' edges to actually break.
    candidateBrokenDInEdges = withStatus dw . finaliseInEdge x <$> promisingInEdges
  in
    -- We filter the candidates to those that we could actually accept.
    case toOnlyAcceptables tol lp br candidateBrokenDInEdges of
      -- If we can't break at this point acceptably somehow, just treat the
      -- break item like a non-break item.
      [] -> (promisingInEdges, rs, [x])
      -- If we have some acceptable edges, this is a fruitful avenue to
      -- explore.
      acceptableDInEdges ->
        let
          -- For subsequent calls, build up the still-promising edges with
          -- the break item added.
          grownPromisingInEdges = inEdgeCons x <$> promisingInEdges
          -- Add a new edge representing the possibility to break here. Note
          -- we set this edge to 'discarding', so that subsequent discardable
          -- items won't be added.
          newInEdge = InEdge [] (Branch $ length rs) (IsDiscarding True)
          -- Add that new edge to the existing, now-grown, edges.
          newInEdges = newInEdge : grownPromisingInEdges
          -- Find the best way to reach this break-point, given the
          -- acceptable previous lines and the known best ways to reach those
          -- lines.
          newRoute = shortestRoute acceptableDInEdges rs
        in
          -- The new state should contain:
          -- - The ongoing edges that are still promising
          -- - The optimal route to reach this break, appended to the list of
          --   node best-routes.
          -- - A new empty chunk, to accumulate the contents we see until the
          --   next break.
          (newInEdges, newRoute:rs, [])
-- If we see a non-break item, the new state is the same as the old, but with
-- the item appended to the accumulated items 'chunk'.
appendEntry _ _ _ (prevInEdges, rs, chunk) x = (prevInEdges, rs, x:chunk)

shortestRoute :: [WithDistance InEdge] -> [Route] -> Route
shortestRoute [] _ = Route [] $ Demerit 0
shortestRoute es rs = minimum (bestSubroute <$> es)
  where
    bestSubroute (WithSummary (InEdge ev Root _) (st, ed))
      = Route [WithSummary ev st] ed
    bestSubroute (WithSummary (InEdge ev (Branch sn) _) (st, ed))
      = let
        sourceNodeIdx = length rs - sn - 1
        Route sevs sd = rs !! sourceNodeIdx
      in
        Route (WithSummary ev st:sevs) (ed + sd)

bestRoute :: HSize -> Tolerance -> LinePenalty -> [BreakableHListElem] -> [Line]
bestRoute dw tol lp xs =
  let
    initialState = ([InEdge [] Root (IsDiscarding False)], [], [])
    (prevInEdges, rs, chunk) = foldl' (appendEntry dw tol lp) initialState (A.toAdjacents xs)
    inEdges = inEdgeConcat chunk <$> prevInEdges
    acceptableDInEdges = toOnlyAcceptables tol lp NoBreak $ withStatus dw <$> inEdges
    (Route lns _) = shortestRoute acceptableDInEdges rs
  in
    (\(WithSummary ev st) -> Line (reverse $ A.fromAdjacencies ev) st) <$> reverse lns

-- Expects contents in reverse order.
-- Returns lines in reading order.
setParagraph
  :: ([BreakableHListElem] -> [Line]) -> [BreakableHListElem] -> [[B.HBoxElem]]
setParagraph _ [] = []
setParagraph getRoute (x : xs) =
  let
    -- Remove the final item if it's glue.
      xsTrimmed = case x of
        HVListElem (ListGlue _) -> xs
        _                       -> x : xs
      -- Add extra bits to finish the list.
      -- Append \penalty10k \hfil \penalty-10k.
      xsFinished =
        (HVListElem $ ListPenalty $ Penalty $ -UN.tenK)
          : (HVListElem $ ListGlue filGlue)
          : (HVListElem $ ListPenalty $ Penalty UN.tenK)
          : xsTrimmed
  in  setLine <$> getRoute (reverse xsFinished)
  where setLine (Line conts _status) = setHList _status conts
