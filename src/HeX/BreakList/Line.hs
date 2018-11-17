{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module HeX.BreakList.Line where

import           Data.Maybe
import           Prelude                 hiding ( lines )
import Data.List.Extra hiding (lines)

import           Control.Applicative            ( liftA2 )
import           Data.List                      ( intercalate )

import qualified Adjacent                      as A
import qualified HeX.Box                       as B
import qualified HeX.Unit                      as UN

import           HeX.BreakList

data Break a = Break
  { before :: [a]
  , after :: [a]
  , item :: BreakItem
  }

data Line = Line
  { contents :: [BreakableHListElem]
  , status :: GlueStatus
  }

data Route = Route
  { lines :: [Line]
  , demerit :: Int
  }

instance Eq Route where
  (==) a b = demerit a == demerit b

instance Ord Route where
  compare a b = compare (demerit a) (demerit b)

instance Show a => Show (Break a) where
  show Break {before = b, after = a, item = k} =
    "Break ->" ++
    "\nBefore: " ++ show b ++ "\nBreak at: " ++ show k ++ "\nAfter: " ++ show a
  showList bs = ((intercalate "\n" $ fmap show bs) ++)

allBreaks :: BreakableListElem a => [a] -> [Break a]
allBreaks = inner [] . A.toAdjacents
 where
  discardAfterBreak =
    liftA2 (&&) (isDiscardable . A.fromAdjacent) (isNothing . toBreakItem)
  trimAfterBreak = A.fromAdjacents . dropWhile discardAfterBreak

  trimBeforeBreak (toGlue -> Just _) seen = seen
  trimBeforeBreak this               seen = this : seen

  inner seen [] = [Break {before = reverse seen, after = [], item = NoBreak}]
  inner seen (thisAdj@(A.Adjacency (_, this, _)) : rest) =
    case toBreakItem thisAdj of
      Nothing -> inner (this : seen) rest
      Just b ->
        let brk = Break
              { before = reverse $ trimBeforeBreak this seen
              , after  = trimAfterBreak rest
              , item   = b
              }
        in  brk : inner (this : seen) rest

-- Optimised filter for cases where we know that once a test has gone from true
-- to false, the test will never again succeed.
touchyFilter :: (a -> Bool) -> [a] -> [a]
touchyFilter _ [] = []
touchyFilter test (x : xs) | test x    = x : takeWhile test xs
                           | otherwise = touchyFilter test xs

-- Expects contents in reading order.
bestRouteInner :: Int -> Int -> Int -> [BreakableHListElem] -> Route
bestRouteInner _ _ _ [] = Route {lines = [], demerit = 0}
bestRouteInner desiredWidth tolerance linePenalty cs
  = let
      breaks         = allBreaks cs
      analyzedBreaks = zip breaks $ analyzeBreak <$> breaks
      sensibleBreaks = do
        (brk, (_status, _badness)) <- touchyFilter isSensibleBreak
                                                   analyzedBreaks
        bNr <- case _badness of
          FiniteBadness b -> return b
          _               -> fail ""
        return (brk, (_status, bNr))
    in
      case filter isMandatoryBreak sensibleBreaks of
        x : _ -> bestRouteFromBreak False x
        []    -> case filter isConsiderableBreak sensibleBreaks of
          [] -> noBreakRoute
          bs -> minimum $ bestRouteFromBreak True <$> bs
 where
  analyzeBreak Break { before = bef } =
    let _status = listGlueStatus desiredWidth bef in (_status, badness _status)
  isSensibleBreak (_, (NaturallyBad Full Unfixable, _)) = False
  isSensibleBreak (_, (_, _)                          ) = True
  -- "any penalty that is −10000 or less is considered so small that TeX
  -- will always break there."
  isMandatoryBreak (Break { item = br }, _) = breakPenalty br <= -UN.tenK
  isConsiderableBreak (Break { item = br }, (_, b)) =
    breakPenalty br < UN.tenK && b <= tolerance
  -- "TeX will not even consider such a line if p ≥ 10000, or b exceeds the
  -- current tolerance or pretolerance."
  -- But also:
  bestRouteFromBreak addDemerit (Break { before = bef, after = aft, item = br }, (_status, bad))
    = let d        = if addDemerit then lineDemerit bad br else 0
          subRoute = bestRouteInner desiredWidth tolerance linePenalty aft
          thisLine = Line {contents = bef, status = _status}
      in  appendToRoute subRoute thisLine d
  appendToRoute Route { lines = subLns, demerit = subD } ln d =
    Route {lines = ln : subLns, demerit = subD + d}
  noBreakRoute = Route
    { lines   = [Line {contents = cs, status = NaturallyBad Full Unfixable}]
    , demerit = -1
    }
  lineDemerit bad br =
    let breakDemerit = breakPenalty br ^ (2 :: Int)
        listDemerit  = (linePenalty + bad) ^ (2 :: Int)
    in  breakDemerit + listDemerit


-- *** Under construction ***

judgeEdge :: EdgeValue -> EdgeStatus
judgeEdge (length -> x)
    | x < 90 = TooBare
    | x > 110 = TooFull
    | otherwise = Acceptable 100

type Entry = A.Adjacency BreakableHListElem
type EdgeDistance = Int
data EdgeStatus = TooBare | TooFull | Acceptable EdgeDistance
type EdgeValue = [Entry]

data Node = Root | Branch Int
    deriving Show

data InEdge = InEdge EdgeValue Node

data WithSummary a b = WithSummary {value :: a, summary :: b}
type WithDistance a = WithSummary a EdgeDistance
type WithStatus a = WithSummary a EdgeStatus

data Route' = Route' [EdgeValue] EdgeDistance
    deriving Show

instance Eq Route' where
    (Route' _ a) == (Route' _ b) = a == b

instance Ord Route' where
    compare (Route' _ a) (Route' _ b) = compare a b

withStatus :: InEdge -> WithStatus InEdge
withStatus e@(InEdge v _) = WithSummary e $ judgeEdge v

toOnlyAcceptables :: [WithStatus InEdge] -> [WithDistance InEdge]
toOnlyAcceptables ds = [WithSummary y x | (WithSummary y (Acceptable x)) <- ds]

toOnlyPromisings :: [WithStatus a] -> [WithStatus a]
toOnlyPromisings ds = [WithSummary y d | (WithSummary y d) <- ds, isPromising d]
    where
        isPromising TooFull = False
        isPromising _ = True

growInEdge :: Entry -> InEdge -> InEdge
growInEdge c (InEdge cs n) = InEdge (c : cs) n

stretchInEdge :: [Entry] -> InEdge -> InEdge
stretchInEdge s (InEdge cs n) = InEdge (s ++ cs) n

appendEntry :: ([InEdge], [Route'], [Entry]) -> Entry -> ([InEdge], [Route'], [Entry])
appendEntry (prevInEdges, rs, chunk) x@(toBreakItem -> Nothing) =
  (prevInEdges, rs, x:chunk)
appendEntry (prevInEdges, rs, chunk) x@(toBreakItem -> Just b) =
  let
    inEdges = stretchInEdge chunk <$> prevInEdges
    promisingDInEdges = toOnlyPromisings $ withStatus <$> inEdges
    promisingInEdges = value <$> promisingDInEdges
  in
    case toOnlyAcceptables promisingDInEdges of
      -- If we can't break at this point acceptably somehow, just treat
      -- like a non-break character.
      -- [] -> (promisingInEdges, rs, [x])
      [] -> (promisingInEdges, rs, [x])
      acceptableDInEdges ->
        let
            grownPromisingInEdges = growInEdge x <$> promisingInEdges
            newInEdges = InEdge [] (Branch $ length rs) : grownPromisingInEdges
            r' = shortestRoute acceptableDInEdges rs
        in
            (newInEdges, r':rs, [])

shortestRoute :: [WithDistance InEdge] -> [Route'] -> Route'
shortestRoute [] _ = Route' [] 0
shortestRoute ies rs = minimum (getBSR <$> ies)
    where
        getBSR (WithSummary (InEdge ev Root) ed)
            = Route' [ev] ed
        getBSR (WithSummary (InEdge ev (Branch sn)) ed)
            = let
                sourceNodeIdx = length rs - sn - 1
                Route' sevs sd = rs !! sourceNodeIdx
            in
                Route' (ev:sevs) (ed + sd)

bestRoute :: Int -> Int -> Int -> [BreakableHListElem] -> [Line]
bestRoute desiredWidth tolerance linePenalty xs =
  let
    (prevInEdges, rs, chunk) = foldl' appendEntry ([InEdge [] Root], [], []) (A.toAdjacents xs)
    inEdges = stretchInEdge chunk <$> prevInEdges
    acceptableDInEdges = toOnlyAcceptables $ withStatus <$> inEdges
    (Route' evs _) = shortestRoute acceptableDInEdges rs
  in
    (\ev -> Line (reverse ev) NaturallyGood) . A.fromAdjacents <$> reverse evs
    -- [Line (take 10 xs) NaturallyGood, Line (take 20 xs) NaturallyGood]

-- *** Under construction ***

-- Expects contents in reverse order.
-- Returns lines in reading order.
setParagraph :: ([BreakableHListElem] -> [Line]) -> [BreakableHListElem] -> [[B.HBoxElem]]
setParagraph _        []              = []
setParagraph getRoute cs@(end : rest) = fmap setLine lns
    -- Remove the final item if it's glue.
 where
  csTrimmed = case end of
    HGlue _ -> rest
    _       -> cs
  -- Append \penalty10k \hfil \penalty-10k.
  -- Add extra bits to finish the list.
  csFinished =
    (HPenalty $ Penalty $ -UN.tenK)
      : HGlue filGlue
      : (HPenalty $ Penalty UN.tenK)
      : csTrimmed
  lns = getRoute $ reverse csFinished
  setLine Line { contents = lncs, status = _status } = set _status lncs
