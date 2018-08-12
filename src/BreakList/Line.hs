{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module BreakList.Line where

import Prelude hiding (lines)

import Control.Applicative (liftA2)
import Data.List (intercalate)

import qualified Adjacent as A
import qualified Box as B

import BreakList

data Break a
  = Break { before :: [a]
          , after :: [a]
          , item :: BreakItem }

data Line = Line
  { contents :: [BreakableHListElem]
  , status :: ListStatus
  }

data Route = Route {lines :: [Line], demerit :: Int }

instance Eq Route where
  (==) a b = demerit a == demerit b

instance Ord Route where
  compare a b = compare (demerit a) (demerit b)

instance Show a => Show (Break a) where
  show Break{before=b, after=a, item=k} =
    "Break ->"
    ++ "\nBefore: " ++ show b
    ++ "\nBreak at: " ++ show k
    ++ "\nAfter: " ++ show a
  showList bs = ((intercalate "\n" $ fmap show bs) ++)

allBreaks :: [BreakableHListElem] -> [Break BreakableHListElem]
allBreaks = inner [] . A.toAdjacents
  where
    -- Bit pretentious: fAnd maps two functions 'f' and 'g' into one
    -- function whose result is 'f x && g x'.
    fAnd = liftA2 (&&)

    discardAfterBreak = fAnd (isDiscardable . A.fromAdjacent) (not . isBreakItem)
    trimAfterBreak = A.fromAdjacents . dropWhile discardAfterBreak

    trimBeforeBreak this seen = if isGlue this then seen else this:seen

    inner seen [] = [Break{before=reverse seen, after=[], item=NoBreak}]
    inner seen (thisAdj@(A.Adjacency (_, this, _)):rest) =
      case toBreakItem thisAdj of
        Nothing ->
          inner (this:seen) rest
        Just b ->
          let
            brk = Break{before=reverse $ trimBeforeBreak this seen
                       , after=trimAfterBreak rest, item=b}
          in
            brk:inner (this:seen) rest

-- Optimised filter for cases where we know that once a test has gone from true
-- to false, the test will never again succeed.
touchyFilter :: (a -> Bool) -> [a] -> [a]
touchyFilter _ [] = []
touchyFilter test (x:xs)
  | test x = x:takeWhile test xs
  | otherwise = touchyFilter test xs

-- Expects contents in reading order.
bestRouteInner :: Int -> Int -> Int -> [BreakableHListElem] -> Route
bestRouteInner _ _ _ [] = Route {lines=[], demerit=0}
bestRouteInner desiredWidth tolerance linePenalty cs =
  let
    breaks = allBreaks cs
    analyzedBreaks = zip breaks $ analyzeBreak <$> breaks

    sensibleBreaks = do
      (brk, (_status, _badness)) <- touchyFilter isSensibleBreak analyzedBreaks

      bNr <- case _badness of
        FiniteBadness b -> return b
        _ -> fail ""
      return (brk, (_status, bNr))
    considerableBreaks = filter isConsiderableBreak sensibleBreaks
  in
    case filter isMandatoryBreak sensibleBreaks of
      x:_ ->
        bestRouteFromBreak False x
      [] ->
        let
        in case considerableBreaks of
          [] -> noBreakRoute
          _ -> minimum $ bestRouteFromBreak True <$> considerableBreaks
  where
    analyzeBreak Break{before=bef} =
      let _status = listGlueSetRatio desiredWidth bef
      in (_status, listStatusBadness _status)

    isSensibleBreak (_, (NaturallyBad Full Unfixable, _)) = False
    isSensibleBreak (_, (_, _)) = True

    -- "any penalty that is −10000 or less is considered so small that TeX
    -- will always break there."
    isMandatoryBreak (Break{item=br}, _) = breakPenalty br <= -tenK

    isConsiderableBreak (Break{item=br}, (_, b)) =
      -- "TeX will not even consider such a line if p ≥ 10000, or b exceeds the
      -- current tolerance or pretolerance."
      -- But also:
      breakPenalty br < tenK && b <= tolerance

    bestRouteFromBreak addDemerit (Break{before=bef, after=aft, item=br}, (_status, bad)) =
      let
        d = if addDemerit then lineDemerit bad br else 0
        subRoute = bestRouteInner desiredWidth tolerance linePenalty aft
        thisLine = Line{contents=bef, status=_status}
      in appendToRoute subRoute thisLine d

    appendToRoute Route{lines=subLns, demerit=subD} ln d =
      Route{lines=ln:subLns, demerit=subD + d}

    noBreakRoute = Route{lines=[Line{contents=cs, status=NaturallyBad Full Unfixable}], demerit= -1}

    lineDemerit bad br =
      let
        breakDemerit = breakPenalty br ^ (2 :: Int)
        listDemerit = (linePenalty + bad)^(2 :: Int)
      in
        breakDemerit + listDemerit

bestRoute :: Int -> Int -> Int -> [BreakableHListElem] -> [Line]
bestRoute x y z cs = lines $ bestRouteInner x y z cs

-- Expects contents in reverse order.
-- Returns lines in reading order.
setParagraph :: ([BreakableHListElem] -> [Line]) -> [BreakableHListElem] -> [[B.HBoxElem]]
setParagraph _ [] = []
setParagraph getRoute cs@(end:rest) = fmap setLine lns
  where
    -- Remove the final item if it's glue.
    csTrimmed = case end of
      HGlue _ -> rest
      _ -> cs
    -- Append \penalty10k \hfil \penalty-10k.
    -- Add extra bits to finish the list.
    csFinished = (HPenalty $ Penalty $ -tenK):hFilGlue:(HPenalty $ Penalty tenK):csTrimmed

    lns = getRoute $ reverse csFinished

    setLine Line{contents=lncs, status=_status} = setListElems _status lncs
