{-# LANGUAGE DuplicateRecordFields #-}

module HeX.BreakList.Page where

import HeX.BreakList

import qualified HeX.Unit                      as UN
import qualified HeX.Box                       as B
import           HeX.Config

data PageBreakJudgment
  -- Penalty >= 10k.
  = DoNotBreak
  -- q >= 10k or b = infinity.
  | BreakPageAtBest
  -- p <= -10k.
  | BreakPageHere
  | TrackCost Int
  deriving (Show)

pageBreakJudgment :: [BreakableVListElem] -> BreakItem -> DesiredHeight -> PageBreakJudgment
pageBreakJudgment cs breakItem (DesiredHeight h) =
  inner penalty splitInsertPenalties _badness
  where
    _badness = badness $ listGlueStatus h cs
    penalty = breakPenalty breakItem
    splitInsertPenalties = 0
    inner _ _ InfiniteBadness = BreakPageAtBest
    inner p q (FiniteBadness b)
      | p >= UN.tenK = DoNotBreak
      | q >= UN.tenK = BreakPageAtBest
      | p <= -UN.tenK = BreakPageHere
      | b == UN.tenK = TrackCost UN.hunK
      | otherwise = TrackCost $ b + p + q

-- Assumes cs is in reading order.
setPage :: DesiredHeight -> [BreakableVListElem] -> B.Page
setPage (DesiredHeight h) cs =
  let _status = listGlueStatus h cs
  in B.Page $ setVList _status cs
