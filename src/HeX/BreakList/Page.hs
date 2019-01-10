{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

module HeX.BreakList.Page where

import HeX.BreakList

import           HeX.Unit                       ( tenK
                                                , hunK
                                                )
import qualified HeX.Box                       as B
import           HeX.Config

data PageBreakJudgment
    = DoNotBreak
    | BreakPageAtBest
    | BreakPageHere
    | TrackCost !Int
    deriving (Show)

pageBreakJudgment
    :: [BreakableVListElem]
    -> BreakItem
    -> LenParamVal VSize
    -> PageBreakJudgment
pageBreakJudgment cs breakItem (LenParamVal h) =
    case badness $ listGlueStatus h cs of
        InfiniteBadness -> BreakPageAtBest
        FiniteBadness b ->
            let
                p = breakPenalty breakItem
                q = 0
            in if
                | p >= tenK  -> DoNotBreak
                | q >= tenK  -> BreakPageAtBest
                | p <= -tenK -> BreakPageHere
                | b == tenK  -> TrackCost hunK
                | otherwise  -> TrackCost $ b + p + q

setPage :: LenParamVal VSize -> [BreakableVListElem] -> B.Page
setPage (LenParamVal h) cs =
  B.Page $ setVList (listGlueStatus h cs) cs
