module HeX.BreakList.Page where

import           HeXlude

import           Data.Adjacent           ( Adj(..) )

import qualified HeX.Box                 as B
import           HeX.BreakList.BreakList
import           HeX.BreakList.Elem
import           HeX.BreakList.Judge
import           HeX.BreakList.Set
import           HeX.Config.Parameters
import           HeX.Unit                ( hunK, tenK )

data PageBreakJudgment =
    DoNotBreak | BreakPageAtBest | BreakPageHere | TrackCost !Int
    deriving ( Show )

pageBreakJudgment
    :: [VListElem]
    -> BreakItem
    -> LenParamVal VSize
    -> PageBreakJudgment
pageBreakJudgment cs breakItem (LenParamVal h) =
    case badness $ listGlueStatus h cs of
        InfiniteBadness -> BreakPageAtBest
        FiniteBadness b ->
            let p = breakPenalty breakItem
                q = 0
            in
                if
                    | p >= tenK -> DoNotBreak
                    | q >= tenK -> BreakPageAtBest
                    | p <= -tenK -> BreakPageHere
                    | b == tenK -> TrackCost hunK
                    | otherwise -> TrackCost $ b + p + q

setPage :: LenParamVal VSize -> [VListElem] -> B.Page
setPage (LenParamVal h) cs = B.Page $ setVList (listGlueStatus h cs) cs

data CurrentPage = CurrentPage { items :: [VListElem]
                               , bestPointAndCost :: Maybe (Int, Int)
                               }

newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage [] Nothing

runPageBuilder :: LenParamVal VSize
               -> CurrentPage
               -> [VListElem]
               -> [B.Page]
runPageBuilder desiredV (CurrentPage cur _) [] =
    [ setPage desiredV $ reverse cur ]
-- If the current vlist has no boxes, we discard a discardable item.
-- Otherwise, if a discardable item is a legitimate breakpoint, we compute
-- the cost c of breaking at this point.
runPageBuilder desiredV (CurrentPage cur _bestPointAndCost) (x : xs)
        | not $ any isBox cur =
            let nextXs = if isDiscardable x
                         then cur
                         else x : cur
            in
                runPageBuilder desiredV
                               (CurrentPage nextXs _bestPointAndCost)
                               xs
        | isDiscardable x = case toBreakItem Adj { adjPre  = headMay cur
                                                 , adjVal  = x
                                                 , adjPost = headMay xs
                                                 } of
            -- If we can't break here, just add it to the list and continue.
            Nothing  -> usualContinue
            Just brk -> case ( pageBreakJudgment cur brk desiredV
                             , _bestPointAndCost
                             ) of
                (DoNotBreak, _) -> usualContinue
                -- I don't think this condition will ever be satisfied, but if we
                -- decide to break before any valid break-point has been considered,
                -- just carry on.
                (BreakPageAtBest, Nothing) -> usualContinue
                -- If c = ∞, we break at the best breakpoint so far.
                -- The current vlist material following that best breakpoint is
                -- returned to the recent contributions, to consider again.
                (BreakPageAtBest, Just (iBest, _)) ->
                    -- the `reverse` will put both of these into reading order.
                    let (curNewPage, toReturn) = splitAt iBest $ reverse cur
                        newPage = setPage desiredV curNewPage
                    in
                        -- xs is also in reading order
                        -- We didn't actually split at x: x was just what made us compute
                        -- cost and notice we'd gone too far. So add it to the left-overs
                        -- to return.
                        (newPage :) $
                        runPageBuilder desiredV
                                       newCurrentPage
                                       (toReturn <> (x : xs))
                -- If p ≤ −10000, we know the best breakpoint is this one, so break
                -- here.
                (BreakPageHere, _) ->
                    -- the `reverse` will put this into reading order.
                    let newPage = setPage desiredV $ reverse cur
                    in
                        (newPage :) $ runPageBuilder desiredV newCurrentPage xs
                -- If the resulting cost <= the smallest cost seen so far, remember
                -- the current breakpoint as the best so far.
                (TrackCost cHere, _) ->
                    let thisPointAndCost = Just (length cur, cHere)
                        newBestPointAndCost = case _bestPointAndCost of
                            Nothing         -> thisPointAndCost
                            Just (_, cBest) -> if cHere > cBest
                                               then _bestPointAndCost
                                               else thisPointAndCost
                    in
                        runPageBuilder desiredV
                                       (CurrentPage (x : cur)
                                                    newBestPointAndCost)
                                       xs
        -- If we can't break here, just add it to the list and continue.
        | otherwise = usualContinue
  where
    usualNextPage = CurrentPage (x : cur) _bestPointAndCost

    usualContinue = runPageBuilder desiredV usualNextPage xs
