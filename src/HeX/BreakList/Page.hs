module HeX.BreakList.Page where

import           HeXlude

import           Data.Adjacent           ( Adj(..) )
import qualified Data.Sequence             as Seq

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
    :: ForwardVList
    -> BreakItem
    -> LenParamVal VSize
    -> PageBreakJudgment
pageBreakJudgment cs breakItem (LenParamVal h) =
    case badness $ listGlueStatus h cs of
        InfiniteBadness ->
            BreakPageAtBest
        FiniteBadness b
            | p >= tenK ->
                DoNotBreak
            | q >= tenK ->
                BreakPageAtBest
            | p <= -tenK ->
                BreakPageHere
            | b == tenK ->
                TrackCost hunK
            | otherwise ->
                TrackCost $ b + p + q
  where
    p = breakPenalty breakItem
    q = 0

setPage :: LenParamVal VSize -> ForwardVList -> B.Page
setPage (LenParamVal h) cs = B.Page $ setVList (listGlueStatus h cs) cs

data CurrentPage = CurrentPage
    { items :: ForwardVList
    , bestPointAndCost :: Maybe (Int, Int)
    }

newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage mempty Nothing

runPageBuilder :: LenParamVal VSize
               -> CurrentPage
               -> ForwardVList
               -> ForwardDirected Seq B.Page
runPageBuilder desiredV curPage@(CurrentPage curElemsFwd@(FDirected curFwdSeq) _bestPointAndCost) vList = case vList of
    FDirected Empty ->
        pure (setPage desiredV curElemsFwd)
    FDirected (x :<| xs)
        -- If the current vlist has no boxes, we discard a discardable item.
        -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
        -- the cost c of breaking at this point.
        | not $ any isBox curElemsFwd ->
            continue (if isDiscardable x then curPage else usualNextPage) smallerVList
        | isDiscardable x ->
            case toBreakItem Adj { adjPre  = seqLastMay curFwdSeq
                                 , adjVal  = x
                                 , adjPost = seqHeadMay xs
                                 } of
                -- If we can't break here, just add it to the list and continue.
                Nothing  ->
                    usualContinue
                Just brk ->
                    case (pageBreakJudgment curElemsFwd brk desiredV, _bestPointAndCost) of
                        (DoNotBreak, _) ->
                            usualContinue
                        -- I don't think this condition will ever be satisfied, but if we
                        -- decide to break before any valid break-point has been considered,
                        -- just carry on.
                        (BreakPageAtBest, Nothing) ->
                            usualContinue
                        -- If c = ∞, we break at the best breakpoint so far.
                        -- The current vlist material following that best breakpoint is
                        -- returned to the recent contributions, to consider again.
                        (BreakPageAtBest, Just (iBest, _)) ->
                            let
                                (newPageElems, leftoverVList) = (\(sqA, sqB) -> (FDirected sqA, FDirected sqB)) $ Seq.splitAt iBest curFwdSeq
                            in
                                addPage newPageElems (leftoverVList <> vList)
                        -- If p ≤ −10000, we know the best breakpoint is this one, so break
                        -- here.
                        (BreakPageHere, _) ->
                            addPage curElemsFwd smallerVList
                        -- If the resulting cost <= the smallest cost seen so far, remember
                        -- the current breakpoint as the best so far.
                        (TrackCost cHere, _) ->
                            let thisPointAndCost = Just (length curElemsFwd, cHere)
                                newBestPointAndCost = case _bestPointAndCost of
                                    Nothing         -> thisPointAndCost
                                    Just (_, cBest) -> if cHere > cBest
                                                       then _bestPointAndCost
                                                       else thisPointAndCost
                            in
                                continue (pageWithAddedElem newBestPointAndCost) smallerVList
        -- If we can't break here, just add it to the list and continue.
        | otherwise ->
            usualContinue
      where
        smallerVList = FDirected xs

        pageWithAddedElem = CurrentPage (curElemsFwd ->. x)

        usualNextPage = pageWithAddedElem _bestPointAndCost

        usualContinue = continue usualNextPage smallerVList
  where
    continue = runPageBuilder desiredV

    addPage newPageElems vListRemaining =
        setPage desiredV newPageElems .-> continue newCurrentPage vListRemaining
