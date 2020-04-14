module Hex.BreakList.Page where

import           Hexlude

import           Data.Adjacent           (Adj (..))
import qualified Data.Sequence           as Seq

import qualified Hex.Box                 as B
import           Hex.BreakList.BreakList
import           Hex.BreakList.Elem
import           Hex.BreakList.Judge
import           Hex.BreakList.Set
import           Hex.Config.Parameters
import           Hex.Quantity
import           Hex.Resolve.Token (LengthParameter(..))

data PageBreakJudgment =
    DoNotBreak | BreakPageAtBest | BreakPageHere | TrackCost !TeXInt
    deriving (Show)

pageBreakJudgment
    :: VList
    -> BreakItem
    -> LenParamVal 'VSize
    -> PageBreakJudgment
pageBreakJudgment vList breakItem (LenParamVal h) =
    let TargetLength status _ = listGlueStatusConcreteTarget h vList
    in case badness status of
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

setPage :: LenParamVal 'VSize -> VList -> B.Page
setPage (LenParamVal h) vList =
    B.Page $ setVList vList (B.To h) DefaultAlign

data CurrentPage = CurrentPage
    { items            :: VList
    , bestPointAndCost :: Maybe (TeXInt, TeXInt)
    }

newCurrentPage :: CurrentPage
newCurrentPage = CurrentPage mempty Nothing

runPageBuilder
    :: LenParamVal 'VSize
    -> CurrentPage
    -> VList
    -> Seq B.Page
runPageBuilder
    desiredV
    (CurrentPage curPageList@(VList curPageElemSeq) _bestPointAndCost)
    (VList toAddElemSeq) =
        let
            continue = runPageBuilder desiredV

            addPage newPageList remainingToAddList =
                setPage desiredV newPageList :<| continue newCurrentPage remainingToAddList
        in case toAddElemSeq of
            Empty ->
                pure (setPage desiredV curPageList)
            x :<| xs ->
                let
                    pageWithAddedElem = CurrentPage (VList (curPageElemSeq :|> x))

                    usualContinue = continue (pageWithAddedElem _bestPointAndCost) (VList xs)
                in if
                    -- If the current vlist has no boxes, we discard a discardable item.
                    -- Otherwise, if a discardable item is a legitimate breakpoint, we compute
                    -- the cost c of breaking at this point.
                    | not $ any isBox curPageElemSeq ->
                        continue
                            (CurrentPage
                                (VList $ if isDiscardable x
                                    then curPageElemSeq
                                    else curPageElemSeq :|> x)
                                _bestPointAndCost)
                            (VList xs)
                    | isDiscardable x ->
                        case toBreakItem Adj { adjPre  = seqLastMay curPageElemSeq
                                             , adjVal  = x
                                             , adjPost = seqHeadMay xs
                                             } of
                            -- If we can't break here, just add it to the list and continue.
                            Nothing  ->
                                usualContinue
                            Just brk ->
                                case (pageBreakJudgment curPageList brk desiredV, _bestPointAndCost) of
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
                                    (BreakPageAtBest, Just (TeXInt iBest, _)) ->
                                        let
                                            (newPageElemSeq, leftoverListElemSeq) = Seq.splitAt iBest curPageElemSeq
                                        in
                                            addPage (VList newPageElemSeq) (VList (leftoverListElemSeq <> toAddElemSeq))
                                    -- If p ≤ −10000, we know the best breakpoint is this one, so break
                                    -- here.
                                    (BreakPageHere, _) ->
                                        addPage curPageList (VList xs)
                                    -- If the resulting cost <= the smallest cost seen so far, remember
                                    -- the current breakpoint as the best so far.
                                    (TrackCost cHere, _) ->
                                        let thisPointAndCost = Just (TeXInt (length curPageElemSeq), cHere)
                                            newBestPointAndCost = case _bestPointAndCost of
                                                Nothing         -> thisPointAndCost
                                                Just (_, cBest) -> if cHere > cBest
                                                                   then _bestPointAndCost
                                                                   else thisPointAndCost
                                        in
                                            continue (pageWithAddedElem newBestPointAndCost) (VList xs)
                    -- If we can't break here, just add it to the list and continue.
                    | otherwise ->
                        usualContinue
