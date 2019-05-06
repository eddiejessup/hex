module HeX.BreakList.Line where

import           HeXlude

import           Control.Applicative     ( empty )
import           Control.Monad           ( guard )
import           Data.Adjacent           ( Adj(..) )
import qualified Data.Adjacent           as Adj
import qualified Safe.Foldable           as Safe.F
import qualified Data.Sequence           as Seq

import qualified HeX.Box                 as B
import           HeX.BreakList.BreakList
import           HeX.BreakList.Elem
import           HeX.BreakList.Glue
import           HeX.BreakList.Judge
import           HeX.BreakList.Set
import           HeX.Config.Parameters
import           HeX.Unit                ( tenK )

newtype BadnessSize = BadnessSize { unBadnessSize :: Int }
    deriving ( Eq, Show, Num )

newtype Demerit = Demerit { unDemerit :: Int }
    deriving ( Show, Eq, Ord, Num )

newtype IsDiscarding = IsDiscarding Bool
    deriving ( Show )

type ElemAdj = Adj HListElem

data Node = Root | Branch !Int
    deriving ( Show )

data InEdge =
    InEdge { elems :: !(BackwardDirected Seq ElemAdj), src :: !Node, discarding :: !IsDiscarding }
    deriving ( Show )

-- Set this edge to 'discarding' so that later discardable items won't be
-- added.
newEdge :: Int -> InEdge
newEdge n = InEdge mempty (Branch n) (IsDiscarding True)

inEdgeCons :: ElemAdj -> InEdge -> InEdge
inEdgeCons c e@(InEdge cs n (IsDiscarding _discarding))
    | _discarding && isDiscardable (Adj.fromAdjacency c) =
        e
    | otherwise =
        InEdge (c .<- cs) n (IsDiscarding False)

inEdgeConcat :: Foldable f => f ElemAdj -> InEdge -> InEdge
inEdgeConcat xs e = foldr inEdgeCons e xs

edgeStatus :: LenParamVal HSize -> InEdge -> GlueStatus
edgeStatus (LenParamVal dw) (InEdge v _ _) =
    listGlueStatus dw $ Adj.fromAdjacencies v

withStatus :: LenParamVal HSize -> InEdge -> (InEdge, GlueStatus)
withStatus dw e = (e, edgeStatus dw e)

data Route = Route !(BackwardDirected Seq (ForwardDirected [] B.HBoxElem)) !Demerit
    deriving ( Show )

instance Eq Route where
    (Route _ distA) == (Route _ distB) = distA == distB

instance Ord Route where
    compare (Route _ distA) (Route _ distB) = compare distA distB

routeCons :: Route -> ForwardDirected [] B.HBoxElem -> Demerit -> Route
routeCons (Route subLns sA) ln sB = Route (ln .<- subLns) (sA + sB)

lineDemerit :: IntParamVal LinePenalty -> BadnessSize -> BreakItem -> Demerit
lineDemerit (IntParamVal lp) b br =
    let breakDemerit = breakPenalty br ^ (2 :: Int)
        listDemerit = (lp + unBadnessSize b) ^ (2 :: Int)
    in
        Demerit $ breakDemerit + listDemerit

toOnlyAcceptables
    :: IntParamVal Tolerance
    -> IntParamVal LinePenalty
    -> BreakItem
    -> Seq (InEdge, GlueStatus)
    -> Seq (InEdge, GlueStatus, Demerit)
toOnlyAcceptables (IntParamVal tol) lp br ds = do
    (edge, st) <- ds
    case badness st of
        InfiniteBadness -> empty
        FiniteBadness b -> do
            guard $ breakPenalty br < tenK && b <= tol
            let _demerit = lineDemerit lp (BadnessSize b) br
            pure (edge, st, _demerit)

isPromising :: LenParamVal HSize -> InEdge -> Bool
isPromising dw e = case edgeStatus dw e of
    FixablyBad Overfull _ -> False
    _ -> True

finaliseInEdge :: ElemAdj -> InEdge -> InEdge

-- If the break-point is at glue, then the line doesn't include that glue.
finaliseInEdge Adj{adjVal = HVListElem (ListGlue _)} e = e
-- If the break is at some other type of break, the line includes it.
finaliseInEdge x (InEdge cs n discard) = InEdge (x .<- cs) n discard

data BreakingState =
    BreakingState { accEdges        :: !(Seq InEdge)
                  , nodeToBestRoute :: !(Seq Route)
                  , chunk           :: !(Seq ElemAdj)
                  }

initialBreakingState :: BreakingState
initialBreakingState =
    BreakingState { accEdges        = pure (InEdge mempty Root (IsDiscarding False))
                  , nodeToBestRoute = mempty
                  , chunk           = mempty
                  }

-- Notation:
-- - An 'Acceptable' line is one that can be realistically considered as being
--   part of an acceptable paragraph.
-- - A 'Promising' line is one that is bare enough that it is either
--   'acceptable' now, or may be acceptable in the future. Conversely, a line
--   that is not promising is one that is so full that it isn't acceptable now,
--   and will never become acceptable. We can remove such edges from
--   consideration, because we know that once a line is not promising, it will
--   never become acceptable.
appendEntry :: LenParamVal HSize
            -> IntParamVal Tolerance
            -> IntParamVal LinePenalty
            -> BreakingState
            -> ElemAdj
            -> Either Text BreakingState
appendEntry dw
            tol
            lp
            st@BreakingState{accEdges, nodeToBestRoute, chunk}
            x = case toBreakItem x of
    -- If we see a non-break item, the new state is the same as the old,
    -- but with the item appended to the accumulated items 'chunk'.
    Nothing ->
        pure st { chunk = x <| chunk }
    Just br ->
        let
            -- Extend the accumulating edges with the normal-items 'chunk',
            -- accumulated since seeing the last break item.
            inEdges = inEdgeConcat chunk <$> accEdges
            -- Here, just consider the in-edges 'unfinalised', i.e. without each
            -- edge's lines stripped to the form used in an actual break. This is
            -- because the edges may be passed on to later calls, in such form, to
            -- build up longer lines.
            promisingInEdges = filter (isPromising dw) inEdges
            -- 'finalise' the promising edges to get 'candidate' edges to actually
            -- break.
            candidateBrokenDInEdges =
                withStatus dw . finaliseInEdge x <$> promisingInEdges
        in
            -- Filter the candidates to those we could actually accept.
            case toOnlyAcceptables tol lp br candidateBrokenDInEdges of
                -- If we can't break at this point acceptably somehow, just treat
                -- the break item like a non-break item.
                Empty ->
                    pure $ BreakingState promisingInEdges nodeToBestRoute (pure x)
                -- If some edges are acceptable, explore this fruitful avenue.
                acceptableDInEdges ->
                    let
                        -- For later calls, build up the still-promising edges with
                        -- the break item added.
                        grownPromisingInEdges =
                            inEdgeCons x <$> promisingInEdges
                        -- Add an edge representing the chance to break here.
                        newInEdge = newEdge $ length nodeToBestRoute
                        -- Add that new edge to the existing, now-grown, edges.
                        newAccEdges = newInEdge <| grownPromisingInEdges
                    in
                        -- Find the best way to reach this break-point, given the
                        -- acceptable previous lines and the known best ways to
                        -- reach those lines.
                        do
                            newRoute <- shortestRoute acceptableDInEdges nodeToBestRoute
                            -- The new state should contain:
                            -- - The ongoing edges that are still promising
                            -- - The optimal route to reach this break,
                            --   appended to the list of node best-routes.
                            -- - A new empty chunk, to accumulate the contents
                            --   we see until the next break.
                            pure $ BreakingState newAccEdges (newRoute <| nodeToBestRoute) mempty

shortestRoute :: Seq (InEdge, GlueStatus, Demerit)
              -> Seq Route
              -> Either Text Route
shortestRoute es rs = do
    subRs <- mapM bestSubroute es
    pure $ Safe.F.minimumDef (Route mempty (Demerit 0)) subRs
  where
    bestSubroute (InEdge ev n _, st, ed) =
        let
            boxes = setHList st (revBackwardSeq $ Adj.fromAdjacencies ev)
        in
            case n of
                Root ->
                    pure $ Route (BDirected (Seq.singleton boxes)) ed
                Branch sn -> do
                    subR <- seqLookupEith "route" rs (length rs - sn - 1)
                    pure $ routeCons subR boxes ed

breakAndSetParagraph
    :: LenParamVal HSize
    -> IntParamVal Tolerance
    -> IntParamVal LinePenalty
    -> BackwardHList
    -> Either Text (ForwardDirected Seq (ForwardDirected [] B.HBoxElem))
breakAndSetParagraph _ _ _ (BDirected Empty) =
    pure mempty
breakAndSetParagraph dw tol lp (BDirected (x :<| xs)) =
    let
        -- Remove the final item if it's glue.
        xsTrimmed = case x of
            HVListElem (ListGlue _) -> xs
            _ -> x <| xs
        -- Add extra bits to finish the list.
        -- Append \penalty10k \hfil \penalty-10k.
        xsFinished =
            (HVListElem $ ListPenalty $ Penalty $ -tenK)
         <| (HVListElem $ ListGlue filGlue)
         <| (HVListElem $ ListPenalty $ Penalty tenK)
         <| xsTrimmed
    in
        go (Seq.reverse xsFinished)
  where
    go _xs = do
        BreakingState{accEdges, nodeToBestRoute, chunk}
            <- foldM (appendEntry dw tol lp) initialBreakingState $
            Adj.toAdjacents _xs
        let inEdges = inEdgeConcat chunk <$> accEdges
            acceptableDInEdges =
                toOnlyAcceptables tol lp NoBreak $ withStatus dw <$> inEdges
        (Route rawLns _) <- shortestRoute acceptableDInEdges nodeToBestRoute
        pure $ revBackwardSeq rawLns
