module HeX.BreakList.Line where

import           HeXlude

import           Data.Adjacent           (Adj (..))
import qualified Data.Adjacent           as Adj
import qualified Safe.Foldable           as Safe.F

import qualified HeX.Box                 as B
import           HeX.BreakList.BreakList
import           HeX.BreakList.Elem
import           HeX.BreakList.Glue
import           HeX.BreakList.Judge
import           HeX.BreakList.Set
import           HeX.Config.Parameters
import           HeX.Quantity

newtype BadnessSize = BadnessSize { unBadnessSize :: TeXInt }
    deriving ( Eq, Show, Num )

newtype Demerit = Demerit { unDemerit :: TeXInt }
    deriving ( Show, Eq, Ord, Num )

data DiscardingState
    = Discarding
    | NotDiscarding
    deriving ( Show )

type ElemAdj = Adj HListElem

data Node = Root | Branch !Int
    deriving ( Show )

data InEdge = InEdge
    { elems      :: !HList
    , src        :: !Node
    , discarding :: !DiscardingState
    }
    deriving ( Show )

-- Set this edge to 'discarding' so that later discardable items won't be
-- added.
newEdge :: Int -> InEdge
newEdge n = InEdge mempty (Branch n) Discarding

inEdgeCons :: InEdge -> HListElem -> InEdge
inEdgeCons e@InEdge{ elems = HList elemSeq, discarding } hElem =
    case discarding of
        Discarding | isDiscardable hElem ->
            e
        _ ->
            e { elems = HList (elemSeq :|> hElem) , discarding = NotDiscarding }

inEdgeConcat :: Seq ElemAdj -> InEdge -> InEdge
inEdgeConcat xs e = foldl' inEdgeCons e (Adj.fromAdjacencies xs)

edgeTarget :: LenParamVal HSize -> InEdge -> TargetLength
edgeTarget (LenParamVal dw) (InEdge hList _ _) =
    listGlueStatusConcreteTarget dw hList

withTarget :: LenParamVal HSize -> InEdge -> (InEdge, TargetLength)
withTarget dw e = (e, edgeTarget dw e)

type SetHListArgs = (TargetLength, HList)

data Route = Route
    { routeSolution :: !(Seq SetHListArgs)
    , routeDemerit  :: !Demerit
    }
    deriving ( Show )

emptyRoute :: Route
emptyRoute = Route mempty (Demerit 0)

instance Eq Route where
    rA == rB = routeDemerit rA == routeDemerit rB

instance Ord Route where
    rA `compare` rB = routeDemerit rA `compare` routeDemerit rB

lineDemerit :: IntParamVal LinePenalty -> BadnessSize -> BreakItem -> Demerit
lineDemerit (IntParamVal lp) b br =
    let breakDemerit = breakPenalty br ^ (2 :: Int)
        listDemerit = (lp + unBadnessSize b) ^ (2 :: Int)
    in
        Demerit $ breakDemerit + listDemerit

toOnlyAcceptables
    :: Filterable f
    => IntParamVal Tolerance
    -> IntParamVal LinePenalty
    -> BreakItem
    -> f (InEdge, TargetLength)
    -> f (InEdge, TargetLength, Demerit)
toOnlyAcceptables (IntParamVal tol) lp br = mapMaybe toMaybeAcceptable
  where
    toMaybeAcceptable (edge, tgt@(TargetLength st _)) =
        case badness st of
            InfiniteBadness ->
                Nothing
            FiniteBadness b
                | breakPenalty br > tenK || b > tol ->
                    Nothing
                | otherwise ->
                    Just (edge, tgt, lineDemerit lp (BadnessSize b) br)

isPromising :: LenParamVal HSize -> InEdge -> Bool
isPromising dw e = case edgeTarget dw e of
    TargetLength (FixablyBad Overfull _) _ -> False
    _                                      -> True

-- The line omits the break-point item iff the break-point is glue.
finaliseInEdge :: HListElem -> InEdge -> InEdge
finaliseInEdge hElem e@InEdge{ elems = HList elemSeq } =
    case hElem of
        HVListElem (ListGlue _) ->
            e
        _ ->
            e{ elems = HList (elemSeq :|> hElem) }

data BreakingState =
    BreakingState { accEdges        :: !(Seq InEdge)
                  , nodeToBestRoute :: !(Seq Route)
                  , chunk           :: !(Seq ElemAdj)
                  }

initialBreakingState :: BreakingState
initialBreakingState =
    BreakingState { accEdges        = pure (InEdge mempty Root NotDiscarding)
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
appendEntry dw tol lp st@BreakingState{ accEdges, nodeToBestRoute, chunk } x =
    case toBreakItem x of
        -- If we see a non-break item, the new state is the same as the old,
        -- but with the item appended to the accumulated items 'chunk'.
        Nothing ->
            pure st{ chunk = chunk :|> x }
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
                candidateBrokenDInEdges = withTarget dw . finaliseInEdge (adjVal x) <$> promisingInEdges
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
                            grownPromisingInEdges = (\e -> inEdgeCons e (adjVal x)) <$> promisingInEdges
                            -- Add an edge representing the chance to break here.
                            newInEdge = newEdge $ length nodeToBestRoute
                            -- Add that new edge to the existing, now-grown, edges.
                            newAccEdges = grownPromisingInEdges :|> newInEdge
                        in
                            -- Find the best way to reach this break-point, given the
                            -- acceptable previous lines and the known best ways to
                            -- reach those lines.
                            do
                                newRoute <- bestRoute acceptableDInEdges nodeToBestRoute
                                -- The new state should contain:
                                -- - The ongoing edges that are still promising
                                -- - The optimal route to reach this break,
                                --   appended to the list of node best-routes.
                                -- - A new empty chunk, to accumulate the contents
                                --   we see until the next break.
                                pure $ BreakingState newAccEdges (nodeToBestRoute :|> newRoute) mempty

bestRoute :: Seq (InEdge, TargetLength, Demerit)
          -> Seq Route
          -> Either Text Route
bestRoute decoratedInEdges nodeToBestRouteSeq =
    mapM bestRouteGivenEdge decoratedInEdges
    <&> Safe.F.minimumMay
    >>= maybeToRight "No routes available"
  where
    bestRouteGivenEdge (InEdge{ elems, src }, tgt, edgeDemerit) =
        routeCons (tgt, elems) edgeDemerit <$> case src of
            Root ->
                pure emptyRoute
            Branch nodeIdx ->
                seqLookupEith "route" nodeToBestRouteSeq nodeIdx

    routeCons ln lnDemerit (Route rSoln rDemerit) =
        Route (rSoln :|> ln) (rDemerit + lnDemerit)

breakAndSetParagraph
    :: LenParamVal HSize
    -> IntParamVal Tolerance
    -> IntParamVal LinePenalty
    -> HList
    -> Either Text (Seq (B.Box B.HBox))
breakAndSetParagraph _ _ _ (HList Empty) =
    pure mempty
breakAndSetParagraph dw tol lp (HList (xs :|> x)) =
    do
    let
        -- Remove the final item if it's glue.
        trimmedElemSeq = case x of
            HVListElem (ListGlue _) -> xs
            _                       -> xs :|> x
        -- Add extra bits to finish the list.
        -- Append \penalty10k \hfil \penalty-10k.
        finishedElemSeq =
            trimmedElemSeq
            :|> (HVListElem $ ListPenalty $ Penalty tenK)
            :|> (HVListElem $ ListGlue filGlue)
            :|> (HVListElem $ ListPenalty $ Penalty $ -tenK)
    BreakingState{ accEdges, nodeToBestRoute, chunk } <-
        foldM (appendEntry dw tol lp) initialBreakingState $ Adj.toAdjacents finishedElemSeq
    let inEdges = inEdgeConcat chunk <$> accEdges
        acceptableDInEdges = toOnlyAcceptables tol lp NoBreak $ withTarget dw <$> inEdges
    setRoute <$> bestRoute acceptableDInEdges nodeToBestRoute
  where
    setRoute route =
        (\(tgt, hList) -> setHList hList (ComputedTargetLength tgt)) <$> routeSolution route
