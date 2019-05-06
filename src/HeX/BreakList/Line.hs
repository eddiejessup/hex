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
import           HeX.Unit                (tenK)

newtype BadnessSize = BadnessSize { unBadnessSize :: Int }
    deriving ( Eq, Show, Num )

newtype Demerit = Demerit { unDemerit :: Int }
    deriving ( Show, Eq, Ord, Num )

data DiscardingState
    = Discarding
    | NotDiscarding
    deriving ( Show )

type ElemAdj = Adj HListElem

data Node = Root | Branch !Int
    deriving ( Show )

data InEdge = InEdge
    { elems      :: !ForwardHList
    , src        :: !Node
    , discarding :: !DiscardingState
    }
    deriving ( Show )

-- Set this edge to 'discarding' so that later discardable items won't be
-- added.
newEdge :: Int -> InEdge
newEdge n = InEdge mempty (Branch n) Discarding

inEdgeCons :: InEdge -> HListElem -> InEdge
inEdgeCons e@InEdge{ elems, discarding } hElem =
    case discarding of
        Discarding | isDiscardable hElem ->
            e
        _ ->
            e{ elems = elems ->. hElem, discarding = NotDiscarding }

inEdgeConcat :: ForwardDirected Seq ElemAdj -> InEdge -> InEdge
inEdgeConcat xs e = foldl' inEdgeCons e (Adj.fromAdjacencies xs)

edgeStatus :: LenParamVal HSize -> InEdge -> GlueStatus
edgeStatus (LenParamVal dw) (InEdge elems _ _) =
    listGlueStatus dw elems

withStatus :: LenParamVal HSize -> InEdge -> (InEdge, GlueStatus)
withStatus dw e = (e, edgeStatus dw e)

data Route = Route
    { routeSolution :: !(ForwardDirected Seq SetHListArgs)
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
    -> f (InEdge, GlueStatus)
    -> f (InEdge, GlueStatus, Demerit)
toOnlyAcceptables (IntParamVal tol) lp br = mapMaybe toMaybeAcceptable
  where
    toMaybeAcceptable (edge, st) =
        case badness st of
            InfiniteBadness ->
                Nothing
            FiniteBadness b
                | breakPenalty br > tenK || b > tol ->
                    Nothing
                | otherwise ->
                    Just (edge, st, lineDemerit lp (BadnessSize b) br)

isPromising :: LenParamVal HSize -> InEdge -> Bool
isPromising dw e = case edgeStatus dw e of
    FixablyBad Overfull _ -> False
    _                     -> True

-- The line omits the break-point item iff the break-point is glue.
finaliseInEdge :: HListElem -> InEdge -> InEdge
finaliseInEdge hElem e@InEdge{ elems } =
    case hElem of
        HVListElem (ListGlue _) ->
            e
        _ ->
            e{ elems = elems ->. hElem }

data BreakingState =
    BreakingState { accEdges        :: !(ForwardDirected Seq InEdge)
                  , nodeToBestRoute :: !(ForwardDirected Seq Route)
                  , chunk           :: !(ForwardDirected Seq ElemAdj)
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
appendEntry dw
            tol
            lp
            st@BreakingState{ accEdges, nodeToBestRoute, chunk }
            x = case toBreakItem x of
    -- If we see a non-break item, the new state is the same as the old,
    -- but with the item appended to the accumulated items 'chunk'.
    Nothing ->
        pure st{ chunk = chunk ->. x }
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
            candidateBrokenDInEdges = withStatus dw . finaliseInEdge (adjVal x) <$> promisingInEdges
        in
            -- Filter the candidates to those we could actually accept.
            case toOnlyAcceptables tol lp br candidateBrokenDInEdges of
                -- If we can't break at this point acceptably somehow, just treat
                -- the break item like a non-break item.
                FDirected Empty ->
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
                        newAccEdges = grownPromisingInEdges ->. newInEdge
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
                            pure $ BreakingState newAccEdges (nodeToBestRoute ->. newRoute) mempty

type SetHListArgs = (GlueStatus, ForwardHList)

bestRoute :: ForwardDirected Seq (InEdge, GlueStatus, Demerit)
          -> ForwardDirected Seq Route
          -> Either Text Route
bestRoute decoratedInEdges (FDirected nodeToBestRouteSeq) =
    mapM bestRouteGivenEdge decoratedInEdges
    <&> Safe.F.minimumMay
    >>= maybeToRight "No routes available"
  where
    bestRouteGivenEdge (InEdge{ elems, src }, glueStatus_, edgeDemerit) =
        routeCons (glueStatus_, elems) edgeDemerit <$> case src of
            Root ->
                pure emptyRoute
            Branch nodeIdx ->
                seqLookupEith "route" nodeToBestRouteSeq nodeIdx

    routeCons ln lnDemerit (Route rSoln rDemerit) =
        Route (rSoln ->. ln) (rDemerit + lnDemerit)

breakAndSetParagraph
    :: LenParamVal HSize
    -> IntParamVal Tolerance
    -> IntParamVal LinePenalty
    -> ForwardHList
    -> Either Text (ForwardDirected Seq (ForwardDirected [] B.HBoxElem))
breakAndSetParagraph _ _ _ (FDirected Empty) =
    pure mempty
breakAndSetParagraph dw tol lp hList@(FDirected (xs :|> x)) =
    do
    let
        -- Remove the final item if it's glue.
        hListTrimmed = case x of
            HVListElem (ListGlue _) -> FDirected xs
            _                       -> hList
        -- Add extra bits to finish the list.
        -- Append \penalty10k \hfil \penalty-10k.
        hListFinished =
            hListTrimmed
            ->. (HVListElem $ ListPenalty $ Penalty tenK)
            ->. (HVListElem $ ListGlue filGlue)
            ->. (HVListElem $ ListPenalty $ Penalty $ -tenK)
    BreakingState{ accEdges, nodeToBestRoute, chunk } <- foldM (appendEntry dw tol lp) initialBreakingState $ Adj.toAdjacents hListFinished
    let inEdges = inEdgeConcat chunk <$> accEdges
        acceptableDInEdges = toOnlyAcceptables tol lp NoBreak $ withStatus dw <$> inEdges
    setRoute <$> bestRoute acceptableDInEdges nodeToBestRoute
  where
    setRoute route = uncurry setHList <$> routeSolution route
