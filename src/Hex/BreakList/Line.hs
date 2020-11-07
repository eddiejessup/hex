module Hex.BreakList.Line where

import Data.Adjacent (Adj (..))
import qualified Data.Adjacent as Adj
import qualified Data.Sequence as Seq
import qualified Hex.Box as B
import Hex.BreakList.BreakList
import Hex.BreakList.Elem
import Hex.BreakList.Glue
import Hex.BreakList.Judge
import Hex.BreakList.Set
import Hex.Config.Parameters
import Hex.Quantity
import qualified Hex.Resolve.Token as HP
import Hexlude

newtype LineBreakError = LineBreakError Text
  deriving stock (Generic)
  deriving newtype (Show)

newtype BadnessSize = BadnessSize {unBadnessSize :: TeXInt}
  deriving newtype (Eq, Show, Num)

newtype Demerit = Demerit {unDemerit :: TeXInt}
  deriving newtype (Show, Eq, Ord, Num)

data DiscardingState
  = Discarding
  | NotDiscarding
  deriving stock (Show)

data Node = Root | Branch !Int
  deriving stock (Show)

data InEdge = InEdge
  { elems :: HList,
    src :: Node,
    discarding :: DiscardingState
  }
  deriving stock (Show)

-- Set this edge to 'discarding' so that later discardable items won't be
-- added.
newEdge :: Int -> InEdge
newEdge n = InEdge mempty (Branch n) Discarding

inEdgeCons :: InEdge -> HListElem -> InEdge
inEdgeCons e@InEdge {elems = HList elemSeq, discarding} hElem = case discarding of
  Discarding
    | isDiscardable hElem ->
      e
  _ ->
    e {elems = HList (elemSeq |> hElem), discarding = NotDiscarding}

inEdgeConcat :: Seq HListElem -> InEdge -> InEdge
inEdgeConcat xs e = foldl' inEdgeCons e xs

edgeTarget :: LenParamVal 'HP.HSize -> InEdge -> TargetLength
edgeTarget (LenParamVal dw) (InEdge hList _ _) =
  listGlueStatusConcreteTarget dw hList

withTarget :: LenParamVal 'HP.HSize -> InEdge -> (InEdge, TargetLength)
withTarget dw e = (e, edgeTarget dw e)

type SetHListArgs = (TargetLength, HList)

data Route = Route
  { routeSolution :: Seq SetHListArgs,
    routeDemerit :: Demerit
  }
  deriving stock (Show)

emptyRoute :: Route
emptyRoute = Route mempty (Demerit 0)

instance Eq Route where
  rA == rB = routeDemerit rA == routeDemerit rB

instance Ord Route where
  rA `compare` rB = routeDemerit rA `compare` routeDemerit rB

lineDemerit :: IntParamVal 'HP.LinePenalty -> BadnessSize -> BreakItem -> Demerit
lineDemerit (IntParamVal lp) b br =
  let breakDemerit = breakPenalty br ^ (2 :: Int)
      listDemerit = (lp + unBadnessSize b) ^ (2 :: Int)
   in Demerit $ breakDemerit + listDemerit

toOnlyAcceptables ::
  IntParamVal 'HP.Tolerance ->
  IntParamVal 'HP.LinePenalty ->
  BreakItem ->
  Seq (InEdge, TargetLength) ->
  Seq (InEdge, TargetLength, Demerit)
toOnlyAcceptables (IntParamVal tol) lp br = seqMapMaybe toMaybeAcceptable
  where
    toMaybeAcceptable (edge, tgt@(TargetLength st _)) = case badness st of
      InfiniteBadness ->
        Nothing
      FiniteBadness b
        | breakPenalty br > tenK || b > tol ->
          Nothing
        | otherwise ->
          Just (edge, tgt, lineDemerit lp (BadnessSize b) br)

isPromising :: LenParamVal 'HP.HSize -> InEdge -> Bool
isPromising dw e = case edgeTarget dw e of
  TargetLength (FixablyBad Overfull _) _ -> False
  _ -> True

-- The line omits the break-point item iff the break-point is glue.
finaliseInEdge :: HListElem -> InEdge -> InEdge
finaliseInEdge hElem e@InEdge {elems = HList elemSeq} = case hElem of
  HVListElem (ListGlue _) ->
    e
  _ ->
    e {elems = HList (elemSeq |> hElem)}

data BreakingState = BreakingState
  { accEdges :: Seq InEdge,
    nodeToBestRoute :: Seq Route,
    chunk :: Seq HListElem
  }
  deriving stock (Generic)

initialBreakingState :: BreakingState
initialBreakingState =
  BreakingState
    { accEdges = pure (InEdge mempty Root NotDiscarding),
      nodeToBestRoute = mempty,
      chunk = mempty
    }

-- Notation:
-- - An 'acceptable' line is one that can be realistically considered as being
--   part of an acceptable paragraph.
-- - A 'promising' line is bare enough that it is either
--   'acceptable' now, or may be acceptable in the future.
--   Conversely, a line that is unacceptable isn't acceptable now,
--   and will never become acceptable. We can ignore such edges, because we know that once a line is unacceptable, it will
--   never become acceptable.
-- In summary, lines go "promising -> promising and acceptable -> unacceptable"
appendEntry ::
  (MonadError e m, AsType LineBreakError e, MonadState BreakingState m, MonadSlog m) =>
  LenParamVal 'HP.HSize ->
  IntParamVal 'HP.Tolerance ->
  IntParamVal 'HP.LinePenalty ->
  Adj HListElem ->
  m ()
appendEntry dw tol lp x = do
  BreakingState {accEdges, nodeToBestRoute, chunk} <- get
  case toBreakItem x of
    -- If we see a non-break item, the new state is the same as the old,
    -- but with the item appended to the accumulated items 'chunk'.
    Nothing ->
      modifying' (field @"chunk") (|> Adj.fromAdjacency x)
    Just br -> do
      sLogStampedJSON
        "Appending break-item"
        [ ("item", toJSON br),
          ("chunk", toJSON chunk)
        ]
      -- Extend the accumulating edges with the normal-items 'chunk',
      -- accumulated since seeing the last break item.
      let inEdges = inEdgeConcat chunk <$> accEdges

      -- Here, just consider the in-edges 'unfinalised', i.e. without each
      -- edge's lines stripped to the form used in an actual break. This is
      -- because the edges may be passed on to later calls, in such form, to
      -- build up longer lines.
      let promisingInEdges = Seq.filter (isPromising dw) inEdges
      -- 'finalise' the promising edges to get 'candidate' edges to actually
      -- break.
      let candidateBrokenDInEdges = withTarget dw . finaliseInEdge (Adj.fromAdjacency x) <$> promisingInEdges
     -- Filter the candidates to those we could actually accept.
      case toOnlyAcceptables tol lp br candidateBrokenDInEdges of
        -- If we can't break at this point acceptably somehow, just treat
        -- the break item like a non-break item.
        Empty ->
          modify $ \bs ->
            bs
              & field @"accEdges" .~ promisingInEdges
              & field @"chunk" .~ Seq.singleton (Adj.fromAdjacency x)
        -- If some edges are acceptable, explore this fruitful avenue.
        acceptableDInEdges -> do
          -- For later calls, build up the still-promising edges with
          -- the break item added.
          let grownPromisingInEdges = (\e -> inEdgeCons e (Adj.fromAdjacency x)) <$> promisingInEdges
          -- Add an edge representing the chance to break here.
          let newInEdge = newEdge $ length nodeToBestRoute
          -- Add that new edge to the existing, now-grown, edges.
          let newAccEdges = grownPromisingInEdges |> newInEdge
          -- Find the best way to reach this break-point, given the
          -- acceptable previous lines and the known best ways to
          -- reach those lines.
          newRoute <- bestRoute acceptableDInEdges nodeToBestRoute
          -- The new state should contain:
          -- - The ongoing edges that are still promising
          -- - The optimal route to reach this break,
          --   appended to the list of node best-routes.
          -- - A new empty chunk, to accumulate the contents
          --   we see until the next break.
          put $
            BreakingState
              { accEdges = newAccEdges,
                nodeToBestRoute = nodeToBestRoute |> newRoute,
                chunk = mempty
              }

bestRoute ::
  (MonadError e m, AsType LineBreakError e) =>
  Seq (InEdge, TargetLength, Demerit) ->
  Seq Route ->
  m Route
bestRoute decoratedInEdges nodeToBestRouteSeq =
  mapM bestRouteGivenEdge decoratedInEdges
    <&> (minimumMay . toList)
    >>= note (injectTyped $ LineBreakError "No routes available")
  where
    bestRouteGivenEdge (InEdge {elems, src}, tgt, edgeDemerit) =
      routeCons (tgt, elems) edgeDemerit <$> case src of
        Root ->
          pure emptyRoute
        Branch nodeIdx ->
          note
            (injectTyped $ LineBreakError $ "No node found at index " <> show nodeIdx)
            (Seq.lookup nodeIdx nodeToBestRouteSeq)
    routeCons ln lnDemerit (Route rSoln rDemerit) =
      Route (rSoln |> ln) (rDemerit + lnDemerit)

breakAndSetParagraph ::
  (MonadError e m, AsType LineBreakError e, MonadSlog m) =>
  LenParamVal 'HP.HSize ->
  IntParamVal 'HP.Tolerance ->
  IntParamVal 'HP.LinePenalty ->
  HList ->
  m (Seq (B.Box B.HBox))
breakAndSetParagraph _ _ _ (HList Empty) =
  pure mempty
breakAndSetParagraph dw tol lp (HList (xs :|> x)) = do
  let -- Remove the final item if it's glue.
      trimmedElemSeq = case x of
        HVListElem (ListGlue _) -> xs
        _ -> xs |> x
      -- Add extra bits to finish the list.
      -- Append \penalty10k \hfil \penalty-10k.
      finishedElemSeq =
        trimmedElemSeq
          |> HVListElem (ListPenalty $ Penalty tenK)
          |> HVListElem (ListGlue filGlue)
          |> HVListElem (ListPenalty $ Penalty $ - tenK)

  BreakingState {accEdges, nodeToBestRoute, chunk} <-
    execStateT
      (forM_ (Adj.toAdjacents finishedElemSeq) (appendEntry dw tol lp))
      initialBreakingState
  let inEdges = inEdgeConcat chunk <$> accEdges
      acceptableDInEdges = toOnlyAcceptables tol lp NoBreak $ withTarget dw <$> inEdges
  setRoute <$> bestRoute acceptableDInEdges nodeToBestRoute
  where
    setRoute route =
      (\(tgt, hList) -> setHList hList (ComputedTargetLength tgt)) <$> routeSolution route
