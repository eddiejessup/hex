{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Box where

import qualified DVI.Write as DVIW
import Data.List (intersperse, minimumBy, elemIndices)
import Data.Maybe (isNothing)
import qualified Data.List.Split as LS
import qualified Parse as P
import Data.Ratio ((%))
import qualified Debug.Trace as T

data SetGlue = SetGlue
  { dimen :: Int
  } deriving (Show)

data HBoxElement
  = HVBox [VBoxElement]
  | HHBox [HBoxElement]
  | HRule P.Rule
  | HGlue SetGlue
  | HKern P.Kern
  | HFontDefinition P.FontDefinition
  | HFontSelection P.FontSelection
  | HCharacter P.Character
  deriving (Show)

data VBoxElement
  = VVBox [VBoxElement]
  | VHBox [HBoxElement]
  | VRule P.Rule
  | VGlue SetGlue
  | VKern P.Kern
  | VFontDefinition P.FontDefinition
  | VFontSelection P.FontSelection
  deriving (Show)

class DVIAble a where
  toDVI :: a -> [DVIW.Instruction]

instance DVIAble [VBoxElement] where
  toDVI a = concatMap toDVI a

instance DVIAble [HBoxElement] where
  toDVI a = concatMap toDVI a

-- TODO: Rule.

instance DVIAble SetGlue where
  toDVI SetGlue {} = []

instance DVIAble P.Kern where
  toDVI P.Kern {} = []

instance DVIAble P.FontDefinition where
  toDVI P.FontDefinition { fontNr = fNr
                         , fontPath = path
                         , fontName = name
                         , fontInfo = info
                         , scaleFactorRatio = scale
                         }
   = [ DVIW.DefineFont
        { fontInfo = info
        , fontPath = name
        , fontNr = fNr
        , scaleFactorRatio = scale
        }
      ]

instance DVIAble P.FontSelection where
  toDVI P.FontSelection {fontNr = fNr} = [DVIW.SelectFont fNr]

instance DVIAble P.Character where
  toDVI P.Character {code = c} = [DVIW.Character {charNr = c, move = True}]

instance DVIAble HBoxElement
    -- TODO: Move by amount dependent on contents.
                                                   where
  toDVI (HVBox e) = [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveRight {distance = 200000}]
    -- TODO: Move by amount dependent on contents.
  toDVI (HHBox e) = [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveRight {distance = 200000}]
    -- TODO: Rule.
  toDVI (HGlue SetGlue {dimen = d}) = [DVIW.MoveRight {distance = d}]
  toDVI (HKern P.Kern {dimen = d}) = [DVIW.MoveRight {distance = d}]
  toDVI (HFontDefinition e@P.FontDefinition {}) = toDVI e
  toDVI (HFontSelection e@P.FontSelection {}) = toDVI e
  toDVI (HCharacter e@P.Character {}) = toDVI e

instance DVIAble VBoxElement
    -- TODO: Move by amount dependent on contents.
                                                   where
  toDVI (VVBox e) = [DVIW.BeginNewPage, DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveDown {distance = 200000}]
    -- TODO: Move by amount dependent on contents.
  toDVI (VHBox e) = [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveDown {distance = 200000}]
    -- TODO: Rule.
  toDVI (VGlue SetGlue {dimen = d}) = [DVIW.MoveDown {distance = d}]
  toDVI (VKern P.Kern {dimen = d}) = [DVIW.MoveDown {distance = d}]
  toDVI (VFontDefinition e) = toDVI e
  toDVI (VFontSelection e) = toDVI e

setHList :: P.HListElement -> [HBoxElement]
-- Vertical material inside a Horizontal list.
setHList (P.HVList P.VList {contents = cs}) = [HVBox $ concatMap setVList cs]
-- Horizontal material inside a Horizontal list. Like a fixed HBox.
setHList (P.HHList P.HList {contents = cs}) = [HHBox $ concatMap setHList cs]
setHList (P.HGlue P.Glue {dimen = d}) = [HGlue $ SetGlue d]
setHList (P.HPenalty _) = []
setHList (P.HRule a) = [HRule a]
setHList (P.HKern a) = [HKern a]
setHList (P.HFontDefinition a) = [HFontDefinition a]
setHList (P.HFontSelection a) = [HFontSelection a]
setHList (P.HCharacter a) = [HCharacter a]

data Adjacency a = Adjacency (Maybe a, a, Maybe a) deriving Show

toAdjacents :: Maybe a -> [a] -> [Adjacency a]
toAdjacents _ [] = []
toAdjacents before [this] = [Adjacency (before, this, Nothing)]
toAdjacents before (this:after:rest) = Adjacency (before, this, Just after):toAdjacents (Just this) (after:rest)

fromAdjacent :: Adjacency a -> a
fromAdjacent (Adjacency (_, a, _)) = a

fromAdjacents :: [Adjacency a] -> [a]
fromAdjacents = fmap fromAdjacent

data BreakItem = GlueBreak P.Glue
                | KernBreak P.Kern
                | PenaltyBreak P.Penalty
                | NoBreak
                deriving Show

data Break = Break { before :: [P.HListElement],
                     after :: [P.HListElement],
                     item :: BreakItem }
           deriving Show

toBreakItem :: Adjacency P.HListElement -> Maybe BreakItem
  -- TODO: Add math formula conditions.
toBreakItem (Adjacency a) = case a of
  (Just x, P.HGlue g, _) -> if P.isDiscardable x then Nothing else Just $ GlueBreak g
  (_, P.HKern k, Just P.HGlue{}) -> Just $ KernBreak k
  (_, P.HPenalty p, _) -> Just $ PenaltyBreak p
  -- TODO: Discretionary break and Math-off.
  _ -> Nothing

isBreakItem :: Adjacency P.HListElement -> Bool
isBreakItem = isNothing . toBreakItem

breakPenalty :: BreakItem -> Int
breakPenalty (GlueBreak _) = 0
breakPenalty (KernBreak _) = 0
breakPenalty NoBreak = 0
breakPenalty (PenaltyBreak (P.Penalty p)) = p

trimBeforeBreak :: [P.HListElement] -> P.HListElement -> [P.HListElement]
trimBeforeBreak before b = case b of
  -- Include the break item in the pre-break list, unless it is glue.
  P.HGlue _ -> before
  x -> before ++ [x]

trimAfterBreak :: [Adjacency P.HListElement] -> [P.HListElement]
trimAfterBreak = fromAdjacents .
  dropWhile
    (\ adj ->
     P.isDiscardable (fromAdjacent adj) && not (isBreakItem adj))

allBreaksInner :: [Break] -> [P.HListElement] -> [Adjacency P.HListElement] -> [Break]
allBreaksInner breaksAccum seen [] = Break{before=seen, after=[], item=NoBreak}:breaksAccum
allBreaksInner breaksAccum seen ((thisAdj@(Adjacency (_, this, _))):rest) =
  let
    newBreaksAccum = case toBreakItem thisAdj of
      Nothing -> breaksAccum
      Just b -> Break{before=trimBeforeBreak seen this, after=trimAfterBreak rest, item=b}:breaksAccum
  in
    allBreaksInner newBreaksAccum (seen ++ [this]) rest

allBreaks :: [P.HListElement] -> [Break]
allBreaks cs = allBreaksInner [] [] (toAdjacents Nothing cs)

class Dimensioned a where
  naturalWidth :: a -> Int
  stretch :: a -> Int
  shrink :: a -> Int

instance Dimensioned [P.VListElement] where
  naturalWidth cs = maximum $ fmap naturalWidth cs
  stretch cs = sum $ fmap stretch cs
  shrink cs = sum $ fmap shrink cs

instance Dimensioned [P.HListElement] where
  naturalWidth cs = sum $ fmap naturalWidth cs
  stretch cs = sum $ fmap stretch cs
  shrink cs = sum $ fmap shrink cs

instance Dimensioned P.HListElement where
  naturalWidth (P.HVList P.VList{contents=cs}) = naturalWidth cs
  naturalWidth (P.HHList P.HList{contents=cs}) = naturalWidth cs
  naturalWidth (P.HRule P.Rule{width=w}) = w
  naturalWidth (P.HGlue P.Glue{dimen=d}) = d
  naturalWidth (P.HKern P.Kern{dimen=d}) = d
  naturalWidth (P.HPenalty _) = 0
  naturalWidth (P.HFontDefinition _) = 0
  naturalWidth (P.HFontSelection _) = 0
  naturalWidth (P.HCharacter P.Character{width=w}) = w

  stretch a = case a of
    (P.HGlue P.Glue{stretch=s}) -> s
    _ -> 0

  shrink a = case a of
    (P.HGlue P.Glue{shrink=s}) -> s
    _ -> 0

instance Dimensioned P.VListElement where
  naturalWidth (P.VVList P.VList{contents=cs}) = naturalWidth cs
  naturalWidth (P.VHList P.HList{contents=cs}) = naturalWidth cs
  naturalWidth (P.VRule P.Rule{width=w}) = w
  naturalWidth (P.VGlue _) = 0
  naturalWidth (P.VKern _) = 0
  naturalWidth (P.VPenalty _) = 0
  naturalWidth (P.VFontDefinition _) = 0
  naturalWidth (P.VFontSelection _) = 0

  stretch a = case a of
    (P.VGlue P.Glue{stretch=s}) -> s
    _ -> 0

  shrink a = case a of
    (P.VGlue P.Glue{shrink=s}) -> s
    _ -> 0

data GlueRatio = FiniteGlueRatio Double | InfiniteGlueRatio deriving Show
data LineStatus = NaturallyGood | TooFull GlueRatio | TooBare GlueRatio | OverFull deriving Show

glueSetRatio :: Int -> Int -> Int -> LineStatus
glueSetRatio excessLength stretch shrink =
  case compare excessLength 0 of
    EQ -> NaturallyGood
    LT -> TooBare $ case stretch of
      0 -> InfiniteGlueRatio
      str -> FiniteGlueRatio $ -((fromIntegral excessLength) / (fromIntegral str))
    GT -> case shrink of
      0 -> TooFull InfiniteGlueRatio
      shr -> case compare excessLength shr of
        GT -> OverFull
        _ -> TooFull $ FiniteGlueRatio $ (fromIntegral excessLength) / (fromIntegral shr)

badness :: LineStatus -> Int
badness g = case g of
  -- TODO: This should actually be infinity, not 1 million.
  OverFull -> 1000000
  NaturallyGood -> 0
  TooFull (FiniteGlueRatio r) -> min 10000 $ round $ (r^3) * 100
  TooBare (FiniteGlueRatio r) -> min 10000 $ round $ (r^3) * 100
  TooFull InfiniteGlueRatio -> 10000
  TooBare InfiniteGlueRatio -> 10000

lineGlueSetRatio :: Int -> [P.HListElement] -> LineStatus
lineGlueSetRatio desiredWidth cs = glueSetRatio (naturalWidth cs - desiredWidth) (stretch cs) (shrink cs)

lineBadness :: Int -> [P.HListElement] -> Int
lineBadness desiredWidth cs = badness $ lineGlueSetRatio desiredWidth cs

isConsiderableAsLine :: Int -> Int -> Break -> Bool
isConsiderableAsLine tolerance desiredWidth Break{before=bef, item=br} = (breakPenalty br < 10000) && (lineBadness desiredWidth bef <= tolerance)

data Line = Line { contents :: [P.HListElement],
                   breakItem :: BreakItem }
           deriving Show

lineDemerit :: Int -> Int -> Line -> Int
lineDemerit linePenalty desiredWidth Line{contents=cs, breakItem=br} = (breakPenalty br)^2 + (linePenalty + (lineBadness desiredWidth cs))^2

bestRoute :: Int -> Int -> Int -> [P.HListElement] -> [Line]
bestRoute _ _ _ [] = []
bestRoute desiredWidth tolerance linePenalty cs =
  let
    csTrimmed = case last cs of
      P.HGlue _ -> init cs
      _ -> cs

    finisher = [P.HPenalty $ P.Penalty 10000, P.HGlue P.Glue{dimen=0, stretch=0, shrink=100000}, P.HPenalty $ P.Penalty $ -10000]
    csFinished = cs ++ finisher
    breaks = allBreaks cs
    badnesses = fmap (\Break{before=bef, after=aft, item=br} -> lineBadness desiredWidth bef) breaks
    ratios = fmap (\Break{before=bef, after=aft, item=br} -> lineGlueSetRatio desiredWidth bef) breaks
    considerables = fmap (isConsiderableAsLine tolerance desiredWidth) breaks
    mmm = zip3 badnesses ratios considerables

    -- considerableBreaks = T.traceShow mmm $ filter (isConsiderableAsLine tolerance desiredWidth) breaks
    considerableBreaks = filter (isConsiderableAsLine tolerance desiredWidth) breaks
    subBestRoute Break{before=bef, after=aft, item=br} = Line{contents=bef, breakItem=br}:bestRoute desiredWidth tolerance linePenalty aft
    bestConsiderableRoutes = fmap subBestRoute considerableBreaks

    routeDemerit = sum . fmap (lineDemerit linePenalty desiredWidth)
    compareRoutes a b = compare (routeDemerit a) (routeDemerit b)

    best = minimumBy compareRoutes $ [Line{contents=cs, breakItem=NoBreak}]:bestConsiderableRoutes
  in best

simpleSet :: [P.HListElement] -> [HBoxElement]
simpleSet = concatMap setHList

superRoute :: Int -> Int -> Int -> [P.HListElement] -> [VBoxElement]
superRoute _ _ _ [] = []
superRoute desiredWidth tolerance linePenalty cs =
  let breaks = allBreaks cs
      breakRoutes = fmap (\Break{before=bef, after=aft, item=br} -> [VHBox $ concatMap setHList bef, VGlue $ SetGlue 400000, VHBox $ concatMap setHList aft]) breaks
      spacedRoutes = intersperse [VGlue SetGlue {dimen = 1600000}] breakRoutes
  in concat spacedRoutes

setVList :: P.VListElement -> [VBoxElement]
-- Vertical material inside a Vertical list.
setVList (P.VVList P.VList {contents = cs}) = [VVBox $ concatMap setVList cs]
-- Horizontal material inside a Vertical list. Like a paragraph.
setVList (P.VHList P.HList {contents = cs}) =
  let width = 28000000
      tolerance = 200
      linePenalty = 10
      bestRouteLists = contents <$> bestRoute width tolerance linePenalty cs
      bestRouteBoxes = fmap (VHBox . simpleSet) bestRouteLists
      route = intersperse (VGlue SetGlue {dimen = 400000}) bestRouteBoxes
      -- route = superRoute width tolerance linePenalty cs
  in route
setVList (P.VGlue P.Glue {dimen = d}) = [VGlue $ SetGlue d]
setVList (P.VPenalty _) = []
setVList (P.VRule a) = [VRule a]
setVList (P.VKern a) = [VKern a]
setVList (P.VFontDefinition a) = [VFontDefinition a]
setVList (P.VFontSelection a) = [VFontSelection a]
