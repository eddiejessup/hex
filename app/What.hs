{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Hashable
import Data.List.Extra
import           Data.Vector ((!))
import qualified Data.Vector as V

import Debug.Trace

data Entry = Char Char | Break
    deriving (Show, Generic)

instance Hashable Entry

type FiniteEdgeDistance = Int

data EdgeDistance
    = Acceptable FiniteEdgeDistance
    | TooFull
    | TooBare
    deriving Show

type EdgeValue = [Entry]

data Node = Root | Branch Int
    deriving Show

type InEdge = (EdgeValue, Node)
type DInEdge = (InEdge, Int)
type DInEdgeList = [DInEdge]
type Graph = [DInEdgeList]
type VGraph = V.Vector DInEdgeList

data Route = Route [EdgeValue] FiniteEdgeDistance
    deriving Show

instance Eq Route where
    (Route _ a) == (Route _ b) = a == b

instance Ord Route where
    compare (Route _ a) (Route _ b) = compare a b

edgeDistance :: EdgeValue -> EdgeDistance
edgeDistance (length -> x)
    | x < 90 = TooBare
    | x > 110 = TooFull
    | otherwise = Acceptable $ 2000 - x ^ (2 :: Int)

-- edgeDistance :: EdgeValue -> EdgeDistance
-- edgeDistance (length -> x)
--     | x == 1 = TooBare
--     | x == 2 = Acceptable 1
--     | x == 3 = Acceptable 2
--     | otherwise = TooFull

withDistance :: InEdge -> (InEdge, EdgeDistance)
withDistance e@(v, _) = (e,  edgeDistance v)

inputRaw :: String
inputRaw = "C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse."

input :: [Entry]
input = convert <$> inputRaw
    where
        convert ' ' = Break
        convert c = Char c

-- input = [Char 'a', Char 'a', Break, Char 'b', Char 'b', Break, Char 'c', Char 'c']

toOnlyAcceptables :: [(a, EdgeDistance)] -> [(a, FiniteEdgeDistance)]
toOnlyAcceptables ds = [(y, x) | (y, Acceptable x) <- ds]

toOnlyPromisings :: [(a, EdgeDistance)] -> [(a, EdgeDistance)]
toOnlyPromisings ds = [(y, d) | (y, d) <- ds, isPromising d]
    where
        isPromising TooFull = False
        isPromising _ = True

growInEdge :: a -> ([a], b) -> ([a], b)
growInEdge x (cs, n) = (x:cs, n)

appendEntry :: ([InEdge], [Route]) -> Entry -> ([InEdge], [Route])
appendEntry (inEdges, rs) x@Break =
    -- Reached a break point.
    -- Plan:
    -- - Remove too long accumulated lines.
    -- - Insert acceptable lines, with the associated predecessor breaks, as the result for this break.
    -- - Add another line for the next break, to represent the possibility of this break being the predecessor.
    let
        -- Compute edge distances, and throw out too long ones.
        promisingDInEdges = toOnlyPromisings $ withDistance <$> inEdges
        -- Of these promising edges, append the current character, to form the
        -- candidates for the next break.
        grownPromisingInEdges = growInEdge x . fst <$> promisingDInEdges
    in
        -- Find the subset of promising edges that are acceptable right now.
        case toOnlyAcceptables promisingDInEdges of
            -- If we can't break at this point acceptably somehow, just treat
            -- like a non-break character.
            [] -> (grownPromisingInEdges, rs)
            acceptableDInEdges ->
                let
                    -- Candidate edges for the next break are:
                    -- - Already promising edges, with this break-point item appended
                    -- - A new edge to hold the line between this break and the next.
                    newInEdges = ([], Branch $ length rs) : grownPromisingInEdges
                    -- Find the best route to this node, and add it to our node
                    -- list.
                    r' = shortestRoute' acceptableDInEdges rs
                in
                    (newInEdges, r':rs)
appendEntry (inEdges, rs) x =
    (growInEdge x <$> inEdges, rs)

bestRoute :: [Entry] -> Route
bestRoute xs =
    let
        (inEdges, rs) = foldl' appendEntry ([([], Root)], []) xs
        -- Force a final 'break' representing the end of the paragraph.
        acceptableDInEdges = toOnlyAcceptables $ withDistance <$> inEdges
    in shortestRoute' acceptableDInEdges rs

shortestRoute' :: [DInEdge] -> [Route] -> Route
shortestRoute' ies rs =
    case getBSR <$> ies of
        [] -> Route [] 0
        sths -> minimum sths
    where
        getBSR ((ev, Root), ed)
            = Route [ev] ed
        getBSR ((ev, Branch sn), ed)
            = let
                sourceNodeIdx = length rs - sn - 1
                Route sevs sd = rs !! sourceNodeIdx
            in
                Route (ev:sevs) (ed + sd)

showOptions :: (Int, [(InEdge, Int)]) -> String
showOptions (n, xs) = show n ++ "\n" ++ intercalate "\n" line
    where
        line = ("\t" ++) . showEdge <$> xs
        showEdge ((_, sn), ed) = show (sn, ed)

main :: IO ()
main = do
    let r = bestRoute input
    -- putStrLn $ intercalate "\n\n" $ showOptions <$> zip (reverse [0 .. length g - 1]) g

    -- putStrLn "\n"

    r `seq` return ()
    -- print r
