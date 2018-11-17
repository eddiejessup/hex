{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List.Extra

data Entry = Char String | Break
    deriving Show

type EdgeDistance = Int

data EdgeStatus
    = Acceptable EdgeDistance
    | TooFull
    | TooBare
    deriving Show

type EdgeValue = [Entry]

data Node = Root | Branch Int
    deriving Show

data InEdge = InEdge EdgeValue Node

data WithSummary a b = WithSummary {value :: a, summary :: b}
type WithDistance a = WithSummary a EdgeDistance
type WithStatus a = WithSummary a EdgeStatus

data Route = Route [EdgeValue] EdgeDistance
    deriving Show

instance Eq Route where
    (Route _ a) == (Route _ b) = a == b

instance Ord Route where
    compare (Route _ a) (Route _ b) = compare a b

judgeEdge :: EdgeValue -> EdgeStatus
judgeEdge (length -> x)
    | x < 90 = TooBare
    | x > 110 = TooFull
    | otherwise = Acceptable $ 2000 - x ^ (2 :: Int)

inputRaw :: String
inputRaw = "C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse. C Biodiesel blog  migas subway tile wayfarers vinyl before they sold out listicle mlkshk kitsch. Food truck vegan synth street art, tofu organic marfa subway tile drinking vinegar. Put a bird on it skateboard offal four loko lo-fi lumbersexual. Actually pabst selfies austin single-origin coffee, mlkshk pork belly whatever lo-fi YOLO skateboard fanny pack thundercats truffaut hammock. Williamsburg next level banh mi blog 90's. Austin next level celiac pok pok, keffiyeh williamsburg health goth brooklyn kinfolk. Coloring book palo santo everyday carry YOLO bicycle rights chartreuse."

input :: [Entry]
input = convert <$> inputRaw
    where
        convert ' ' = Break
        convert c = Char [c]

-- judgeEdge :: EdgeValue -> EdgeStatus
-- judgeEdge (length -> x)
--     | x == 1 = Acceptable 0
--     | x == 2 = Acceptable 1
--     | x == 3 = Acceptable 2
--     | x == 4 = Acceptable 0
--     | otherwise = TooFull

-- input = [Char "a1", Char "a2", Break, Char "b1", Char "b2", Break, Char "c1", Char "c2"]



withStatus :: InEdge -> WithStatus InEdge
withStatus e@(InEdge v _) = WithSummary e $ judgeEdge v

toOnlyAcceptables :: [WithStatus InEdge] -> [WithDistance InEdge]
toOnlyAcceptables ds = [WithSummary y x | (WithSummary y (Acceptable x)) <- ds]

toOnlyPromisings :: [WithStatus a] -> [WithStatus a]
toOnlyPromisings ds = [WithSummary y d | (WithSummary y d) <- ds, isPromising d]
    where
        isPromising TooFull = False
        isPromising _ = True

growInEdge :: Entry -> InEdge -> InEdge
growInEdge c (InEdge cs n) = InEdge (c : cs) n

stretchInEdge :: [Entry] -> InEdge -> InEdge
stretchInEdge s (InEdge cs n) = InEdge (s ++ cs) n

appendEntry :: ([InEdge], [Route], [Entry]) -> Entry -> ([InEdge], [Route], [Entry])
appendEntry (prevInEdges, rs, chunk) x@Break =
    -- Reached a break point.
    -- Plan:
    -- - Remove too long accumulated lines.
    -- - Insert acceptable lines, with the associated predecessor breaks, as the result for this break.
    -- - Add another line for the next break, to represent the possibility of this break being the predecessor.
    let
        -- Get the candidates for this break, by appending the chunk we just grabbed to our previous candidate edges.
        inEdges = stretchInEdge chunk <$> prevInEdges
        -- Compute edge distances, and throw out too long ones.
        promisingDInEdges = toOnlyPromisings $ withStatus <$> inEdges
        promisingInEdges = value <$> promisingDInEdges
    in
        -- Find the subset of promising edges that are acceptable right now.
        case toOnlyAcceptables promisingDInEdges of
            -- If we can't break at this point acceptably somehow, just treat
            -- like a non-break character.
            [] -> (promisingInEdges, rs, [x])
            acceptableDInEdges ->
                let
                    -- Of the promising edges, append the current character, to form the
                    -- candidates for the next break.
                    grownPromisingInEdges = growInEdge x <$> promisingInEdges
                    -- Candidate edges for the next break are:
                    -- - Already promising edges, with this break-point item appended
                    -- - A new edge to hold the line between this break and the next.
                    newInEdges = InEdge [] (Branch $ length rs) : grownPromisingInEdges
                    -- Find the best route to this node, and add it to our node
                    -- list.
                    r' = shortestRoute acceptableDInEdges rs
                in
                    (newInEdges, r':rs, [])
appendEntry (prevInEdges, rs, chunk) x =
    (prevInEdges, rs, x:chunk)

bestRoute :: [Entry] -> Route
bestRoute xs =
    let
        (prevInEdges, rs, chunk) = foldl' appendEntry ([InEdge [] Root], [], []) xs
        -- Force a final 'break' representing the end of the paragraph.
        inEdges = stretchInEdge chunk <$> prevInEdges
        acceptableDInEdges = toOnlyAcceptables $ withStatus <$> inEdges
    in shortestRoute acceptableDInEdges rs

shortestRoute :: [WithDistance InEdge] -> [Route] -> Route
shortestRoute [] _ = Route [] 0
shortestRoute ies rs = minimum (getBSR <$> ies)
    where
        getBSR (WithSummary (InEdge ev Root) ed)
            = Route [ev] ed
        getBSR (WithSummary (InEdge ev (Branch sn)) ed)
            = let
                sourceNodeIdx = length rs - sn - 1
                Route sevs sd = rs !! sourceNodeIdx
            in
                Route (ev:sevs) (ed + sd)

main :: IO ()
main = do
    let r = bestRoute input
    r `seq` return ()
    -- print r
