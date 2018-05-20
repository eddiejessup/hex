module Cat where

import qualified Data.Map as Map
import qualified Data.Char as C
import Data.Maybe (fromJust)

data NestedList a = Elem a | List [NestedList a] deriving (Show, Eq)

contents :: NestedList a -> [NestedList a]
contents (Elem x) = [Elem x]
contents (List x) = x

insert :: NestedList a -> Int -> NestedList a -> NestedList a
insert (List x) 0 y = List $ y:x
insert (List (x:xs)) n y = List $ x:contents (insert (List xs) (n - 1) y)

down :: NestedList a -> NestedList a
down ls = case ls of Elem a -> Elem a
                     List (x:xs) -> down x

data CatCode = Escape
             | BeginGroup
             | EndGroup
             | MathShift
             | AlignTab
             | EndOfLine
             | Parameter
             | Superscript
             | Subscript
             | Ignored
             | Space
             | Letter
             | Other
             | Active
             | Comment
             | Invalid
             deriving (Show, Eq, Enum)

data CharCat = CharCat {char :: Char, cat :: CatCode, len :: Int} deriving (Show)

defaultCatCode :: Char -> CatCode
defaultCatCode char
    | char == '\\' = Escape
    | char == ' ' = Space
    | char == '%' = Comment
    -- Null
    | C.ord char == 0 = Ignored
    -- NON-STANDARD
    | C.ord char == 10 = EndOfLine
    | C.ord char == 13 = EndOfLine
    | C.ord char == 127 = Invalid
    | char `elem` ['a'..'z'] = Letter
    | char `elem` ['A'..'Z'] = Letter
    | otherwise = Other

chrs = fmap C.chr [0..127]
defaultCharCatMap = Map.fromList $ zip chrs $ fmap Cat.defaultCatCode chrs

toCatCode :: Ord a => Map.Map a b -> a -> b
toCatCode m c = fromJust $ Map.lookup c m

extractCharCat :: (Char -> CatCode) -> [Char] -> (CharCat, [Char])
extractCharCat f (c:cs) = (CharCat {char=c, cat=f c, len=1}, cs)

extractCharCatTrio :: (Char -> CatCode) -> [Char] -> (CharCat, [Char])
extractCharCatTrio toCat (cOne:cTwo:cThree:cs) =
    -- Next two characters must be identical, and have category
    -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
    if (toCat cOne == Superscript) && (cOne == cTwo) && (toCat cThree /= EndOfLine)
        then
            let
                nThree = C.ord cThree
                cThreeTriod = C.chr (if nThree < 64 then nThree + 64 else nThree - 64)
            in (CharCat {char=cThreeTriod, cat=toCat cThreeTriod, len=3}, cs)
        else extractCharCat toCat (cOne:cTwo:cThree:cs)
extractCharCatTrio toCat cs = extractCharCat toCat cs

process :: (Char -> CatCode) -> [Char] -> [CharCat]
process _ [] = []
process f cs = gotCC : process f remainChars
    where
        (gotCC, remainChars) = extractCharCatTrio f cs
