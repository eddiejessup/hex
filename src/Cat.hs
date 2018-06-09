module Cat where

import qualified Data.IntMap.Strict as IMap
import Data.List.Split (chop)
import Data.Maybe (catMaybes)

data CatCode
  = Escape
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

type CharCode = Int

data CharCat = CharCat
  { char :: CharCode
  , cat :: CatCode
  } deriving (Show)

type CharCatMap = IMap.IntMap CatCode

defaultCatCode :: CharCode -> CatCode
defaultCatCode n
  | n == 92 = Escape -- '\'
  | n == 32 = Space -- ' '
  | n == 37 = Comment -- '%'
  | n == 0 = Ignored -- Null
    -- NON-STANDARD
  | n == 10 = EndOfLine
  | n == 13 = EndOfLine
  | n == 127 = Invalid
  | n `elem` [97 .. 122] = Letter -- 'a' to 'z'.
  | n `elem` [65 .. 90] = Letter -- 'A' to 'Z'.
  | otherwise = Other

defaultCharCatMap :: CharCatMap
defaultCharCatMap = IMap.fromList $ fmap (\n -> (n, defaultCatCode n)) [0 .. 127]

catLookup :: CharCatMap -> CharCode -> CatCode
catLookup m n = IMap.findWithDefault Invalid n m

extractCharCat :: CharCatMap -> [CharCode] -> Maybe (CharCat, [CharCode])
extractCharCat _ [] = Nothing
extractCharCat ccMap (n1:n2:n3:rest)
    -- Next two characters must be identical, and have category
    -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
 = 
  let
    cat1 = catLookup ccMap n1
    n3Triod = if n3 < 64 then n3 + 64 else n3 - 64
    charCatTriod = CharCat {char = n3Triod, cat = catLookup ccMap n3Triod}
    charCatSimple = CharCat {char = n1, cat = cat1}
  in
    if (cat1 == Superscript) &&
       (n1 == n2) &&
       (catLookup ccMap n3 /= EndOfLine)
    then Just (charCatTriod, rest)
    else Just (charCatSimple, n2:n3:rest)
extractCharCat ccMap (n1:rest) = Just (CharCat {char = n1, cat = catLookup ccMap n1}, rest)

extractAll :: Cat.CharCatMap -> [Cat.CharCode] -> [CharCat]
extractAll _ [] = []
extractAll ccMap cs =
  case extractCharCat ccMap cs of
    Nothing -> []
    Just (thisCC, rest) ->
      thisCC:extractAll ccMap rest
