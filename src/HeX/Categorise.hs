module HeX.Categorise where

import           HeXlude

import           Data.Foldable                  ( foldl' )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Text

import qualified Data.HashMap.Strict           as HMap

data CatCode
    = Escape       -- 0
    | BeginGroup   -- 1
    | EndGroup     -- 2
    | MathShift    -- 3
    | AlignTab     -- 4
    | EndOfLine    -- 5
    | Parameter    -- 6
    | Superscript  -- 7
    | Subscript    -- 8
    | Ignored      -- 9
    | Space        -- 10
    | Letter       -- 11
    | Other        -- 12
    | Active       -- 13
    | Comment      -- 14
    | Invalid      -- 15
    deriving (Show, Eq, Enum, Bounded)

type CharCode = Char

data CharCat = CharCat
    { char :: CharCode
    , cat  :: CatCode
    } deriving (Show)

instance Readable CharCat where
  describe (CharCat c ct) = showT ct <> " '" <> Text.singleton c <> "'"

type CharCodeMap v = HMap.HashMap CharCode v

type CatCodes = CharCodeMap CatCode

newCatCode :: CharCode -> CatCode
newCatCode c | c == '\\'             = Escape
             | c == ' '              = Space
             | c == '%'              = Comment
             | c == toEnum 0         = Ignored  -- Null.
             | c == '\n'             = EndOfLine  -- Non-Standard.
             | c == '\r'             = EndOfLine
             | c == toEnum 127       = Invalid
             | c `elem` ['a' .. 'z'] = Letter
             | c `elem` ['A' .. 'Z'] = Letter
             | otherwise             = Other

newCatCodes :: CatCodes
newCatCodes =
  HMap.fromList $ fmap (\n -> (toEnum n, newCatCode $ toEnum n)) [0 .. 127]

-- Add in some useful extras beyond the technical defaults.
extras :: [(CharCode, CatCode)]
extras =
  [('^', Superscript), ('{', BeginGroup), ('}', EndGroup), ('#', Parameter)]

usableCatCodes :: CatCodes
usableCatCodes = foldl' (\m (k, v) -> HMap.insert k v m) newCatCodes extras

catDefault :: Maybe CatCode -> CatCode
catDefault = fromMaybe Invalid

catLookup :: CatCodes -> CharCode -> CatCode
catLookup m n = catDefault $ HMap.lookup n m

extractCharCat
  :: (CharCode -> CatCode) -> ForwardDirected [] CharCode -> Maybe (CharCat, ForwardDirected [] CharCode)
extractCharCat _ (FDirected []) =
    Nothing
extractCharCat charToCat (FDirected (n1 : n2 : n3 : rest)) =
    -- Next two characters must be identical, and have category
    -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
    let cat1    = charToCat n1
        n3Triod = toEnum $ fromEnum n3 + if fromEnum n3 < 64 then 64 else (-64)
    in
        Just $ if (cat1 == Superscript) && (n1 == n2) && (charToCat n3 /= EndOfLine)
            then (CharCat n3Triod $ charToCat n3Triod, FDirected rest)
            else (CharCat n1 cat1, FDirected (n2 : n3 : rest))
extractCharCat charToCat (FDirected (n1 : rest)) =
    Just (CharCat n1 $ charToCat n1, FDirected rest)
