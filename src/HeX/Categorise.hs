module HeX.Categorise where

import           HeXlude

import           Data.Foldable       (foldl')
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as Text

import qualified Data.HashMap.Strict as HMap

-- Not all Catcodes make it past the lexer, which we can represent in the
-- type system.
data CoreCatCode
    = BeginGroup   -- 1
    | EndGroup     -- 2
    | MathShift    -- 3
    | AlignTab     -- 4
    | Parameter    -- 6
    | Superscript  -- 7
    | Subscript    -- 8
    | Space        -- 10
    | Letter       -- 11
    | Other        -- 12
    | Active       -- 13
    deriving (Show, Eq)

data CatCode
    = Escape       -- 0
    | EndOfLine    -- 5
    | Ignored      -- 9
    | Comment      -- 14
    | Invalid      -- 15
    | CoreCatCode CoreCatCode
    deriving (Show, Eq)

instance Enum CatCode where
    fromEnum = \case
        Escape                  -> 0
        CoreCatCode BeginGroup  -> 1
        CoreCatCode EndGroup    -> 2
        CoreCatCode MathShift   -> 3
        CoreCatCode AlignTab    -> 4
        EndOfLine               -> 5
        CoreCatCode Parameter   -> 6
        CoreCatCode Superscript -> 7
        CoreCatCode Subscript   -> 8
        Ignored                 -> 9
        CoreCatCode Space       -> 10
        CoreCatCode Letter      -> 11
        CoreCatCode Other       -> 12
        CoreCatCode Active      -> 13
        Comment                 -> 14
        Invalid                 -> 15

    toEnum = \case
        0  -> Escape
        1  -> CoreCatCode BeginGroup
        2  -> CoreCatCode EndGroup
        3  -> CoreCatCode MathShift
        4  -> CoreCatCode AlignTab
        5  -> EndOfLine
        6  -> CoreCatCode Parameter
        7  -> CoreCatCode Superscript
        8  -> CoreCatCode Subscript
        9  -> Ignored
        10 -> CoreCatCode Space
        11 -> CoreCatCode Letter
        12 -> CoreCatCode Other
        13 -> CoreCatCode Active
        14 -> Comment
        15 -> Invalid

instance Bounded CatCode where
    minBound = Escape

    maxBound = Invalid

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
newCatCode = \case
    '\\' -> Escape
    ' '  -> CoreCatCode Space
    '%'  -> Comment
    '\n' -> EndOfLine  -- Non-Standard.
    '\r' -> EndOfLine
    c | c == toEnum 0         -> Ignored  -- Null.
      | c == toEnum 127       -> Invalid
      | c `elem` ['a' .. 'z'] -> CoreCatCode Letter
      | c `elem` ['A' .. 'Z'] -> CoreCatCode Letter
    _                         -> CoreCatCode Other

newCatCodes :: CatCodes
newCatCodes =
  HMap.fromList $ fmap (\n -> (toEnum n, newCatCode $ toEnum n)) [0 .. 127]

-- Add in some useful extras beyond the technical defaults.
extras :: [(CharCode, CatCode)]
extras =
  [ ('^', CoreCatCode Superscript)
  , ('{', CoreCatCode BeginGroup)
  , ('}', CoreCatCode EndGroup)
  , ('#', CoreCatCode Parameter)
  ]

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
        Just $ if (cat1 == CoreCatCode Superscript) && (n1 == n2) && (charToCat n3 /= EndOfLine)
            then (CharCat n3Triod $ charToCat n3Triod, FDirected rest)
            else (CharCat n1 cat1, FDirected (n2 : n3 : rest))
extractCharCat charToCat (FDirected (n1 : rest)) =
    Just (CharCat n1 $ charToCat n1, FDirected rest)
