module HeX.Categorise where

import qualified Data.HashMap.Strict as HMap

data CatCode
  = Escape -- 0
  | BeginGroup -- 1
  | EndGroup -- 2
  | MathShift -- 3
  | AlignTab -- 4
  | EndOfLine -- 5
  | Parameter -- 6
  | Superscript -- 7
  | Subscript -- 8
  | Ignored -- 9
  | Space -- 10
  | Letter -- 11
  | Other -- 12
  | Active -- 13
  | Comment -- 14
  | Invalid -- 15
  deriving (Show, Eq, Enum)

type CharCode = Char

data CharCat = CharCat
  { char :: CharCode
  , cat :: CatCode
  }

instance Show CharCat where
  show (CharCat c ct) = show ct ++ " '" ++ [c] ++ "'"

type CharCatMap = HMap.HashMap CharCode CatCode

defaultCatCode :: CharCode -> CatCode
defaultCatCode c
  | c == '\\' = Escape
  | c == ' ' = Space
  | c == '%' = Comment
  | c == toEnum 0 = Ignored -- Null
    -- NON-STANDARD
  | c == '\n' = EndOfLine
  | c == '\r' = EndOfLine
  | c == toEnum 127 = Invalid
  | c `elem` ['a' .. 'z'] = Letter
  | c `elem` ['A' .. 'Z'] = Letter
  | otherwise = Other

defaultCharCatMap :: CharCatMap
defaultCharCatMap =
  HMap.fromList $ fmap (\n -> (toEnum n, defaultCatCode $ toEnum n)) [0 .. 127]

-- Add in some useful extras beyond the technical defaults.
extras :: [(CharCode, CatCode)]
extras
  = [ ('^', Superscript)
    , ('{', BeginGroup)
    , ('}', EndGroup)
    , ('#', Parameter) ]

usableCharCatMap :: CharCatMap
usableCharCatMap =
  foldl (\m (k, v) -> HMap.insert k v m) defaultCharCatMap extras

catLookup :: CharCatMap -> CharCode -> CatCode
catLookup m n = HMap.lookupDefault Invalid n m

defaultCharToCat, usableCharToCat :: CharCode -> CatCode
defaultCharToCat = catLookup defaultCharCatMap
usableCharToCat = catLookup usableCharCatMap

extractCharCat ::
     (CharCode -> CatCode) -> [CharCode] -> Maybe (CharCat, [CharCode])
extractCharCat _ [] = Nothing
extractCharCat charToCat (n1:n2:n3:rest)
    -- Next two characters must be identical, and have category
    -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
 =
  let cat1 = charToCat n1
      n3Triod =
        toEnum $
        fromEnum n3 +
        (if fromEnum n3 < 64
           then 64
           else (-64))
  in if (cat1 == Superscript) && (n1 == n2) && (charToCat n3 /= EndOfLine)
       then Just (CharCat n3Triod $ charToCat n3Triod, rest)
       else Just (CharCat n1 cat1, n2 : n3 : rest)
extractCharCat charToCat (n1:rest) = Just (CharCat n1 $ charToCat n1, rest)
