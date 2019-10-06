{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module HeX.Config.Codes where

import           HeXlude

import           Data.Ascii.Word8
import           Data.Bits           (shiftL, shiftR, (.&.))
import           Data.Hashable       (Hashable)
import qualified Data.Map.Strict     as Map
import qualified Data.Path           as D.Path
import qualified Data.Sequence       as Seq
import qualified Data.Text           as Text
import           Path                (File, Path)

import           HeX.Quantity

class TeXCode a where
    toTeXInt :: a -> TeXInt

    fromTeXInt :: TeXInt -> Maybe a




newtype CharCode = CharCode { codeWord :: Word8 }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits)
    deriving anyclass (Hashable)

instance Readable CharCode where
  describe c = "'" <> Text.singleton (codeAsChar c) <> "'"

instance TeXCode CharCode where
    toTeXInt = TeXInt . fromIntegral

    fromTeXInt n
        | n > 256 = Nothing
        | n < 0 = Nothing
        | otherwise = Just $ fromIntegral n

instance StringConv [CharCode] Text where
    strConv leniency ccList =
        let charList = codeAsChar <$> ccList
        in strConv leniency charList

instance StringConv (Seq CharCode) Text where
    strConv leniency ccSeq =
        let ccList = toList ccSeq
        in strConv leniency ccList

instance StringConv [Char] (Seq CharCode) where
    strConv _ chars = Seq.fromList $ CharCode . ascii <$> chars

instance StringConv [Char] [CharCode] where
    strConv _ chars = CharCode . ascii <$> chars

codesToS :: (StringConv [Char] a, Functor f, Foldable f) => f CharCode -> a
codesToS cs = toS $ toList $ codeAsChar <$> cs

readCharCodes :: MonadIO m => Path a File -> m (Seq CharCode)
readCharCodes path = fmap CharCode <$> liftIO (D.Path.readPathBytes path)

pattern CharCode_ :: Char -> CharCode
pattern CharCode_ c <- (codeAsChar -> c)
  where
    CharCode_ c = CharCode (ascii c)

codeAsChar :: CharCode -> Char
codeAsChar = toChar . codeWord

codeFromChar :: Char -> CharCode
codeFromChar = CharCode . ascii

codesFromStr :: [Char] -> [CharCode]
codesFromStr = toS

isAlphaChar :: CharCode -> Bool
isAlphaChar = isAlpha . codeWord

isLowerChar :: CharCode -> Bool
isLowerChar = isLower . codeWord

isUpperChar :: CharCode -> Bool
isUpperChar = isUpper . codeWord

isDigitChar :: CharCode -> Bool
isDigitChar = isDigit . codeWord

toUpperChar :: CharCode -> CharCode
toUpperChar = CharCode . toUpper . codeWord

toLowerChar :: CharCode -> CharCode
toLowerChar = CharCode . toLower . codeWord

safeFromUpAF :: Num a => Word8 -> Maybe a
safeFromUpAF w
    | isUpAF w = Just $ fromUpAF w
    | otherwise = Nothing



type CharCodeMap v = Map CharCode v

initialiseCharCodeMap :: (CharCode -> v) -> CharCodeMap v
initialiseCharCodeMap val = Map.fromList $
    (\c -> (c, val c)) <$> [minBound..maxBound]






-- The ⟨number⟩ at the end of a ⟨code assignment⟩ must not be negative, except
-- in the case that a \delcode is being assigned.
-- Furthermore, that ⟨number⟩
-- should be at most 15 for \catcode, 32768 for \mathcode, 255 for \lccode or
-- \uccode, 32767 for \sfcode, and 2^24 − 1 for \delcode.
-- Delimiter code.
-- a delcode is either negative, for characters that should not act as
-- delimiters, or less than "1000000.
-- In other words, non-negative delcodes consist of six hexadecimal digits.
-- The first and last sets of three digits specify "small" and "large" variants
-- of the delimiter, respectively.
-- the code,
--     "123456
-- implies a small variant in position "23 of family "1, and a large variant in
-- position "56 of family "4.
-- If the small or large variant is given as "000, however (position 0 of
-- family 0), that variant is ignored.
data DelimiterCode = NotADelimiter TeXInt | DelimiterSpecCode DelimiterSpec
    deriving ( Show )

instance TeXCode DelimiterCode where
    toTeXInt (NotADelimiter n) = n
    toTeXInt (DelimiterSpecCode DelimiterSpec{smallVar, largeVar}) =
        (toTeXInt largeVar `shiftL` 12) + toTeXInt smallVar

    fromTeXInt n
        | n < 0 = Just $ NotADelimiter n
        | n > 0xFFFFFF = Nothing
        | otherwise =
            do
            smallVar <- fromTeXInt $ n `shiftR` 12
            largeVar <- fromTeXInt $ n .&. 0xFFF
            Just $ DelimiterSpecCode $ DelimiterSpec smallVar largeVar

data DelimiterSpec = DelimiterSpec { smallVar, largeVar :: DelimiterVar }
    deriving ( Show )

data DelimiterVar = PresentDelimiterVar FamilyCharRef | NullDelimiterVar
    deriving ( Show )

instance TeXCode DelimiterVar where
    toTeXInt NullDelimiterVar        = 0
    toTeXInt (PresentDelimiterVar f) = toTeXInt f

    fromTeXInt n
        | n == 0 = Just NullDelimiterVar
        | otherwise = PresentDelimiterVar <$> fromTeXInt n

data FamilyCharRef = FamilyCharRef { family :: TeXInt, position :: CharCode }
    deriving ( Show )

instance TeXCode FamilyCharRef where
    toTeXInt (FamilyCharRef fam pos) = (fam `shiftL` 8) + fromIntegral pos

    fromTeXInt n
        | n < 0 = Nothing
        | n > 0xFFF = Nothing
        | otherwise = Just $ FamilyCharRef (n `shiftR` 8) (fromIntegral (n .&. 0xFF))

-- All delcodes are −1 until they are changed by a \delcode command.
newDelimiterCodes :: CharCodeMap DelimiterCode
newDelimiterCodes = initialiseCharCodeMap $ const $ NotADelimiter (-1)

-- Math code.
-- A math-code is specified by a number between 0 and 4095.
-- Consider such a number, represented as four hexadecimal digits: "pfcc
-- p: MathClass
-- f: font family
-- cc: character code position
-- A mathcode can also have the special value "8000, which causes the character
-- to behave as if it has catcode 13 (active).
data MathCode = NormalMathCode MathClass FamilyCharRef | ActiveMathCode
    deriving ( Show )

instance TeXCode MathCode where
    toTeXInt ActiveMathCode =
        0x8000
    toTeXInt (NormalMathCode cls famRef) =
        toTeXInt famRef + (toTeXInt cls `shiftL` 12)

    fromTeXInt n
        | n < 0 = Nothing
        | n > 0x8000 = Nothing
        | n == 0x8000 = Just ActiveMathCode
        | otherwise =
            NormalMathCode <$> fromTeXInt (n `shiftR` 12) <*> fromTeXInt (n .&. 0xFFF)

data MathClass =
      Ordinary        -- 0
    | LargeOperator   -- 1
    | BinaryRelation  -- 2
    | Relation        -- 3
    | Opening         -- 4
    | Closing         -- 5
    | Punctuation     -- 6
    | VariableFamily  -- 7
    deriving ( Show, Enum, Bounded )

instance TeXCode MathClass where
    toTeXInt = TeXInt . fromEnum

    fromTeXInt = Just . toEnum . unInt

-- The ten digits have \mathcode x = x + "7000.
-- The 52 letters have \mathcode x = x + "7100.
-- Otherwise,          \mathcode x = x
-- Put otherwise: letters are class 7, family 1; digits are class 7, family 0.
newMathCodes :: CharCodeMap MathCode
newMathCodes = initialiseCharCodeMap f
  where
    f c
        | isDigitChar c =
            NormalMathCode VariableFamily (FamilyCharRef (TeXInt 0) c)
        | isAlphaChar c =
            NormalMathCode VariableFamily (FamilyCharRef (TeXInt 1) c)
        | otherwise =
            NormalMathCode Ordinary       (FamilyCharRef (TeXInt 0) c)

-- Change case code.
-- Conversion to uppercase means that a character is replaced by its \uccode
-- value, unless the \uccode value is zero, when no change is made. Conversion
-- to lowercase is similar, using the \lccode.
data CaseChangeCode = NoCaseChange | ChangeToCode CharCode
    deriving ( Show )

instance TeXCode CaseChangeCode where
    toTeXInt NoCaseChange     = 0
    toTeXInt (ChangeToCode c) = toTeXInt c

    fromTeXInt n
        | n < 0 = Nothing
        | n > 255 = Nothing
        | n == 0 = Just NoCaseChange
        | otherwise = ChangeToCode <$> fromTeXInt n


-- By default, all \uccode and \lccode values are zero except that the
-- letters a to z and A to Z have \uccode values A to Z and \lccode values a to
-- z.
newLowercaseCodes :: CharCodeMap CaseChangeCode
newLowercaseCodes = initialiseCharCodeMap f
  where
    f c
        | isAlphaChar c = ChangeToCode $ toLowerChar c
        | otherwise = NoCaseChange

newUppercaseCodes :: CharCodeMap CaseChangeCode
newUppercaseCodes = initialiseCharCodeMap f
  where
    f c
        | isAlphaChar c = ChangeToCode $ toUpperChar c
        | otherwise = NoCaseChange

-- Space factor code.
newtype SpaceFactorCode = SpaceFactorCode TeXInt
    deriving ( Show )

instance TeXCode SpaceFactorCode where
    toTeXInt (SpaceFactorCode n) = n

    fromTeXInt n
        | n < 0 = Nothing
        | n >= 0x8000 = Nothing
        | otherwise = Just $ SpaceFactorCode n

-- By default, all characters have a space factor code of 1000, except that the
-- uppercase letters ‘A’ through ‘Z’ have code 999.
newSpaceFactors :: CharCodeMap SpaceFactorCode
newSpaceFactors = initialiseCharCodeMap $ SpaceFactorCode . f
  where
    f c
        | isUpperChar c = 999
        | otherwise = 1000







-- Not all Catcodes make it past the lexer.
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

instance Readable CoreCatCode where
    describe = show

data CatCode
    = Escape       -- 0
    | EndOfLine    -- 5
    | Ignored      -- 9
    | Comment      -- 14
    | Invalid      -- 15
    | CoreCatCode CoreCatCode
    deriving (Show, Eq)

instance TeXCode CatCode where
    toTeXInt = \case
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

    fromTeXInt = \case
        0  -> Just Escape
        1  -> Just $ CoreCatCode BeginGroup
        2  -> Just $ CoreCatCode EndGroup
        3  -> Just $ CoreCatCode MathShift
        4  -> Just $ CoreCatCode AlignTab
        5  -> Just EndOfLine
        6  -> Just $ CoreCatCode Parameter
        7  -> Just $ CoreCatCode Superscript
        8  -> Just $ CoreCatCode Subscript
        9  -> Just Ignored
        10 -> Just $ CoreCatCode Space
        11 -> Just $ CoreCatCode Letter
        12 -> Just $ CoreCatCode Other
        13 -> Just $ CoreCatCode Active
        14 -> Just Comment
        15 -> Just Invalid
        _ -> Nothing

type CatCodes = CharCodeMap CatCode

newCatCodes :: CatCodes
newCatCodes = initialiseCharCodeMap $ \case
    CharCode_ '\\'    -> Escape
    CharCode_ ' '     -> CoreCatCode Space
    CharCode_ '%'     -> Comment
    CharCode_ '\n'    -> EndOfLine  -- Non-Standard.
    CharCode_ '\r'    -> EndOfLine
    CharCode_ '\0'    -> Ignored
    CharCode_ '\DEL'  -> Invalid
    c | isAlphaChar c -> CoreCatCode Letter
    _ -> CoreCatCode Other

-- Add useful extras beyond the technical defaults.
usableCatCodes :: CatCodes
usableCatCodes = foldl' (\m (k, v) -> Map.insert k v m) newCatCodes extras
  where
    extras =
      [ (CharCode_ '^', CoreCatCode Superscript)
      , (CharCode_ '{', CoreCatCode BeginGroup)
      , (CharCode_ '}', CoreCatCode EndGroup)
      , (CharCode_ '#', CoreCatCode Parameter)
      ]

catDefault :: Maybe CatCode -> CatCode
catDefault = fromMaybe Invalid

catLookup :: CatCodes -> CharCode -> CatCode
catLookup m n = catDefault $ Map.lookup n m
