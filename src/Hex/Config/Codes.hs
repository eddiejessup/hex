{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Hex.Config.Codes where

import           Hexlude

import qualified Data.Ascii           as Ascii
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Vector          as V
import qualified Data.Map.Strict      as Map
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text            as Text
import           Path                 (File, Path)
import qualified Path
import qualified Test.QuickCheck as QC
import           Hex.Quantity

class TeXCode a where
    toTeXInt :: a -> TeXInt

    fromTeXInt :: TeXInt -> Maybe a

newtype CharCode = CharCode { codeWord :: Word8 }
    deriving newtype (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Hashable, ToJSON)

instance Arbitrary CharCode where
    arbitrary = QC.elements [0x20..0x7e]

codeInt :: CharCode -> Int
codeInt = fromIntegral . codeWord

instance Describe CharCode where
  describe c = singleLine $ "CharCode/" <> quote (Text.singleton (unsafeCodeAsChar c))

instance TeXCode CharCode where
    toTeXInt = TeXInt . fromIntegral

    fromTeXInt n
        | n > 256 = Nothing
        | n < 0 = Nothing
        | otherwise = Just $ fromIntegral n

readCharCodes :: MonadIO m => Path a File -> m BS.L.ByteString
readCharCodes path = liftIO (BS.L.readFile (Path.toFilePath path))

pattern CharCode_ :: Char -> CharCode
pattern CharCode_ c <- (unsafeCodeAsChar -> c)
  where
    CharCode_ c = unsafeCodeFromChar c

unsafeCodeFromChar :: Char -> CharCode
unsafeCodeFromChar c = CharCode (Ascii.unsafeCharToWord8 c)

unsafeCodeAsChar :: CharCode -> Char
unsafeCodeAsChar = Ascii.unsafeWord8ToChar . codeWord

unsafeCodesFromChars :: [Char] -> [CharCode]
unsafeCodesFromChars = fmap unsafeCodeFromChar

unsafeCodesAsChars :: [CharCode] -> [Char]
unsafeCodesAsChars = fmap unsafeCodeAsChar

-- unsafeCodeFromChar :: Char -> CharCode
-- unsafeCodeFromChar = CharCode . c2w

-- codesFromStr :: [Char] -> [CharCode]
-- codesFromStr = toS

isAlphaChar :: CharCode -> Bool
isAlphaChar = Ascii.isAsciiAlpha . codeWord

isLowerChar :: CharCode -> Bool
isLowerChar = Ascii.isAsciiLower . codeWord

isUpperChar :: CharCode -> Bool
isUpperChar = Ascii.isAsciiUpper . codeWord

isDecDigitChar :: CharCode -> Bool
isDecDigitChar = Ascii.isDecDigit . codeWord

toUpperChar :: CharCode -> CharCode
toUpperChar = CharCode . Ascii.toAsciiUpper . codeWord

toLowerChar :: CharCode -> CharCode
toLowerChar = CharCode . Ascii.toAsciiLower . codeWord

type CharCodeMap v = Map CharCode v
type CharCodeHashMap v = HashMap.HashMap CharCode v

initialiseCharCodeMap :: (CharCode -> v) -> CharCodeMap v
initialiseCharCodeMap val = Map.fromList $
    (\c -> (c, val c)) <$> [minBound..maxBound]

initialiseCharCodeHashMap :: (CharCode -> v) -> CharCodeHashMap v
initialiseCharCodeHashMap val = HashMap.fromList $
    (\c -> (c, val c)) <$> [minBound..maxBound]

initialiseCharCodeVector :: (CharCode -> v) -> V.Vector (Maybe v)
initialiseCharCodeVector val = V.fromList $ Just . val <$> [minBound..maxBound]

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
    deriving stock (Show)

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
    deriving stock (Show)

data DelimiterVar = PresentDelimiterVar FamilyCharRef | NullDelimiterVar
    deriving stock (Show)

instance TeXCode DelimiterVar where
    toTeXInt NullDelimiterVar        = 0
    toTeXInt (PresentDelimiterVar f) = toTeXInt f

    fromTeXInt n
        | n == 0 = Just NullDelimiterVar
        | otherwise = PresentDelimiterVar <$> fromTeXInt n

data FamilyCharRef = FamilyCharRef { family :: TeXInt, position :: CharCode }
    deriving stock (Show)

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
    deriving stock (Show)

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
    deriving stock ( Show, Enum, Bounded )

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
        | isDecDigitChar c =
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
    deriving stock (Show)

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
    deriving stock (Show)

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
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

instance Describe CoreCatCode where
    describe a = singleLine $ "CoreCatCode/" <> show a

instance Arbitrary CoreCatCode where
  arbitrary = genericArbitraryU

data CatCode
    = Escape       -- 0
    | EndOfLine    -- 5
    | Ignored      -- 9
    | Comment      -- 14
    | Invalid      -- 15
    | CoreCatCode CoreCatCode
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

instance Describe CatCode where
    describe = \case
        CoreCatCode ccc ->
            describePrepended 0 "CatCode" ccc
        a -> singleLine $ "CatCode " <> show a

instance Arbitrary CatCode where
  arbitrary = genericArbitraryU

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

type CatCodes = HashMap.HashMap CharCode CatCode

newCatCodes :: CatCodes
newCatCodes = initialiseCharCodeHashMap $ \case
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
usableCatCodes = foldl' (\m (k, v) -> HashMap.insert k v m) newCatCodes extras
  where
    extras =
      [ (CharCode_ '^', CoreCatCode Superscript)
      , (CharCode_ '{', CoreCatCode BeginGroup)
      , (CharCode_ '}', CoreCatCode EndGroup)
      , (CharCode_ '#', CoreCatCode Parameter)
      ]

catLookup :: CatCodes -> CharCode -> CatCode
catLookup m n = HashMap.lookupDefault Invalid n m

usableCatLookup :: CharCode -> CatCode
usableCatLookup = catLookup usableCatCodes
