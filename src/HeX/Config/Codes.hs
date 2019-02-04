{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HeX.Config.Codes where

import           Data.Bits                      ( (.&.)
                                                , shiftR
                                                , shiftL
                                                )
import           Data.Char                      ( chr
                                                , ord
                                                , toLower
                                                , toUpper
                                                )
import qualified Data.HashMap.Strict           as HMap

import           HeXPrelude
import           HeX.Type
import qualified HeX.Categorise                as Cat

initialiseCharCodeMap :: (Cat.CharCode -> v) -> Cat.CharCodeMap v
initialiseCharCodeMap val = HMap.fromList $ ((\c -> (c, val c)) . chr) <$> [0 .. 127]

digits :: [Char]
digits = ['1'..'9']

lowerLetters :: [Char]
lowerLetters = ['a'..'z']

upperLetters :: [Char]
upperLetters = ['A'..'Z']

letters :: [Char]
letters = lowerLetters ++ upperLetters

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

data DelimiterCode
    = NotADelimiter IntVal
    | DelimiterSpecCode DelimiterSpec
    deriving (Show)

instance Bounded DelimiterCode where
    minBound = NotADelimiter (minBound :: IntVal)
    maxBound = DelimiterSpecCode $ DelimiterSpec maxBound maxBound

instance Enum DelimiterCode where
    toEnum n
        | n < 0 = NotADelimiter n
        | n > 0xFFFFFF = error $ "Value too large: " ++ show n
        | otherwise =
            let smallVar = toEnum $ n `shiftR` 12
                largeVar = toEnum $ n .&. 0xFFF
            in  DelimiterSpecCode $ DelimiterSpec smallVar largeVar

    fromEnum (NotADelimiter n) = n
    fromEnum (DelimiterSpecCode DelimiterSpec{..}) =
        (fromEnum largeVar `shiftL` 12) + (fromEnum smallVar)

data DelimiterSpec = DelimiterSpec { smallVar, largeVar :: DelimiterVar }
    deriving (Show)

data DelimiterVar
    = PresentDelimiterVar FamilyCharRef
    | NullDelimiterVar
    deriving (Show)

instance Bounded DelimiterVar where
    minBound = NullDelimiterVar
    maxBound = PresentDelimiterVar (maxBound :: FamilyCharRef)

instance Enum DelimiterVar where
    toEnum n
        | n == 0 = NullDelimiterVar
        | otherwise = PresentDelimiterVar $ toEnum n

    fromEnum NullDelimiterVar = 0
    fromEnum (PresentDelimiterVar f) = fromEnum f

data FamilyCharRef = FamilyCharRef { family, position :: IntVal }
    deriving (Show)

instance Bounded FamilyCharRef where
    minBound = FamilyCharRef 0x0 0x00
    maxBound = FamilyCharRef 0xF 0xFF

instance Enum FamilyCharRef where
    toEnum n
        | n < 0 = error $ "Negative value: " ++ show n
        | n > 0xFFF = error $ "Value too large: " ++ show n
        | otherwise = FamilyCharRef (n `shiftR` 8) (n .&. 0xFF)

    fromEnum (FamilyCharRef fam pos) =
        (fam `shiftL` 8) + pos

-- All delcodes are −1 until they are changed by a \delcode command.
newDelimiterCodeMap :: Cat.CharCodeMap DelimiterCode
newDelimiterCodeMap = initialiseCharCodeMap $ const $ NotADelimiter (-1)

-- Math code.

-- A math-code is specified by a number between 0 and 4095.
-- Consider such a number, represented as four hexadecimal digits: "pfcc
-- p: MathClass
-- f: font family
-- cc: character code position
-- A mathcode can also have the special value "8000, which causes the character
-- to behave as if it has catcode 13 (active).

data MathCode
    = NormalMathCode MathClass FamilyCharRef
    | ActiveMathCode
    deriving (Show)

data MathClass
    = Ordinary        -- 0
    | LargeOperator   -- 1
    | BinaryRelation  -- 2
    | Relation        -- 3
    | Opening         -- 4
    | Closing         -- 5
    | Punctuation     -- 6
    | VariableFamily  -- 7
    deriving (Show, Enum, Bounded)

instance Bounded MathCode where
    minBound = NormalMathCode minBound minBound
    maxBound = ActiveMathCode

instance Enum MathCode where
    toEnum n
        | n < 0 = error $ "Negative value: " ++ show n
        | n > 0x8000 = error $ "Value too large: " ++ show n
        | n == 0x8000 = ActiveMathCode
        | otherwise =
            let cls = toEnum $ n `shiftR` 12
                fam = toEnum $ n .&. 0xFFF
            in NormalMathCode cls fam

    fromEnum ActiveMathCode = 0x8000
    fromEnum (NormalMathCode cls famRef) =
        fromEnum famRef + (fromEnum cls `shiftL` 12)

-- The ten digits have \mathcode x = x + "7000.
-- The 52 letters have \mathcode x = x + "7100.
-- Otherwise,          \mathcode x = x
-- Put otherwise: letters are class 7, family 1; digits are class 7, family 0.
newMathCodeMap :: Cat.CharCodeMap MathCode
newMathCodeMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` digits  = NormalMathCode VariableFamily (FamilyCharRef 0 $ ord c)
        | c `elem` letters = NormalMathCode VariableFamily (FamilyCharRef 1 $ ord c)
        | otherwise        = NormalMathCode Ordinary       (FamilyCharRef 0 $ ord c)

-- Change case code.

-- Conversion to uppercase means that a character is replaced by its \uccode
-- value, unless the \uccode value is zero, when no change is made. Conversion
-- to lowercase is similar, using the \lccode.

data CaseChangeCode
    = NoCaseChange
    | ChangeToCode Cat.CharCode
    deriving (Show)

instance Bounded CaseChangeCode where
    minBound = NoCaseChange
    maxBound = ChangeToCode maxBound

instance Enum CaseChangeCode where
    toEnum n
        | n < 0 = error $ "Negative value: " ++ show n
        | n > 255 = error $ "Value too large: " ++ show n
        | n == 0 = NoCaseChange
        | otherwise = ChangeToCode $ toEnum n

    fromEnum NoCaseChange = 0
    fromEnum (ChangeToCode c) = fromEnum c

-- By default, all \uccode and \lccode values are zero except that the
-- letters a to z and A to Z have \uccode values A to Z and \lccode values a to
-- z.
newLowercaseMap :: Cat.CharCodeMap CaseChangeCode
newLowercaseMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` letters = ChangeToCode $ toLower c
        | otherwise        = NoCaseChange

newUppercaseMap :: Cat.CharCodeMap CaseChangeCode
newUppercaseMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` letters = ChangeToCode $ toUpper c
        | otherwise        = NoCaseChange

-- Space factor code.

newtype SpaceFactorCode = SpaceFactorCode IntVal
    deriving (Show)

instance Bounded SpaceFactorCode where
    minBound = SpaceFactorCode 0
    maxBound = SpaceFactorCode 0x7FFF

instance Enum SpaceFactorCode where
    toEnum n
        | n < 0 = error $ "Negative value: " ++ show n
        | n >= 0x8000 = error $ "Value too large: " ++ show n
        | otherwise = SpaceFactorCode n

    fromEnum (SpaceFactorCode n) = n

-- By default, all characters have a space factor code of 1000, except that the
-- uppercase letters ‘A’ through ‘Z’ have code 999.
newSpaceFactorMap :: Cat.CharCodeMap SpaceFactorCode
newSpaceFactorMap = initialiseCharCodeMap $ SpaceFactorCode . f
  where
    f c
        | c `elem` ['A' .. 'Z'] = 999
        | otherwise             = 1000
