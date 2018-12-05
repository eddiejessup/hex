module TFM.LigKern where

import           Prelude                 hiding ( length )

import           Data.Bits
import           Data.ByteString         hiding ( head
                                                , last
                                                )
import           Data.List.Split

import           TFM.Common

-- The lig/kern array contains instructions explaining how to handle special
-- letter pairs. Each instruction consists of four bytes:
-- * skip_byte: Skip this many intervening steps to reach the next step. If
--   >= 128, indicates that this is the final program step.
-- * next_char: If this character follows the current character in the input,
--   then perform the given operation and stop. Otherwise, continue.
-- * op_byte: If < 128, indicates a ligature step. Otherwise, indicates a kern step.
-- * remainder: Argument for the ligature or kern instruction.
-- # Kern step instructions:
--
-- Insert an additional space equal to:
--     kern[256 * (op_byte + 128) + remainder]
-- between the current character and next characters in the input. This amount
-- can be negative, bringing the characters closer together, or positive,
-- pushing them apart.
-- # Ligature step instructions:
--
-- There are eight kinds of ligature steps, with 'op_byte' codes:
--     4a + 2b + c
-- where
--     0 <= a <= b + c
--     0 <= b
--     c in [0, 1]
-- * Insert the character with code 'remainder', between the current and next characters
-- * If 'b = 0', delete the current character
-- * If 'c = 0', delete the next character
-- * Pass over 'a' characters in the input to reach the next current character.
--   (This character may have its own ligature/kerning program).
-- Note that:
-- * if 'a = 0' and 'b = 1', the current character is unchanged
-- * if 'a = b' and 'c = 1', the current character is changed but the next character is unchanged.
-- # Boundary characters.
--
-- TeX puts implicit characters at the left and right boundaries of each
-- consecutive string of characters from the same font. These characters do not
-- appear in the output, but they can affect ligatures and kerning.
--
-- If the first instruction of the lig/kern array has 'skip_byte = 255', that
-- instruction's 'next_char' is the right-boundary character of
-- this font. The value of 'next_char' need not lie between the smallest and
-- largest character codes in the font, according to the TFM file.
-- If the last instruction of the lig/kern array has 'skip_byte = 255', there
-- is a special ligature/kerning program for a left-boundary character,
-- beginning at location:
--     256 * op_byte + remainder
-- # Optional larger lig/kern arrays
--
-- If the first instruction of a character's 'lig_kern' program has
-- 'skip_byte > 128', the program actually begins in location:
--     256 * op_byte + remainder
-- This allows large lig/kern arrays, because the first
-- instruction must otherwise appear in a location that is <= 255.
-- Any instruction with 'skip_byte > 128' in the lig/kern array must satisfy:
--     256 * op_byte + remainder < nl
-- Where 'nl' is the number of words in the lig/kern table. If such an
-- instruction is encountered during program execution, it denotes an
-- unconditional halt, without performing a ligature command.
kernOp :: Int
kernOp = 128

data LigKernOp
  = LigatureOp { ligatureChar :: Int
               , charsToPassOver :: Int
               , deleteCurrentChar :: Bool
               , deleteNextChar :: Bool }
  | KernOp { size :: Rational }
  deriving (Show)

data LigKernInstr = LigKernInstr
  { stop :: Bool
  , nextChar :: Int
  , operation :: LigKernOp
  } deriving (Show)

readInstrSet :: [[Int]] -> Int -> (Int, Int, Int, Int)
readInstrSet sets i =
  let [w, x, y, z] = sets !! i
  in (w, x, y, z)

getLigKernOperation :: (Int -> LigKernOp) -> Int -> Int -> LigKernOp
getLigKernOperation readKern op remain =
  if op >= kernOp
    then readKern $ 256 * (op - kernOp) + remain
    else LigatureOp
         { ligatureChar = remain
         , charsToPassOver = op `shift` 2
         , deleteCurrentChar = (op .&. 0x02) == 0
         , deleteNextChar = (op .&. 0x01) == 0
         }

analyzeLigKernInstr ::
     (Int -> LigKernOp) -> Int -> Int -> Int -> Int -> LigKernInstr
analyzeLigKernInstr readKern skip next op remain =
  let _operation = getLigKernOperation readKern op remain
  in LigKernInstr {stop = skip >= 128, nextChar = next, operation = _operation}

getKern :: ByteString -> (Int -> LigKernOp)
getKern kStr iWords = KernOp {size = runGetAt getFixWord kStr iWords}

readLigKerns :: ByteString -> ByteString -> [LigKernInstr]
readLigKerns ligKernStr kernStr
  | firstSkipByte == 255 =
    error "Sorry, right boundary characters are not supported"
  | firstSkipByte > 128 = error "Sorry, large LigKern arrays are not supported"
  | lastSkipByte == 255 =
    error "Sorry, left boundary characters are not supported"
  | otherwise =
    let kernGetter = getKern kernStr
    in fmap (\(a, b, c, d) -> analyzeLigKernInstr kernGetter a b c d) sets
  where
    ligKernTblLengthWords = length ligKernStr `div` 4
    readSet = readInstrSet $ (chunksOf 4 . fmap fromIntegral) (unpack ligKernStr)
    sets = fmap readSet [0 .. ligKernTblLengthWords - 1]
    (firstSkipByte, _, _, _) = head sets
    (lastSkipByte, _, _, _) = last sets
