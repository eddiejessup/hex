module Main where

import qualified Data.ByteString as BS
import qualified Data.Binary as BI
import qualified Data.Map as M
import Data.List.Split (chunksOf)
import Data.Binary.Strict.Get as BSG
import Data.Maybe (fromJust)
import qualified TFM.Parse as TFMP
import qualified Data.Word as W
import qualified Control.Monad as CM
import Data.Bits ((.&.), shift)

import Debug.Trace (traceShow, trace)


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

-- between :: Int -> Int -> [a] -> [a]
-- between start end a = drop start $ take end a

-- readFourBytes :: [W.Word8] -> Int -> (Int, Int, Int, Int)
-- readFourBytes bs words =
--     let
--         start = TFMP.wordToByte words
--         end = TFMP.wordToByte (words + 1)
--         z:y:x:w:[] = fmap fromIntegral $ between start end bs
--     in (w, x, y, z)

kernOp =  128


data LigKernOperation = Ligature { ligatureChar :: Int
                                 , charsToPassOver :: Int
                                 , deleteCurrentChar :: Bool
                                 , deleteNextChar :: Bool
                                 }
                      | Kern { size:: Double }
                      deriving (Show)

data LigKern = LigKern { stop :: Bool
                       , nextChar :: Int
                       , operation :: LigKernOperation } deriving (Show)


readInstrSet :: [[Int]] -> Int -> (Int, Int, Int, Int)
readInstrSet sets i =
    let
        w:x:y:z:[] = sets !! i
    in (w, x, y, z)

getLigKernOperation :: (Int -> Either String LigKernOperation) -> Int -> Int -> Either String LigKernOperation
getLigKernOperation readKern op remain =
    if op >= kernOp
        then readKern $ 256 * (op - kernOp) + remain
        else Right Ligature { ligatureChar=remain
                            , charsToPassOver=op `shift` 2
                            , deleteCurrentChar=(op .&. 0x02) == 0
                            , deleteNextChar=(op .&. 0x01) == 0
                            }

analyzeLigKernInstr :: (Int -> Either String LigKernOperation) -> Int -> Int -> Int -> Int -> Either String LigKern
analyzeLigKernInstr readKern skip next op remain =
    let operation = getLigKernOperation readKern op remain
    in case operation of Left s -> Left s
                         Right o -> Right LigKern { stop=skip >= 128
                                                  , nextChar=next
                                                  , operation=o }

getKern :: TFMP.TFM -> BS.ByteString -> (Int -> Either String LigKernOperation)
getKern tfm contents iWords =
    let
        kernTblPos = TFMP.tablePointerPos tfm TFMP.Kern
        kernPos = kernTblPos + TFMP.wordToByte iWords
        kernContents = BS.drop kernPos contents
        (size, _) = BSG.runGet TFMP.getFixWord kernContents
    in
        case size of Left s -> Left s
                     Right s -> Right Kern { size=s }
        

readLigKerns :: TFMP.TFM -> BS.ByteString -> Either String [Either String LigKern]
readLigKerns tfm contents
    | firstSkipByte == 255 = Left "Sorry, right boundary characters are not supported"
    | firstSkipByte > 128 = Left "Sorry, large LigKern arrays are not supported"
    | lastSkipByte == 255 = Left "Sorry, left boundary characters are not supported"
    | otherwise = Right ligKerns
    where
        ligKernTblPos = TFMP.tablePointerPos tfm TFMP.LigKern
        contentsLigKernOnwards = BS.drop ligKernTblPos contents
        ligKernBytes = BS.unpack contentsLigKernOnwards
        ligKernInts = fmap fromIntegral ligKernBytes
        ligKernIntSets = chunksOf 4 ligKernInts
        readSet = readInstrSet ligKernIntSets

        (firstSkipByte, _, _, _) = readSet 0

        ligKernTblLengthWords = TFMP.tableLength tfm TFMP.LigKern
        (lastSkipByte, _, _, _) = readSet (ligKernTblLengthWords - 1)

        kernGetter = getKern tfm contents
        sets = fmap readSet [0..ligKernTblLengthWords - 1]
        ligKerns = fmap (\(a, b, c, d) -> analyzeLigKernInstr kernGetter a b c d) sets

main = do
    contents <- BS.readFile "cmr10.tfm"
    let foo = BSG.runGet TFMP.newTFM contents
    let (Right tfm, _) = BSG.runGet TFMP.newTFM contents
    let (Right headers, _) = BSG.runGet (TFMP.readHeader tfm) contents
    let (fontParams, _) = BSG.runGet (TFMP.readFontParams tfm headers) contents
    let (Right ligKerns) = readLigKerns tfm contents
    -- print headers
    -- print fontParams
    print ligKerns
    -- print $ TFMP.tableLength tfm TFMP.Kern
    -- print $ TFMP.tablePointerPos tfm TFMP.Kern
    return ()
