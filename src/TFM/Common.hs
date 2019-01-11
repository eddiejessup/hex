module TFM.Common where

import           Prelude                 hiding ( drop )

-- Convert a quantity in units of words, into the equivalent in bytes.
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString
import qualified Data.ByteString.Lazy          as BSL
import           Data.Ratio                     ( (%) )

-- The increment by which real numbers can be specified.
fixWordScale :: Rational
fixWordScale = 1 % (2 ^ (20 :: Integer))

wordToByte :: (Num a) => a -> a
wordToByte = (* 4)

-- Read a floating point value.
getFixWord :: Get Rational
getFixWord = (* fixWordScale) . fromIntegral <$> getWord32be

-- Read integers encoded as big-endian byte sequences.
getWord8Int, getWord16beInt, getWord32beInt :: Get Int
getWord8Int = fromIntegral <$> getWord8
getWord16beInt = fromIntegral <$> getWord16be
getWord32beInt = fromIntegral <$> getWord32be

-- Read a string that's encoded as an integer, followed by that number of
-- characters.
getBCPL :: Get ByteString
getBCPL = getWord8Int >>= getByteString

get4Word8Ints :: Get (Int, Int, Int, Int)
get4Word8Ints =
    do
    b1 <- getWord8Int
    b2 <- getWord8Int
    b3 <- getWord8Int
    b4 <- getWord8Int
    pure (b1, b2, b3, b4)

runGetStrict :: Get a -> ByteString -> a
runGetStrict a s = runGet a $ BSL.fromStrict s

runGetAt :: Get a -> ByteString -> Int -> a
runGetAt a s iWords = runGetStrict a $ drop (wordToByte iWords) s
