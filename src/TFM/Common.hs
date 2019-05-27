module TFM.Common where

import HeXlude

import           Control.Monad                  ( when )
import           Control.Monad.Fail             ( fail )

import           Data.Ascii                     ( Ascii )
import qualified Data.Ascii                    as Asc
import           Data.Binary.Get                ( Get)
import qualified Data.Binary.Get               as B.G
import           Data.Ratio                     ( (%) )

-- The increment by which real numbers can be specified.
fixWordScale :: Rational
fixWordScale = 1 % (2 ^ (20 :: Integer))

-- Convert a quantity in units of words, into the equivalent in bytes.
wordToByte :: (Num a) => a -> a
wordToByte = (* 4)

-- Read a floating point value.
getFixWord :: Get Rational
getFixWord = (* fixWordScale) . fromIntegral <$> getWord32beInt

-- Read integers encoded as big-endian byte sequences.
getWord8Int, getWord16beInt, getWord32beInt :: Get Int
getWord8Int = fromIntegral <$> B.G.getWord8
getWord16beInt = fromIntegral <$> B.G.getWord16be
getWord32beInt = fromIntegral <$> B.G.getWord32be

getChunks :: Get v -> Get [v]
getChunks f =
    B.G.isEmpty >>= \b -> if b
        then pure []
        else
            do
            el <- f
            (el:) <$> getChunks f

-- Read a string that's encoded as an integer, followed by that number of
-- characters.
getBCPL :: Int -> Get Ascii
getBCPL maxLen =
    do
    n <- getWord8Int
    when (n > (maxLen - 1)) $ fail $ "BCPL string length too large: " <> show n
    s <- B.G.getByteString n
    asc <- case Asc.fromByteString s of
        Just v -> pure v
        Nothing -> fail "Could not decode ASCII from bytes"
    B.G.skip $ maxLen - 1 - n
    pure asc
