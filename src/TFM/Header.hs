module TFM.Header where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString

import           TFM.Common

-- The length of the character coding scheme and font family sections,
-- respectively.
characterCodingSchemeLength, familyLength :: Int
characterCodingSchemeLength = 40
familyLength = 20

-- The information stored in the header table of a TFM file.
data Header = Header
  { checksum :: Int
  , designFontSize :: Rational
  , characterCodingScheme :: ByteString
  , family :: ByteString
  } deriving (Show)

getHeader :: Int -> Get Header
getHeader charInfoPos
    -- Read header[0 ... 1], containing the checksum and design size.
 = do
  _checksum <- getWord32beInt
  _designFontSize <- getFixWord
    -- Store our current position.
  postDesignPos <- fromIntegral <$> bytesRead
    -- Read header[2 ... 11] if present, containing the character coding
    -- scheme.
    -- If we haven't yet reached the character info table, then assume a
    -- character coding scheme is given.
  _characterCodingScheme <-
    if postDesignPos < charInfoPos
      then getBCPL
      else pure empty
    -- Compute where the next section should start, and where we actually are,
    -- and skip past the difference.
  let postSchemePos = postDesignPos + characterCodingSchemeLength
  postSchemeNowPos <- fromIntegral <$> bytesRead
  skip $ postSchemePos - postSchemeNowPos
    -- Read header[12 ... 16] if present, containing the font family.
    -- If we still haven't reached the character info table, then assume a
    -- font family is given.
  _family <-
    if postSchemePos < charInfoPos
      then getBCPL
      else pure empty
    -- Again, skip past any remaining data until the next section.
  let postFamilyPos = postSchemePos + familyLength
  postFamilyNowPos <- fromIntegral <$> bytesRead
  skip $ postFamilyPos - postFamilyNowPos
    -- Read header[12 ... 16] if present, containing random bits.
    -- We don't actually use these for anything now.
  when
    (postFamilyPos < charInfoPos)
    (sequence_
       [ getWord8 -- Seven-bit safe flag.
       , getWord8 -- Unknown.
       , getWord8 -- 'Face'.
       ])
    -- Don't read header [18 ... whatever]
  return
    Header
    { checksum = _checksum
    , designFontSize = _designFontSize
    , characterCodingScheme = _characterCodingScheme
    , family = _family
    }
