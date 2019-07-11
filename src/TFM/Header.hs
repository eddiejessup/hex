module TFM.Header where

import           HeXlude

import           Data.Ascii      (Ascii)
import qualified Data.Binary     as B
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as B.G

import           TFM.Common

-- The length of the character coding scheme and font family, respectively.
characterCodingSchemeLength, familyLength :: Int
characterCodingSchemeLength = 40
familyLength                = 20

-- The information stored in the header table of a TFM file.
data Header = Header
    { checksum              :: Int
    , designFontSize        :: Rational
    , characterCodingScheme :: Maybe Ascii
    , family                :: Maybe Ascii
    , sevenBitSafeFlag      :: Maybe B.Word8
    , face                  :: Maybe Face
    } deriving (Show)

data Face = Face Weight Slope Expansion
    deriving (Show)

data Weight = Medium | Bold | Light
    deriving (Show)

data Slope = Roman | Italic
    deriving (Show)

data Expansion = Regular | Condensed | Extended
    deriving (Show)

parseFace :: B.Word8 -> Maybe Face
parseFace n
    | n > 17 = Nothing
    | otherwise = Just $ Face wt sl ex
  where
    (d6, m6) = n `divMod` 6
    (d2, m2) = m6 `divMod` 2
    wt = case d6 of
        0 -> Medium
        1 -> Bold
        _ -> Light
    ex = case d2 of
        0 -> Regular
        1 -> Condensed
        _ -> Extended
    sl = case m2 of
        0 -> Roman
        _ -> Italic

getHeader :: Get Header
getHeader =
    do
    -- header[0 ... 1]: Required; checksum and design size.
    checksum <- getWord32beInt
    designFontSize <- getFixWord
    -- header[2 ... 11]: Optional; character coding scheme.
    characterCodingScheme <- B.G.isEmpty >>= \b -> if b
        then pure Nothing
        else Just <$> getBCPL characterCodingSchemeLength
    -- header[12 ... 16]: Optional; font family.
    family <- B.G.isEmpty >>= \b -> if b
        then pure Nothing
        else Just <$> getBCPL familyLength
    -- header[17]: Optional; seven-bit-safe-flag, and face code.
    (sevenBitSafeFlag, face) <- B.G.isEmpty >>= \b -> if b
            then pure (Nothing, Nothing)
            else
                do
                sevenBitSafeFlag <- B.G.getWord8
                _ <- B.G.getWord8
                _ <- B.G.getWord8
                face <- parseFace <$> B.G.getWord8
                pure (Just sevenBitSafeFlag, face)
    pure Header
        { checksum
        , designFontSize
        , characterCodingScheme
        , family
        , sevenBitSafeFlag
        , face
        }
