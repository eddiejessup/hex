module TFM.Parse where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS
import Data.ByteString.Char8 (pack)
import qualified Data.Binary.Get as BG
import qualified Data.Map as M
import Data.List.Split (chunksOf)
import Data.Bits ((.&.), shift)
import Data.Maybe (fromJust)
import qualified Control.Monad as CM
import Data.Ratio ((%))

-- Sections within the file containing different bits of information.
data Table =
      Header
    | CharacterInfo
    | Width
    | Height
    | Depth
    | ItalicCorrection
    | LigKern
    | Kern
    | ExtensibleCharacter
    | FontParameter
    deriving (Show, Eq, Enum, Ord, Bounded)

-- The minimum length of the header.
headerDataLengthWordsMin :: Int
headerDataLengthWordsMin = 18
-- The position where the header starts.
headerPointerBytes :: Int
headerPointerBytes = 24
-- The length of the character coding scheme and font family sections,
-- respectively.
characterCodingSchemeLength :: Int
characterCodingSchemeLength = 40
familyLength :: Int
familyLength = 20
-- The increment by which real numbers can be specified.
fixWordScale :: Rational
fixWordScale = 1 % (2 ^ (20 :: Integer))

-- Character coding schemes that should have the corresponding extra sets of
-- font parameters.
mathSymbolSchemes :: [BS.ByteString]
mathSymbolSchemes = fmap pack ["TeX math symbols"]
mathExtensionSchemes :: [BS.ByteString]
mathExtensionSchemes = fmap pack ["TeX math extension", "euler substitutions only"]

-- Convert a quantity in units of words, into the equivalent in bytes.
wordToByte :: (Num a) => a -> a
wordToByte = (*4)

-- Read a floating point value.
getFixWord :: BG.Get Rational
getFixWord = do
    nr <- BG.getWord32be
    return $ fromIntegral nr * fixWordScale

-- Read integers encoded as big-endian byte sequences.
getWord32beInt :: BG.Get Int
getWord32beInt = fmap fromIntegral BG.getWord32be
getWord16beInt :: BG.Get Int
getWord16beInt = fmap fromIntegral BG.getWord16be
getWord8Int :: BG.Get Int
getWord8Int = fmap fromIntegral BG.getWord8

-- Read a string that's encoded as an integer, followed by that number of
-- characters.
getBCPL :: BG.Get BS.ByteString
getBCPL = do
    len <- getWord8Int
    BG.getByteString len

get4Word8Ints :: BG.Get (Int, Int, Int, Int)
get4Word8Ints = do
    [b1, b2, b3, b4] <- CM.replicateM 4 getWord8Int
    return (b1, b2, b3, b4)

getPos :: TFM -> Table -> Int -> Int
getPos tfm tbl iWords = tablePointerPos tfm tbl + wordToByte iWords

getSubContents :: TFM -> Table -> Int -> BLS.ByteString -> BLS.ByteString
getSubContents tfm tbl iWords s = BLS.fromStrict $ BS.drop (getPos tfm tbl iWords) $ BLS.toStrict s

getFixWordAt :: TFM -> BLS.ByteString -> Table -> Int -> Rational
getFixWordAt tfm contents tbl iWords =
    BG.runGet getFixWord $ getSubContents tfm tbl iWords contents

get4Word8IntsAt :: TFM -> BLS.ByteString -> Table -> Int -> (Int, Int, Int, Int)
get4Word8IntsAt tfm contents tbl iWords =
    BG.runGet get4Word8Ints $ getSubContents tfm tbl iWords contents

-- Utility to simplify lookup of integers from a map.
certainIntLookup :: (Integral b, Ord a) => M.Map a b -> a -> Int
certainIntLookup tmap tbl = fromIntegral $ fromJust $ M.lookup tbl tmap

-- From a map from each table to their length, infer a particular table's
-- position.
tablePos :: M.Map Table Int -> Table -> Int
-- Base case: The header starts at a fixed position.
tablePos _ Header = headerPointerBytes
-- Otherwise,
tablePos tblLengthsWords tbl =
    let
        prevTbl = pred tbl
        prevPos = tablePos tblLengthsWords prevTbl
        prevLen = wordToByte $ certainIntLookup tblLengthsWords prevTbl
    in
        -- The table's position is the previous table's position, plus that
        -- table's length.
        prevPos + prevLen

-- With a map from each table to its starting position, and a map from each
-- table to its length, infer the position of the end of a particular table.
tablePointerEnd :: (Integral a) => M.Map Table a -> M.Map Table a -> Table -> Int
tablePointerEnd pointers wlengths table =
    let
        pos = certainIntLookup pointers table
        len = wordToByte $ certainIntLookup wlengths table
    in
        pos + len

-- Similar utilities to above, but inferring table attributes using a TFM
-- object.

tablePointerPos :: TFM -> (Table -> Int)
tablePointerPos tfm = certainIntLookup (tablePointers tfm)

tableLength :: TFM -> (Table -> Int)
tableLength tfm = certainIntLookup (tableLengthsWords tfm)

tablePointerEndTFM :: TFM -> Table -> Int
tablePointerEndTFM tfm = tablePointerEnd (tablePointers tfm) (tableLengthsWords tfm)

-- Basic information about a TeX Font Metric file.
data TFM = TFM { fileLengthWords :: Int
               , headerDataLengthWords :: Int
               , smallestCharCode :: Int
               , largestCharCode :: Int
               , tableLengthsWords :: M.Map Table Int
               , tablePointers :: M.Map Table Int } deriving (Show)

newTFM :: BG.Get TFM
newTFM = do
    -- Read and set table lengths.
    _fileLengthWords <- getWord16beInt
    headerDataLengthWordsRead <- getWord16beInt
    let _headerDataLengthWords = max headerDataLengthWordsRead headerDataLengthWordsMin
    _smallestCharCode <- getWord16beInt
    _largestCharCode <- getWord16beInt

    -- Read the lengths of all tables after and including the 'Width' table.
    let laterTables = filter (>=Width) [minBound..]
    laterLengths <- mapM (const getWord16beInt) laterTables

    let nrChars = _largestCharCode - _smallestCharCode + 1

        -- Assemble the inferred lengths of the tables into maps from them, to
        -- their length and position.
        tableLengthsWordsLst = (Header, _headerDataLengthWords)
                               : (CharacterInfo, nrChars)
                               : zip laterTables laterLengths
        _tableLengthsWords = M.fromList tableLengthsWordsLst
        _tablePointers = M.mapWithKey (\tbl _ -> tablePos _tableLengthsWords tbl) _tableLengthsWords

        -- Check the inferred file length matches expectations.
        validationFileLength = tablePointerEnd _tablePointers _tableLengthsWords maxBound

    CM.when (validationFileLength /= wordToByte _fileLengthWords) $
        fail $ "Invalid TFM File " ++ show validationFileLength ++ " " ++ show _fileLengthWords

    return TFM { fileLengthWords=_fileLengthWords
               , headerDataLengthWords=_headerDataLengthWords
               , smallestCharCode=_smallestCharCode
               , largestCharCode=_largestCharCode
               , tableLengthsWords=_tableLengthsWords
               , tablePointers=_tablePointers }

-- The information stored in the header table of a TFM file.
data Headers = Headers { checksum :: Int
                       , designFontSize :: Rational
                       , characterCodingScheme :: BS.ByteString
                       , family :: BS.ByteString } deriving (Show)

readHeader :: TFM -> BG.Get Headers
readHeader tfm = do
    BG.skip $ tablePointerPos tfm Header

    -- Read header[0 ... 1], containing the checksum and design size.
    _checksum <- getWord32beInt
    _designFontSize <- getFixWord
    -- Store our current position.
    postDesignPos <- fromIntegral <$> BG.bytesRead

    -- Get the position of the character info table, to orient ourselves.
    let charInfoPos = tablePointerPos tfm CharacterInfo

    -- Read header[2 ... 11] if present, containing the character coding
    -- scheme.
    -- If we haven't yet reached the character info table, then assume a
    -- character coding scheme is given.
    _characterCodingScheme <- if postDesignPos < charInfoPos
        then getBCPL else return BS.empty
    -- Compute where the next section should start, and where we actually are,
    -- and skip past the difference.
    let postSchemePos = postDesignPos + characterCodingSchemeLength
    postSchemeNowPos <- fromIntegral <$> BG.bytesRead
    BG.skip $ postSchemePos - postSchemeNowPos

    -- Read header[12 ... 16] if present, containing the font family.
    -- If we still haven't reached the character info table, then assume a
    -- font family is given.
    _family <- if postSchemePos < charInfoPos
        then getBCPL else return BS.empty
    -- Again, skip past any remaining data until the next section.
    let postFamilyPos = postSchemePos + familyLength
    postFamilyNowPos <- fromIntegral <$> BG.bytesRead
    BG.skip $ postFamilyPos - postFamilyNowPos

    -- Read header[12 ... 16] if present, containing random bits.
    -- We don't actually use these for anything now.
    _ <- if postFamilyPos < charInfoPos
        then sequence [ BG.getWord8 -- Seven-bit safe flag.
                      , BG.getWord8 -- Unknown.
                      , BG.getWord8 ] -- 'Face'.
        else
            return []

    -- Don't read header [18 ... whatever]

    return Headers { checksum=_checksum
                   , designFontSize=_designFontSize
                   , characterCodingScheme=_characterCodingScheme
                   , family=_family }

-- Define some structures containing core parameters of the font, and some
-- optional extra sets of parameters.
data FontParams = FontParams { slant :: Rational
                             , spacing :: Rational
                             , spaceStretch :: Rational
                             , spaceShrink :: Rational
                             , xHeight :: Rational
                             , quad :: Rational
                             , extraSpace :: Rational
                             , mathSymbolParams :: Maybe MathSymbolParams
                             , mathExtensionParams :: Maybe MathExtensionParams
                             } deriving (Show)
data MathSymbolParams = MathSymbolParams { num1 :: Rational
                                         , num2 :: Rational
                                         , num3 :: Rational
                                         , denom1 :: Rational
                                         , denom2 :: Rational
                                         , sup1 :: Rational
                                         , sup2 :: Rational
                                         , sup3 :: Rational
                                         , sub1 :: Rational
                                         , sub2 :: Rational
                                         , supdrop :: Rational
                                         , subdrop :: Rational
                                         , delim1 :: Rational
                                         , delim2 :: Rational
                                         , axisHeight :: Rational } deriving (Show)
data MathExtensionParams = MathExtensionParams { defaultRuleThickness :: Rational
                                               , bigOpSpacing :: [Rational]
                                               } deriving (Show)

readMathSymbolParams :: BS.ByteString -> BG.Get (Maybe MathSymbolParams)
readMathSymbolParams scheme =
    if scheme `elem` mathSymbolSchemes then do
        [_num1, _num2, _num3, _denom1, _denom2, _sup1, _sup2, _sup3, _sub1, _sub2,
         _supdrop, _subdrop, _delim1, _delim2, _axisHeight] <- CM.replicateM 15 getFixWord
        return $ Just MathSymbolParams { num1=_num1
                                       , num2=_num2
                                       , num3=_num3
                                       , denom1=_denom1
                                       , denom2=_denom2
                                       , sup1=_sup1
                                       , sup2=_sup2
                                       , sup3=_sup3
                                       , sub1=_sub1
                                       , sub2=_sub2
                                       , supdrop=_supdrop
                                       , subdrop=_subdrop
                                       , delim1=_delim1
                                       , delim2=_delim2
                                       , axisHeight=_axisHeight
                                       }
    else return Nothing

readMathExtensionParams :: BS.ByteString -> BG.Get (Maybe MathExtensionParams)
readMathExtensionParams scheme = if scheme `elem` mathExtensionSchemes
    then do
        _defaultRuleThickness <- getFixWord
        _bigOpSpacing <- CM.replicateM 5 getFixWord
        return $ Just MathExtensionParams { defaultRuleThickness=_defaultRuleThickness
                                          , bigOpSpacing=_bigOpSpacing
                                          }
    else return Nothing

readFontParams :: TFM -> Headers -> BG.Get FontParams
readFontParams tfm tfmHeads = do
    BG.skip $ tablePointerPos tfm FontParameter
    let scheme = characterCodingScheme tfmHeads
    CM.when (scheme == pack "TeX math italic") $
      fail "Unsupported character coding scheme"

    [_slant, _spacing, _spaceStretch, _spaceShrink, _xHeight, _quad, _extraSpace] <- CM.replicateM 7 getFixWord

    -- Read parameters relating to math symbols and extensions, if present.
    _mathSymbolParams <- readMathSymbolParams scheme
    _mathExtensionParams <- readMathExtensionParams scheme

    return FontParams { slant=_slant
                              , spacing=_spacing
                              , spaceStretch=_spaceStretch
                              , spaceShrink=_spaceShrink
                              , xHeight=_xHeight
                              , quad=_quad
                              , extraSpace=_extraSpace
                              , mathSymbolParams=_mathSymbolParams
                              , mathExtensionParams=_mathExtensionParams
                              }


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
kernOp =  128

data LigKernOp = LigatureOp { ligatureChar :: Int
                            , charsToPassOver :: Int
                            , deleteCurrentChar :: Bool
                            , deleteNextChar :: Bool
                            }
                | KernOp { size :: Rational }
                deriving (Show)

data LigKernInstr = LigKernInstr { stop :: Bool
                                 , nextChar :: Int
                                 , operation :: LigKernOp } deriving (Show)

readInstrSet :: [[Int]] -> Int -> (Int, Int, Int, Int)
readInstrSet sets i =
    let
        [w, x, y, z] = sets !! i
    in (w, x, y, z)

getLigKernOperation :: (Int -> LigKernOp) -> Int -> Int -> LigKernOp
getLigKernOperation readKern op remain =
    if op >= kernOp
        then readKern $ 256 * (op - kernOp) + remain
        else LigatureOp { ligatureChar=remain
                        , charsToPassOver=op `shift` 2
                        , deleteCurrentChar=(op .&. 0x02) == 0
                        , deleteNextChar=(op .&. 0x01) == 0
                        }

analyzeLigKernInstr :: (Int -> LigKernOp) -> Int -> Int -> Int -> Int -> LigKernInstr
analyzeLigKernInstr readKern skip next op remain =
    let _operation = getLigKernOperation readKern op remain
    in LigKernInstr { stop=skip >= 128
                    , nextChar=next
                    , operation=_operation }

getKern :: TFM -> BLS.ByteString -> (Int -> LigKernOp)
getKern tfm contents iWords =
    let _size = getFixWordAt tfm contents Kern iWords
    in KernOp { size=_size }


readLigKerns :: TFM -> BLS.ByteString -> [LigKernInstr]
readLigKerns tfm contents
    | firstSkipByte == 255 = error "Sorry, right boundary characters are not supported"
    | firstSkipByte > 128 = error "Sorry, large LigKern arrays are not supported"
    | lastSkipByte == 255 = error "Sorry, left boundary characters are not supported"
    | otherwise =
        let kernGetter = getKern tfm contents
        in fmap (\(a, b, c, d) -> analyzeLigKernInstr kernGetter a b c d) sets
    where
        ligKernTblPos = tablePointerPos tfm LigKern
        ligKernTblLengthWords = tableLength tfm LigKern

        contentsLigKernOnwards = BS.drop ligKernTblPos $ BLS.toStrict contents
        instrSets = chunksOf 4 . fmap fromIntegral $ BS.unpack contentsLigKernOnwards
        readSet = readInstrSet instrSets
        sets = fmap readSet [0..ligKernTblLengthWords - 1]

        (firstSkipByte, _, _, _) = head sets
        (lastSkipByte, _, _, _) = last sets
