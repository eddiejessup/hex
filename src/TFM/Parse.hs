module TFM.Parse where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.Binary.Strict.Get as BSG
import qualified Data.Word as W
import qualified Data.Map as M
import Data.List.Split (chunksOf)
import Data.Bits ((.&.), shift)
import Data.Maybe (fromJust)
import qualified Control.Monad as CM

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

nrTables = length [Header ..]
-- The minimum length of the header.
headerDataLengthWordsMin = 18
-- The position where the header starts.
headerPointerBytes = 24
-- The length of the character coding scheme and font family sections,
-- respectively.
characterCodingSchemeLength = 40
familyLength = 20
-- The increment by which real numbers can be specified.
fixWordScale = 2 ^^ (-20)

-- Character coding schemes that should have the corresponding extra sets of
-- font parameters.
mathSymbolSchemes = map pack ["TeX math symbols"]
mathExtensionSchemes = map pack ["TeX math extension", "euler substitutions only"]

-- Convert a quantity in units of words, into the equivalent in bytes.
wordToByte :: (Num a) => (a -> a)
wordToByte = (*4)

-- Read a floating point value.
getFixWord :: BSG.Get Double
getFixWord = do
    nr <- BSG.getWord32be
    return ((fromIntegral nr) * fixWordScale)

-- Read integers encoded as big-endian byte sequences.
getWord16beInt = fmap fromIntegral BSG.getWord16be
getWord8Int = fmap fromIntegral BSG.getWord8

-- Read a string that's encoded as an integer, followed by that number of
-- characters.
getBCPL :: BSG.Get BS.ByteString
getBCPL = do
    len <- getWord8Int
    BSG.getByteString len

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

newTFM :: BSG.Get TFM
newTFM = do
    -- Read and set table lengths.
    fileLengthWords <- getWord16beInt
    headerDataLengthWordsRead <- getWord16beInt
    let headerDataLengthWords = max headerDataLengthWordsRead headerDataLengthWordsMin
    smallestCharCode <- getWord16beInt
    largestCharCode <- getWord16beInt

    -- Read the lengths of all tables after and including the 'Width' table.
    let laterTables = filter (>=Width) [minBound..]
    laterLengths <- mapM (\_ -> getWord16beInt) laterTables

    let nrChars = largestCharCode - smallestCharCode + 1

    -- Assemble the inferred lengths of the tables into maps from them, to
    -- their length and position.
    let tableLengthsWordsLst = (Header, headerDataLengthWords)
                               : (CharacterInfo, nrChars)
                               : zip laterTables laterLengths
    let tableLengthsWords = M.fromList(tableLengthsWordsLst)
    let tablePointers = M.mapWithKey (\tbl _ -> tablePos tableLengthsWords tbl) tableLengthsWords

    -- Check the inferred file length matches expectations.
    let validationFileLength = tablePointerEnd tablePointers tableLengthsWords maxBound
    CM.when (validationFileLength /= (wordToByte fileLengthWords)) $
        error ("Invalid TFM File " ++ (show validationFileLength) ++ " " ++ (show fileLengthWords))

    return TFM { fileLengthWords=fileLengthWords
               , headerDataLengthWords=headerDataLengthWords
               , smallestCharCode=smallestCharCode
               , largestCharCode=largestCharCode
               , tableLengthsWords=tableLengthsWords
               , tablePointers=tablePointers }

-- The information stored in the header table of a TFM file.
data Headers = Headers { checksum :: Int
                       , designFontSize :: Double
                       , characterCodingScheme :: BS.ByteString
                       , family :: BS.ByteString } deriving (Show)

readHeader :: TFM -> BSG.Get Headers
readHeader tfm = do
    BSG.skip $ tablePointerPos tfm Header

    -- Read header[0 ... 1], containing the checksum and design size.
    checksum <- fmap fromIntegral BSG.getWord32be
    designFontSize <- getFixWord
    -- Store our current position.
    postDesignPos <- BSG.bytesRead

    -- Get the position of the character info table, to orient ourselves.
    let charInfoPos = tablePointerPos tfm CharacterInfo

    -- Read header[2 ... 11] if present, containing the character coding
    -- scheme.
    -- If we haven't yet reached the character info table, then assume a
    -- character coding scheme is given.
    characterCodingScheme <- if postDesignPos < charInfoPos
        then getBCPL else return BS.empty
    -- Compute where the next section should start, and where we actually are,
    -- and skip past the difference.
    let postSchemePos = postDesignPos + characterCodingSchemeLength
    postSchemeNowPos <- BSG.bytesRead
    BSG.skip $ postSchemePos - postSchemeNowPos

    -- Read header[12 ... 16] if present, containing the font family.
    -- If we still haven't reached the character info table, then assume a
    -- font family is given.
    family <- if postSchemePos < charInfoPos
        then getBCPL else return BS.empty
    -- Again, skip past any remaining data until the next section.
    let postFamilyPos = postSchemePos + familyLength
    postFamilyNowPos <- BSG.bytesRead
    BSG.skip $ postFamilyPos - postFamilyNowPos

    -- Read header[12 ... 16] if present, containing random bits.
    -- We don't actually use these for anything now.
    if postFamilyPos < charInfoPos
        then sequence [ BSG.getWord8 -- Seven-bit safe flag.
                      , BSG.getWord8 -- Unknown.
                      , BSG.getWord8 ] -- 'Face'.
        else
            return []

    -- Don't read header [18 ... whatever]

    return Headers { checksum=checksum
                   , designFontSize=designFontSize
                   , characterCodingScheme=characterCodingScheme
                   , family=family }

-- Define some structures containing core parameters of the font, and some
-- optional extra sets of parameters.
data FontParams = FontParams { slant :: Double
                             , spacing :: Double
                             , spaceStretch :: Double
                             , spaceShrink :: Double
                             , xHeight :: Double
                             , quad :: Double
                             , extraSpace :: Double
                             , mathSymbolParams :: Maybe MathSymbolParams
                             , mathExtensionParams :: Maybe MathExtensionParams
                             } deriving (Show)
data MathSymbolParams = MathSymbolParams { num1 :: Double
                                         , num2 :: Double
                                         , num3 :: Double
                                         , denom1 :: Double
                                         , denom2 :: Double
                                         , sup1 :: Double
                                         , sup2 :: Double
                                         , sup3 :: Double
                                         , sub1 :: Double
                                         , sub2 :: Double
                                         , supdrop :: Double
                                         , subdrop :: Double
                                         , delim1 :: Double
                                         , delim2 :: Double
                                         , axisHeight :: Double } deriving (Show)
data MathExtensionParams = MathExtensionParams { defaultRuleThickness :: Double
                                               , bigOpSpacing :: [Double]
                                               } deriving (Show)

readMathSymbolParams :: BS.ByteString -> BSG.Get (Maybe MathSymbolParams)
readMathSymbolParams scheme = if scheme `elem` mathSymbolSchemes
        then do
            num1:num2:num3:denom1:denom2:sup1:sup2:sup3:sub1:sub2:supdrop:subdrop:delim1:delim2:axisHeight:[] <- CM.replicateM 15 getFixWord
            return $ Just MathSymbolParams { num1=num1
                                           , num2=num2
                                           , num3=num3
                                           , denom1=denom1
                                           , denom2=denom2
                                           , sup1=sup1
                                           , sup2=sup2
                                           , sup3=sup3
                                           , sub1=sub1
                                           , sub2=sub2
                                           , supdrop=supdrop
                                           , subdrop=subdrop
                                           , delim1=delim1
                                           , delim2=delim2
                                           , axisHeight=axisHeight
                                           }
        else return Nothing

readMathExtensionParams :: BS.ByteString -> BSG.Get (Maybe MathExtensionParams)
readMathExtensionParams scheme = if scheme `elem` mathExtensionSchemes
        then do
            defaultRuleThickness <- getFixWord
            bigOpSpacing <- CM.replicateM 5 getFixWord
            return $ Just MathExtensionParams { defaultRuleThickness=defaultRuleThickness
                                              , bigOpSpacing=bigOpSpacing
                                              }
        else return Nothing

readFontParams :: TFM -> Headers -> BSG.Get (Either String FontParams)
readFontParams tfm tfmHeads = do
    BSG.skip $ tablePointerPos tfm FontParameter
    let scheme = (characterCodingScheme tfmHeads)

    if scheme == (pack "TeX math italic")
        then return $ Left "Unsupported character coding scheme"
        else return $ Right ()

    extraSpace:quad:xHeight:spaceShrink:spaceStretch:spacing:slant:[] <- CM.replicateM 7 getFixWord

    -- Read parameters relating to math symbols and extensions, if present.
    mathSymbolParams <- readMathSymbolParams scheme
    mathExtensionParams <- readMathExtensionParams scheme

    return $ Right FontParams { slant=slant
                              , spacing=spacing
                              , spaceStretch=spaceStretch
                              , spaceShrink=spaceShrink
                              , xHeight=xHeight
                              , quad=quad
                              , extraSpace=extraSpace
                              , mathSymbolParams=mathSymbolParams
                              , mathExtensionParams=mathExtensionParams
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

kernOp =  128

data LigKernOp = LigatureOp { ligatureChar :: Int
                            , charsToPassOver :: Int
                            , deleteCurrentChar :: Bool
                            , deleteNextChar :: Bool
                            }
                | KernOp { size :: Double }
                deriving (Show)

data LigKernInstr = LigKernInstr { stop :: Bool
                                 , nextChar :: Int
                                 , operation :: LigKernOp } deriving (Show)

readInstrSet :: [[Int]] -> Int -> (Int, Int, Int, Int)
readInstrSet sets i =
    let
        w:x:y:z:[] = sets !! i
    in (w, x, y, z)

getLigKernOperation :: (Int -> Either String LigKernOp) -> Int -> Int -> Either String LigKernOp
getLigKernOperation readKern op remain =
    if op >= kernOp
        then readKern $ 256 * (op - kernOp) + remain
        else Right LigatureOp { ligatureChar=remain
                            , charsToPassOver=op `shift` 2
                            , deleteCurrentChar=(op .&. 0x02) == 0
                            , deleteNextChar=(op .&. 0x01) == 0
                            }

analyzeLigKernInstr :: (Int -> Either String LigKernOp) -> Int -> Int -> Int -> Int -> Either String LigKernInstr
analyzeLigKernInstr readKern skip next op remain =
    let operation = getLigKernOperation readKern op remain
    in case operation of Left s -> Left s
                         Right o -> Right LigKernInstr { stop=skip >= 128
                                                  , nextChar=next
                                                  , operation=o }

getKern :: TFM -> BS.ByteString -> (Int -> Either String LigKernOp)
getKern tfm contents iWords =
    let
        kernTblPos = tablePointerPos tfm Kern
        kernPos = kernTblPos + wordToByte iWords
        kernContents = BS.drop kernPos contents
        (size, _) = BSG.runGet getFixWord kernContents
    in
        case size of Left s -> Left s
                     Right s -> Right KernOp { size=s }


readLigKerns :: TFM -> BS.ByteString -> Either String [Either String LigKernInstr]
readLigKerns tfm contents
    | firstSkipByte == 255 = Left "Sorry, right boundary characters are not supported"
    | firstSkipByte > 128 = Left "Sorry, large LigKern arrays are not supported"
    | lastSkipByte == 255 = Left "Sorry, left boundary characters are not supported"
    | otherwise = Right ligKerns
    where
        ligKernTblPos = tablePointerPos tfm LigKern
        contentsLigKernOnwards = BS.drop ligKernTblPos contents
        ligKernBytes = BS.unpack contentsLigKernOnwards
        ligKernInts = fmap fromIntegral ligKernBytes
        ligKernIntSets = chunksOf 4 ligKernInts
        readSet = readInstrSet ligKernIntSets

        (firstSkipByte, _, _, _) = readSet 0

        ligKernTblLengthWords = tableLength tfm LigKern
        (lastSkipByte, _, _, _) = readSet (ligKernTblLengthWords - 1)

        kernGetter = getKern tfm contents
        sets = fmap readSet [0..ligKernTblLengthWords - 1]
        ligKerns = fmap (\(a, b, c, d) -> analyzeLigKernInstr kernGetter a b c d) sets
