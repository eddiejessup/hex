module TFM.Parse where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.Binary.Strict.Get as BSG
import qualified Data.Word as W
import qualified Data.Map as M
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
