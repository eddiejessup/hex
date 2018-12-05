module TFM.Table where

import           Control.Monad
import           Data.Binary

import           TFM.Common

headerDataLengthWordsMin, headerPointerWords, headerPointerBytes :: Int
-- The minimum length of the header.
headerDataLengthWordsMin = 18

-- The position where the header starts.
headerPointerWords = 6

headerPointerBytes = wordToByte headerPointerWords

-- From a map from each table to their length, infer a particular table's
-- position.
tablePosBytes :: (Table -> Int) -> Table -> Int
-- Base case: The header starts at a fixed position.
tablePosBytes _ Header = headerPointerBytes
-- Otherwise,
tablePosBytes tblLengthsWords tbl =
  let prevTbl = pred tbl
      prevPos = tablePosBytes tblLengthsWords prevTbl
      prevLen = wordToByte $ tblLengthsWords prevTbl
    -- The table's position is the previous table's position, plus that
    -- table's length.
  in prevPos + prevLen

-- Sections within the file containing different bits of information.
data Table
  = Header
  | CharacterInfo
  | Width
  | Height
  | Depth
  | ItalicCorrection
  | LigKerns
  | Kern
  | ExtensibleCharacter
  | FontParameter
  deriving (Show, Eq, Enum, Ord, Bounded)

getTableParams :: Get (Table -> Int, Int, Int)
getTableParams
    -- Read and set table lengths.
 = do
  _fileLengthWords <- getWord16beInt
  _headerDataLengthWords <- max headerDataLengthWordsMin <$> getWord16beInt
  _smallestCharCode <- getWord16beInt
  _largestCharCode <- getWord16beInt
    -- Read the lengths of all tables after and including the 'Width' table.
  widthDataLengthWords <- getWord16beInt
  heightDataLengthWords <- getWord16beInt
  depthDataLengthWords <- getWord16beInt
  italicCorrectionDataLengthWords <- getWord16beInt
  ligKernDataLengthWords <- getWord16beInt
  kernDataLengthWords <- getWord16beInt
  extensibleCharDataLengthWords <- getWord16beInt
  fontParamDataLengthWords <- getWord16beInt
  let tableToLength Header = _headerDataLengthWords
      tableToLength CharacterInfo = _largestCharCode - _smallestCharCode + 1
      tableToLength Width = widthDataLengthWords
      tableToLength Height = heightDataLengthWords
      tableToLength Depth = depthDataLengthWords
      tableToLength ItalicCorrection = italicCorrectionDataLengthWords
      tableToLength LigKerns = ligKernDataLengthWords
      tableToLength Kern = kernDataLengthWords
      tableToLength ExtensibleCharacter = extensibleCharDataLengthWords
      tableToLength FontParameter = fontParamDataLengthWords
      inferredFileLengthWords =
        headerPointerWords + sum (fmap tableToLength [minBound ..])
  when
    (_fileLengthWords /= inferredFileLengthWords)
    (fail
       ("Incorrect table lengths: read " ++
        show _fileLengthWords ++
        " is not equal to inferred " ++ show inferredFileLengthWords))
  pure (tableToLength, _smallestCharCode, _largestCharCode)
