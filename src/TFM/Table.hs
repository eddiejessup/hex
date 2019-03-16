module TFM.Table where

import           Control.Monad
import           Data.Binary.Get ( Get, getByteString )
import           Data.ByteString ( ByteString )

import           TFM.Common

headerDataLengthWordsMin, headerPointerWords, headerPointerBytes :: Int
-- The minimum length of the header.
headerDataLengthWordsMin = 18
-- The position where the header starts.
headerPointerWords = 6
headerPointerBytes = wordToByte headerPointerWords

-- Sections within the file containing different bits of information.
data Table
    = Header
    | CharacterInfo
    | Width
    | Height
    | Depth
    | ItalicCorrection
    | LigKern
    | Kern
    | ExtensibleRecipe
    | FontParameter
    deriving (Show, Eq, Enum, Ord, Bounded)

data TableParams = TableParams
    { tableToString :: Table -> ByteString
    , smallestCharCode, largestCharCode :: Int
    } deriving Show

instance Show (Table -> ByteString) where
    show _ = "Table -> ByteString"

getTableParams :: Get TableParams
getTableParams
   = do
    -- Read and set table lengths.
    fileLengthWords <- getWord16beInt
    headerDataLengthWords <- max headerDataLengthWordsMin <$> getWord16beInt
    _smallestCharCode <- getWord16beInt
    _largestCharCode <- getWord16beInt
    -- Read the lengths of all tables after and including the 'Width' table.
    widthDataLengthWords <- getWord16beInt
    heightDataLengthWords <- getWord16beInt
    depthDataLengthWords <- getWord16beInt
    italicCorrectionDataLengthWords <- getWord16beInt
    ligKernDataLengthWords <- getWord16beInt
    kernDataLengthWords <- getWord16beInt
    extensibleRecipeDataLengthWords <- getWord16beInt
    fontParamDataLengthWords <- getWord16beInt
    let characterInfoDataLengthWords = _largestCharCode - _smallestCharCode + 1
        inferredFileLengthWords =
            headerPointerWords
            + headerDataLengthWords
            + characterInfoDataLengthWords
            + widthDataLengthWords
            + heightDataLengthWords
            + depthDataLengthWords
            + italicCorrectionDataLengthWords
            + ligKernDataLengthWords
            + kernDataLengthWords
            + extensibleRecipeDataLengthWords
            + fontParamDataLengthWords
    when (fileLengthWords /= inferredFileLengthWords)
        $ fail $ "Incorrect table lengths: read "
            ++ show fileLengthWords
            ++ " is not equal to inferred "
            ++ show (headerDataLengthWords, (_largestCharCode, _smallestCharCode), widthDataLengthWords, heightDataLengthWords, depthDataLengthWords, italicCorrectionDataLengthWords, ligKernDataLengthWords, kernDataLengthWords, extensibleRecipeDataLengthWords, fontParamDataLengthWords)

    let getByteStringWords = getByteString . wordToByte

    headerStr <- getByteStringWords headerDataLengthWords
    characterInfoStr <- getByteStringWords characterInfoDataLengthWords
    widthStr <- getByteStringWords widthDataLengthWords
    heightStr <- getByteStringWords heightDataLengthWords
    depthStr <- getByteStringWords depthDataLengthWords
    italicCorrectionStr <- getByteStringWords italicCorrectionDataLengthWords
    ligKernsStr <- getByteStringWords ligKernDataLengthWords
    kernStr <- getByteStringWords kernDataLengthWords
    extensibleRecipeStr <- getByteStringWords extensibleRecipeDataLengthWords
    fontParameterStr <- getByteStringWords fontParamDataLengthWords

    let _tableToString = \case
            Header           -> headerStr
            CharacterInfo    -> characterInfoStr
            Width            -> widthStr
            Height           -> heightStr
            Depth            -> depthStr
            ItalicCorrection -> italicCorrectionStr
            LigKern          -> ligKernsStr
            Kern             -> kernStr
            ExtensibleRecipe -> extensibleRecipeStr
            FontParameter    -> fontParameterStr

    pure $ TableParams _tableToString _smallestCharCode _largestCharCode
