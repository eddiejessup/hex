module TFM.Parse
  ( TexFont(..)
  , newTFM
  )
where

import qualified Data.Binary.Get               as BG
import           Data.ByteString
import           Data.HashMap.Strict

import           TFM.Character
import           TFM.Common
import qualified TFM.FontParams                as F
import qualified TFM.Header                    as H
import           TFM.LigKern
import           TFM.Table                      ( Table(..)
                                                , getTableParams
                                                , tablePosBytes
                                                )

data TexFont = TexFont
    { checksum              :: Int
    , designFontSize        :: Rational
    , characterCodingScheme
    , family                :: ByteString
    , slant
    , spacing
    , spaceStretch
    , spaceShrink
    , xHeight
    , quad
    , extraSpace            :: Rational
    , mathSymbolParams      :: Maybe F.MathSymbolParams
    , mathExtensionParams   :: Maybe F.MathExtensionParams
    , ligKerns              :: [LigKernInstr]
    , characters            :: HashMap Char Character
    } deriving (Show)

newTFM :: BG.Get TexFont
newTFM =
    do
    -- Read and set table lengths.
    (tableToLength, _smallestCharCode, _largestCharCode) <- getTableParams
    let skipUpTo tbl =
            do
            curPos <- fromIntegral <$> BG.bytesRead
            BG.skip $ tablePosBytes tableToLength tbl - curPos
    skipUpTo Header
    header <- H.getHeader (tablePosBytes tableToLength CharacterInfo)
    let readTableStr = BG.getByteString . wordToByte . tableToLength
    skipUpTo CharacterInfo
    cInfoStr <- readTableStr CharacterInfo
    wStr <- readTableStr Width
    hStr <- readTableStr Height
    dStr <- readTableStr Depth
    iStr <- readTableStr ItalicCorrection
    ligKernStr <- readTableStr LigKerns
    kernStr <- readTableStr Kern
    extStr <- readTableStr ExtensibleCharacter
    skipUpTo FontParameter
    fontParams <- F.getFontParams $ H.characterCodingScheme header
    let _characters = readCharInfos
            _smallestCharCode
            _largestCharCode
            cInfoStr
            extStr
            wStr
            hStr
            dStr
            iStr
        _ligKerns = readLigKerns ligKernStr kernStr
    pure TexFont
        { checksum              = H.checksum header
        , designFontSize        = H.designFontSize header
        , characterCodingScheme = H.characterCodingScheme header
        , family                = H.family header
        , slant                 = F.slant fontParams
        , spacing               = F.spacing fontParams
        , spaceStretch          = F.spaceStretch fontParams
        , spaceShrink           = F.spaceShrink fontParams
        , xHeight               = F.xHeight fontParams
        , quad                  = F.quad fontParams
        , extraSpace            = F.extraSpace fontParams
        , mathSymbolParams      = F.mathSymbolParams fontParams
        , mathExtensionParams   = F.mathExtensionParams fontParams
        , ligKerns              = _ligKerns
        , characters            = _characters
        }
