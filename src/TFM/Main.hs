module TFM.Main where

import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.Get as BSG
import qualified Data.IntMap.Strict as IntMap

import qualified Unit as U

import qualified TFM.Parse as TFMP
import qualified TFM.Character as TFMC

data TexFont = TexFont { checksum :: Int
                       , designFontSize :: Rational
                       , characterCodingScheme :: BS.ByteString
                       , family :: BS.ByteString

                       , slant :: Rational
                       , spacing :: Rational
                       , spaceStretch :: Rational
                       , spaceShrink :: Rational
                       , xHeight :: Rational
                       , quad :: Rational
                       , extraSpace :: Rational
                       , mathSymbolParams :: Maybe TFMP.MathSymbolParams
                       , mathExtensionParams :: Maybe TFMP.MathExtensionParams

                       , ligKerns :: [TFMP.LigKernInstr]
                       , characters :: IntMap.IntMap TFMC.Character }
             deriving (Show)

toScaledPoint :: TexFont -> Rational -> Int
toScaledPoint f = round . U.pointToScaledPoint . (designFontSize f *)

contentsToTFM :: BS.ByteString -> Either String TexFont
contentsToTFM contents = do
    meta <- fst $ BSG.runGet TFMP.newTFM contents
    headers <- fst $ BSG.runGet (TFMP.readHeader meta) contents
    fontParams <- fst $ BSG.runGet (TFMP.readFontParams meta headers) contents
    _ligKerns <- TFMP.readLigKerns meta contents
    _characters <- TFMC.readCharInfos meta contents
    return TexFont { checksum=TFMP.checksum headers
                   , designFontSize=TFMP.designFontSize headers
                   , characterCodingScheme=TFMP.characterCodingScheme headers
                   , family=TFMP.family headers

                   , slant=TFMP.slant fontParams
                   , spacing=TFMP.spacing fontParams
                   , spaceStretch=TFMP.spaceStretch fontParams
                   , spaceShrink=TFMP.spaceShrink fontParams
                   , xHeight=TFMP.xHeight fontParams
                   , quad=TFMP.quad fontParams
                   , extraSpace=TFMP.extraSpace fontParams
                   , mathSymbolParams=TFMP.mathSymbolParams fontParams
                   , mathExtensionParams=TFMP.mathExtensionParams fontParams

                   , ligKerns=_ligKerns
                   , characters=_characters }

readTFM :: String -> IO TexFont
readTFM path = do
    contents <- BS.readFile path
    either fail return $ contentsToTFM contents
