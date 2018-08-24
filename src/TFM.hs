module TFM where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Binary.Get as BG
import qualified Data.HashMap.Strict as HashMap
import Path (Path, Abs, File, toFilePath)

import qualified HeX.Unit as U

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
                       , characters :: HashMap.HashMap Int TFMC.Character }
             deriving (Show)

designSizeSP :: TexFont -> Rational
designSizeSP f = U.toScaledPoint (designFontSize f) U.Point

designScaleSP :: TexFont -> Rational -> Int
designScaleSP f x = round $ designSizeSP f * x

contentsToTFM :: BLS.ByteString -> TexFont
contentsToTFM contents =
    let
      meta = BG.runGet TFMP.newTFM contents
      headers = BG.runGet (TFMP.readHeader meta) contents
      fontParams = BG.runGet (TFMP.readFontParams meta headers) contents
      _ligKerns = TFMP.readLigKerns meta contents
      _characters = TFMC.readCharInfos meta contents
    in TexFont { checksum=TFMP.checksum headers
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

readTFM :: FilePath -> IO TexFont
readTFM path = do
    contents <- BLS.readFile path
    return $ contentsToTFM contents

readTFMFancy :: Path Abs File -> IO TexFont
readTFMFancy = readTFM . toFilePath
