module TFM
    ( module TFM.Parse
    , designSizeSP
    , designScaleSP
    , readTFM
    , readTFMFancy
    , Character(..)
    )
where

import           HeXlude

import qualified Data.ByteString as BS
import           HeX.Unit        (PhysicalUnit (..), toScaledPoint)
import           Path

import           TFM.Character   (Character (..))
import           TFM.Parse

designSizeSP :: TexFont -> Rational
designSizeSP f = toScaledPoint (designFontSize f) Point

designScaleSP :: TexFont -> Rational -> Int
designScaleSP f x = round $ designSizeSP f * x

readTFM :: FilePath -> IO (Either Text TexFont)
readTFM path = newTFM <$> BS.readFile path

readTFMFancy :: Path Abs File -> IO (Either Text TexFont)
readTFMFancy = readTFM . toFilePath
