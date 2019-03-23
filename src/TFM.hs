module TFM
    ( module TFM.Parse
    , designSizeSP
    , designScaleSP
    , readTFM
    , readTFMFancy
    , Character(..)
    )
where

import HeXlude

import qualified Data.ByteString               as BS
import           Path
import           HeX.Unit                       ( toScaledPoint
                                                , PhysicalUnit(..)
                                                )

import           TFM.Parse
import           TFM.Character                  ( Character(..) )

designSizeSP :: TexFont -> Rational
designSizeSP f = toScaledPoint (designFontSize f) Point

designScaleSP :: TexFont -> Rational -> Int
designScaleSP f x = round $ designSizeSP f * x

readTFM :: FilePath -> IO (Either Text TexFont)
readTFM path = newTFM <$> BS.readFile path

readTFMFancy :: Path Abs File -> IO (Either Text TexFont)
readTFMFancy = readTFM . toFilePath
