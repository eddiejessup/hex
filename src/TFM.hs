module TFM
    ( module TFM.Parse
    , designSizeSP
    , designScaleSP
    , readTFM
    , readTFMFancy
    )
where

import qualified Data.ByteString               as BS
import           Path
import           HeX.Unit                       ( toScaledPoint
                                                , PhysicalUnit(..)
                                                )

import           TFM.Parse

designSizeSP :: TexFont -> Rational
designSizeSP f = toScaledPoint (designFontSize f) Point

designScaleSP :: TexFont -> Rational -> Int
designScaleSP f x = round $ designSizeSP f * x

readTFM :: FilePath -> IO TexFont
readTFM path =
    do
    contents <- BS.readFile path
    case newTFM contents of
        Left err -> ioError $ userError err
        Right tfm -> pure tfm

readTFMFancy :: Path Abs File -> IO TexFont
readTFMFancy = readTFM . toFilePath
