module TFM
    ( module TFM.Parse
    , designSizeSP
    , designScaleSP
    , readTFM
    , readTFMFancy
    )
where

import           Prelude                 hiding ( readFile )

import           Data.Binary.Get
import           Data.ByteString.Lazy
import           HeX.Unit
import           Path

import           TFM.Parse

designSizeSP :: TexFont -> Rational
designSizeSP f = toScaledPoint (designFontSize f) Point

designScaleSP :: TexFont -> Rational -> Int
designScaleSP f x = round $ designSizeSP f * x

readTFM :: FilePath -> IO TexFont
readTFM path = do
    contents <- readFile path
    pure $ runGet newTFM contents

readTFMFancy :: Path Abs File -> IO TexFont
readTFMFancy = readTFM . toFilePath
