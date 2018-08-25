module TFM where

import Prelude hiding (readFile)

import Data.ByteString.Lazy
import Data.Binary.Get
import Path
import HeX.Unit

import TFM.Parse

designSizeSP :: TexFont -> Rational
designSizeSP f = toScaledPoint (designFontSize f) Point

designScaleSP :: TexFont -> Rational -> Int
designScaleSP f x = round $ designSizeSP f * x

readTFM :: FilePath -> IO TexFont
readTFM path = do
    contents <- readFile path
    return $ runGet newTFM contents

readTFMFancy :: Path Abs File -> IO TexFont
readTFMFancy = readTFM . toFilePath
