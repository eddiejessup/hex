module TFM
  ( module TFM.Parse
  , designSizeSP
  , designScaleSP
  , readTFM
  , readTFMFancy
  , TFMError (..)
  , Character (..)
  )
where

import qualified Data.ByteString as BS
import Hex.Quantity
import Hexlude
import Path
import TFM.Character (Character (..))
import TFM.Parse

newtype TFMError = TFMError Text
  deriving stock Show

designSizeRational :: TexFont -> Rational
designSizeRational f = toScaledPoint (designFontSize f) Point

designSizeSP :: TexFont -> Length
designSizeSP f = round $ designSizeRational f

designScaleSP :: TexFont -> Rational -> Length
designScaleSP f x = round $ designSizeRational f * x

readTFM :: (MonadIO m, MonadError e m, AsType TFMError e) => FilePath -> m TexFont
readTFM path =
  liftIO (BS.readFile path) <&> newTFM >>= \case
    Left err -> throwError $ injectTyped $ TFMError err
    Right v -> pure v

readTFMFancy :: (MonadIO m, MonadError e m, AsType TFMError e) => Path Abs File -> m TexFont
readTFMFancy = readTFM . toFilePath
