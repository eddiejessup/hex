module TFM
    ( module TFM.Parse
    , designSizeSP
    , designScaleSP
    , readTFM
    , readTFMFancy
    , TFMError(..)
    , Character(..)
    )
where

import           HeXlude

import qualified Data.ByteString as BS
import           HeX.Unit        (PhysicalUnit (..), toScaledPoint)
import           Path

import           TFM.Character   (Character (..))
import           TFM.Parse

newtype TFMError = TFMError Text
    deriving (Show)

designSizeSP :: TexFont -> Rational
designSizeSP f = toScaledPoint (designFontSize f) Point

designScaleSP :: TexFont -> Rational -> Int
designScaleSP f x = round $ designSizeSP f * x

readTFM
    :: ( MonadIO m
       , MonadErrorAnyOf e m '[TFMError]
       )
    => FilePath
    -> m TexFont
readTFM path =
    liftIO (BS.readFile path) <&> newTFM >>= \case
        Left err -> throwM $ TFMError err
        Right v -> pure v

readTFMFancy
    :: ( MonadIO m
       , MonadErrorAnyOf e m '[TFMError]
       )
    => Path Abs File
    -> m TexFont
readTFMFancy = readTFM . toFilePath
