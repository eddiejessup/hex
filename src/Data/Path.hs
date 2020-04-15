module Data.Path where

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Word as Word
import Hexlude
import Path (File, Path)
import qualified Path

newtype PathError = PathError Text
  deriving stock (Show, Generic)

readPathText :: Path a File -> IO Text
readPathText = Path.toFilePath >>> readFile

readPathBytes :: Path a File -> IO (Seq Word.Word8)
readPathBytes = Path.toFilePath >>> BS.readFile >>> fmap (BS.unpack >>> Seq.fromList)

stripExtension
  :: (MonadError e m, AsType PathError e)
  => Path b File
  -> m (Path b File)
stripExtension p =
  Path.setFileExtension "" p &
    note (injectTyped $ PathError $ "Could not strip extension for: " <> show p)

fileNameText
  :: (MonadError e m, AsType PathError e)
  => Path b File
  -> m Text
fileNameText p =
  stripExtension p <&>
    (Path.filename >>> Path.toFilePath >>> toS)

setFileExtension
  :: (MonadError e m, AsType PathError e)
  => Path b File
  -> Text
  -> m (Path b File)
setFileExtension p ext =
  Path.setFileExtension (toS ext) p &
    note (injectTyped $ PathError $ "Path not valid with extension: " <> show p)
