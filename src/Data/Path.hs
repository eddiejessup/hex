module Data.Path where

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Word as Word
import HeXlude
import Path (File, Path)
import qualified Path

newtype PathError = PathError Text
  deriving Show

readPathText :: Path a File -> IO Text
readPathText = Path.toFilePath >>> readFile

readPathBytes :: Path a File -> IO (Seq Word.Word8)
readPathBytes = Path.toFilePath >>> BS.readFile >>> fmap (BS.unpack >>> Seq.fromList)

stripExtension
  :: MonadErrorAnyOf e m '[PathError]
  => Path b File
  -> m (Path b File)
stripExtension p =
  Path.setFileExtension "" p &
    note (throw $ PathError $ "Could not strip extension for: " <> show p)

fileNameText
  :: MonadErrorAnyOf e m '[PathError]
  => Path b File
  -> m Text
fileNameText p =
  stripExtension p <&>
    (Path.filename >>> Path.toFilePath >>> toS)

setFileExtension
  :: MonadErrorAnyOf e m '[PathError]
  => Path b File
  -> Text
  -> m (Path b File)
setFileExtension p ext =
  Path.setFileExtension (toS ext) p &
    note (throw $ PathError $ "Path not valid with extension: " <> show p)
