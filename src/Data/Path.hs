module Data.Path where

import           HeXlude
import qualified Prelude

import           Path    (File, Path)
import qualified Path

newtype PathError = PathError Text
    deriving (Show)

readPathText :: Path a File -> IO Text
readPathText = Path.toFilePath >>> readFile

readPathChars :: Path a File -> IO [Char]
readPathChars = Path.toFilePath >>> Prelude.readFile

stripExtension
    :: MonadErrorAnyOf e m '[PathError]
    => Path b File
    -> m (Path b File)
stripExtension p =
    Path.setFileExtension "" p
    & liftMaybe (throw $ PathError $ "Could not strip extension for: " <> show p)

fileNameText
    :: MonadErrorAnyOf e m '[PathError]
    => Path b File
    -> m Text
fileNameText p =
    stripExtension p
    <&> (Path.filename >>> Path.toFilePath >>> toS)

setFileExtension
    :: MonadErrorAnyOf e m '[PathError]
    => Path b File
    -> Text
    -> m (Path b File)
setFileExtension p ext =
    Path.setFileExtension (toS ext) p
    & liftMaybe (throw $ PathError $ "Path not valid with extension: " <> show p)
