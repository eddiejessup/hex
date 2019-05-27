module Data.Path where

import           HeXlude
import qualified Prelude
import           Path

readPathText :: Path a File -> IO Text
readPathText = toFilePath >>> readFile

readPathChars :: Path a File -> IO [Char]
readPathChars = toFilePath >>> Prelude.readFile

stripExtension :: MonadError Text m => Path b File -> m (Path b File)
stripExtension p = Path.setFileExtension "" p
                   & liftMaybe ("Could not strip extension for: " <> show p)

fileNameText :: MonadError Text m => Path b File -> m Text
fileNameText p = stripExtension p
               <&> (Path.filename >>> Path.toFilePath >>> toS)

setFileExtension :: (MonadError Text m) => Path b File -> Text -> m (Path b File)
setFileExtension p ext = Path.setFileExtension (toS ext) p
                       & liftMaybe ("Path not valid with extension: " <> show p)
