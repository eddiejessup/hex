module Data.Path where

import           HeXlude

import           Control.Monad.Extra       ( findM )
import           Control.Monad.IO.Class    ( liftIO )
import           Control.Monad.Trans.Maybe ( MaybeT(..) )
import           Path
import           System.Directory

firstExistingPath :: [Path b File] -> MaybeT IO (Path b File)

-- Make a MaybeT of...
-- The result of an IO function, lifted to our MaybeT IO monad.
-- Namely 'find', but using a predicate which acts in the IO monad,
-- and which tests if a file exists.
firstExistingPath = MaybeT . liftIO . findM (doesFileExist . toFilePath)

findFilePath :: Path Rel File -> [Path b Dir] -> MaybeT IO (Path b File)
findFilePath name dirs = firstExistingPath $ fmap (</> name) dirs

stripExtension' :: Path b File -> Maybe (Path b File)
stripExtension' = Path.setFileExtension ""

stripExtension :: (MonadIO m, MonadError Text m)
               => Path b File
               -> m (Path b File)
stripExtension p = liftThrow ("Could not strip font extension for: " <> showT p)
                             (Path.setFileExtension "" p)
