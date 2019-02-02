module Data.Path where

import           Control.Monad.Extra            ( findM )
import           Control.Monad.IO.Class         ( liftIO )
import           System.Directory
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Path

firstExistingPath :: [Path b File] -> MaybeT IO (Path b File)
-- Make a MaybeT of...
-- The result of an IO function, lifted to our MaybeT IO monad.
-- Namely 'find', but using a predicate which acts in the IO monad,
-- and which tests if a file exists.
firstExistingPath = MaybeT . liftIO . findM (doesFileExist . toFilePath)

findFilePath :: Path Rel File -> [Path Abs Dir] -> MaybeT IO (Path Abs File)
findFilePath name dirs = firstExistingPath $ fmap (</> name) dirs
