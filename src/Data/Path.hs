{-# LANGUAGE UndecidableInstances #-}
module Data.Path where

import qualified Data.ByteString as BS
import Hexlude
import Path (Dir, Abs, Rel, File, Path)
import qualified Path
import qualified Path.IO

newtype PathError = PathError Text
  deriving stock (Show, Generic)

stripExtension
  :: (MonadError e m, AsType PathError e)
  => Path b File
  -> m (Path b File)
stripExtension p =
  Path.replaceExtension "" p &
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
  Path.replaceExtension (toS ext) p &
    note (injectTyped $ PathError $ "Path not valid with extension: " <> show p)

data FindFilePolicy
    = NoImplicitExtension
    | WithImplicitExtension Text

normaliseFileName :: (MonadError e m, AsType PathError e) => FindFilePolicy -> Path b File -> m (Path b File)
normaliseFileName findPolicy p = case findPolicy of
    NoImplicitExtension ->
        pure p
    WithImplicitExtension ext ->
        setFileExtension p ext

readBytes :: MonadIO m => Path a File -> m BS.ByteString
readBytes = liftIO . BS.readFile . Path.toFilePath

class Monad m => MonadInput m where

    findPath :: FindFilePolicy -> Path Rel File -> [Path Abs Dir] -> m (Path Abs File)

    readPathBytes :: Path Abs File -> m ByteString

newtype IOInputT m a = IOInputT { unIOInputT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadState s, MonadIO, MonadError e)

instance (Monad m, MonadIO m, MonadError e m, AsType PathError e) => MonadInput (IOInputT m) where

    findPath policy tgt searchDirs = IOInputT $
        normaliseFileName policy tgt
          >>= Path.IO.findFile searchDirs
          >>= note (injectTyped $ PathError $ "Could not find file: " <> show tgt)

    readPathBytes = IOInputT . readBytes
