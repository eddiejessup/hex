{-# LANGUAGE LambdaCase #-}

module HeXPrelude
  ( Readable(..)
  , liftMaybe
  , liftThrow
  )
where

import           Control.Monad.Except           ( liftIO
                                                , MonadError
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans.Maybe      ( MaybeT
                                                , runMaybeT
                                                )
import           Debug.Readable                  ( Readable(..) )

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e = \case
    Nothing -> throwError e
    Just a -> pure a

liftThrow :: (MonadIO m, MonadError e m) => e -> MaybeT IO a -> m a
liftThrow e v = (liftIO $ runMaybeT v) >>= liftMaybe e
