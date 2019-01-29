module HeXPrelude
  ( Readable(..)
  , liftMaybe
  )
where

import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )

import           Debug.Readable                  ( Readable(..) )

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e Nothing  = throwError e
liftMaybe _ (Just a) = pure a
