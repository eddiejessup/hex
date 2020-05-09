module System.Log.Slog where

import Protolude

class Monad m => MonadSlog m where

  sLog :: Text -> m ()

sLogs :: (MonadSlog m, Foldable f, Functor f) => f Text -> m ()
sLogs ts = sequence_ $ sLog <$> ts
