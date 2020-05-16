module System.Log.Slog where

import Protolude

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BS.L
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.Map.Strict

class Monad m => MonadSlog m where

  sLog :: ByteString -> m ()

  sTime :: m UTCTime

instance MonadSlog m => MonadSlog (StateT st m) where
  sLog bs = lift $ sLog bs

  sTime = lift sTime

instance MonadSlog m => MonadSlog (ExceptT e m) where
  sLog bs = lift $ sLog bs

  sTime = lift sTime

sLogAsJSON :: (MonadSlog m, Ae.ToJSON a) => a -> m ()
sLogAsJSON = sLog . BS.L.toStrict . Ae.encode

sLogStampedJSON :: MonadSlog m => Text -> [(Text, Ae.Value)] -> m ()
sLogStampedJSON msg kvs = do
  utcTime <- sTime
  sLogAsJSON
    $ insert "timestamp" (Ae.String (toS $ iso8601Show utcTime))
    $ insert "message" (Ae.String msg)
    $ fromList kvs
