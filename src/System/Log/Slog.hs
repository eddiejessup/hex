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

sLogs :: (MonadSlog m, Foldable f, Functor f) => f ByteString -> m ()
sLogs ts = sequence_ $ sLog <$> ts

sLogText :: MonadSlog m => Text -> m ()
sLogText = sLog . encodeUtf8

sLogJSONMap :: MonadSlog m => Map Text Text -> m ()
sLogJSONMap = sLog . BS.L.toStrict . Ae.encode

sLogStampedJSONMap :: MonadSlog m => Map Text Text -> m ()
sLogStampedJSONMap kvs = do
  utcTime <- sTime
  sLogJSONMap $ insert "timestamp" (toS $ iso8601Show utcTime) kvs

sLogStampedJSON :: MonadSlog m => Text -> [(Text, Text)] -> m ()
sLogStampedJSON msg kvs = do
  utcTime <- sTime
  sLogJSONMap
    $ insert "timestamp" (toS $ iso8601Show utcTime)
    $ insert "message" msg
    $ fromList kvs
