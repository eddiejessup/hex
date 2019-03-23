module HeXlude
    ( module Protolude
    , module Readable
    , id
    , liftMaybe
    , liftThrow
    , (>>>)
    , roundToDec
    , atEith
    , fail
    , maybeToFail
    )
where

import           Protolude
import           Prelude                        ( id )

import           Control.Arrow                  ( (>>>) )
import           Control.Monad.Fail             ( MonadFail, fail )
import           Data.Functor                   ( (<&>) )
import           Data.Function                  ( (&) )
import           Control.Monad.Except           ( liftIO
                                                , MonadError
                                                , throwError
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans.Maybe      ( MaybeT
                                                , runMaybeT
                                                )

import           Debug.Readable                as Readable

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e = \case
    Nothing -> throwError e
    Just a -> pure a

liftThrow :: (MonadIO m, MonadError e m) => e -> MaybeT IO a -> m a
liftThrow e v = (liftIO $ runMaybeT v) >>= liftMaybe e

roundToDec :: RealFrac a => Int -> a -> a
roundToDec n v = (fromInteger $ round $ v * (10 ^ n)) / (10.0 ^^ n)

atEith :: Show a => Text -> [a] -> Int -> Either Text a
atEith str xs i = maybeToRight
    ("No " <> str <> " at index " <> showT i <> ", values are: "
     <> showT xs)
    (atMay xs i)

maybeToFail :: MonadFail m => Text -> Maybe a -> m a
maybeToFail err = \case
    Nothing -> fail $ toS err
    Just v -> pure v
