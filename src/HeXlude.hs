module HeXlude
    ( module Protolude
    , module Readable
    , module HeX.Type
    , module Data.Sequence
    , seqLookupEith
    , seqLastMay
    , seqHeadMay
    , module Data.Witherable
    , id
    , liftMaybe
    , liftThrow
    , (>>>)
    , roundToDec
    , atEith
    , fail
    , maybeToFail
    , traceText
    , mconcat
    )
where

import           Prelude                   (id)
import           Protolude                 hiding ( catMaybes
                                                  , filter
                                                  , group
                                                  , mapMaybe
                                                  , mconcat )

import           Control.Arrow             ((>>>))
import           Control.Monad.Except      (MonadError, liftIO, throwError)
import           Control.Monad.Fail        (MonadFail, fail)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import qualified Data.Sequence             as Seq
import           Data.Sequence             (Seq (..), (|>), (<|))
import           Data.Witherable           (Filterable (..))
import           Debug.Readable            as Readable

import           HeX.Type

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

traceText :: Text -> a -> a
traceText = trace

mconcat :: (Monoid a, Foldable t) => t a -> a
mconcat xs = foldr (<>) mempty xs

seqLookupEith :: Show a => Text -> Seq a -> Int -> Either Text a
seqLookupEith str xs i = maybeToRight
    ("No " <> str <> " at index " <> showT i <> ", values are: "
     <> showT xs)
    (Seq.lookup i xs)

seqLastMay :: Seq a -> Maybe a
seqLastMay = \case
    _ :|> x -> Just x
    _ -> Nothing

seqHeadMay :: Seq a -> Maybe a
seqHeadMay = \case
    x :<| _ -> Just x
    _ -> Nothing
