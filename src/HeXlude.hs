{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}

module HeXlude
    ( module Protolude
    , module Readable
    , module Data.Sequence
    , module Data.Variant

    , MonadErrorVariant
    , MonadErrorAnyOf

    , seqLookupEith
    , seqLastMay
    , seqHeadMay
    , module Data.Witherable
    , id
    , liftThrow
    , (>>>)
    , atEith
    , traceText
    , mconcat
    , mconcatMap

    , HDirection(..)
    , VDirection(..)
    , Direction(..)
    , Axis(..)
    , MoveMode(..)
    , BoxDim(..)
    )
where

import           Prelude                   (id)
import           Protolude                 hiding (catMaybes, filter, group,
                                            mapMaybe, mconcat, catch)

import           Control.Applicative       (Alternative, empty)
import           Control.Arrow             ((>>>))
import           Control.Monad.Except      (MonadError, liftIO, throwError)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           Data.Sequence             (Seq (..), (<|), (|>), singleton)
import qualified Data.Sequence             as Seq
import           Data.Witherable           (Filterable (..))
import           Debug.Readable            as Readable
import           GHC.Generics              (Generic)
import           HeX.Orphans               ()

import           Data.Variant              hiding (fold)


type MonadErrorVariant e m = MonadError (Variant e) m

type MonadErrorAnyOf e m es = (MonadErrorVariant e m, e `CouldBeAnyOf` es)

liftThrow :: (MonadIO m, MonadError e m) => e -> MaybeT IO a -> m a
liftThrow e v = liftIO (runMaybeT v) >>= note e

atEith :: Show a => Text -> [a] -> Int -> Either Text a
atEith str xs i = maybeToRight
    ("No " <> str <> " at index " <> showT i <> ", values are: "
     <> showT xs)
    (atMay xs i)

traceText :: Text -> a -> a
traceText = trace

mconcat :: (Monoid a, Foldable t) => t a -> a
mconcat = foldl' (<>) mempty

mconcatMap :: (Monoid c, Foldable t, Functor t) => (a -> c) -> t a -> c
mconcatMap f = mconcat . (f <$>)

-- Sequence.

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


-- Concepts.

data HDirection
    = Leftward
    | Rightward
    deriving (Show, Eq)

data VDirection
    = Upward
    | Downward
    deriving (Show, Eq)

data Direction
    = Forward
    | Backward
    deriving (Show, Eq)

data Axis
    = Horizontal
    | Vertical
    deriving (Show, Eq)

data MoveMode
    = Put
    | Set
    deriving (Show)

data BoxDim
     = BoxWidth
     | BoxHeight
     | BoxDepth
     deriving (Show, Eq)
