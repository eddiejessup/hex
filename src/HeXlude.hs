module Hexlude
    ( module Protolude
    , module Data.Sequence

    , module Data.Generics.Sum
    , module Data.Generics.Product

    , module Generic.Random
    , module Test.QuickCheck

    , module Describe
    , module System.Log.Slog
    , module Data.Aeson

    , module Optics.Lens
    , module Optics.Operators
    , module Optics.Getter
    , module Optics.Optic

    , mkRatio

    , seqLookupEith
    , seqLastMay
    , seqHeadMay
    , seqMapMaybe

    , id
    , (>>>)
    , atEith
    , traceText
    , flap

    , HDirection(..)
    , VDirection(..)
    , Direction(..)
    , Axis(..)
    , MoveMode(..)
    , BoxDim(..)
    )
where

import           Prelude                   (id)
import           Protolude                 hiding ((%), group, catch, to, try, log, getField, HasField)

import           Control.Arrow             ((>>>))
import           Optics.Lens               (Lens', lens)
import           Optics.Optic              ((%))
import           Optics.Operators          ((^.), (%~), (.~), (?~))
import           Optics.Getter             (view, to)
import           Data.Aeson
import           Data.Sequence             (Seq (..), (<|), (|>), singleton)
import qualified Data.Sequence             as Seq
import           Debug.Describe            as Describe
import           Data.Generics.Product     hiding (list)
import           Data.Generics.Sum
import           Generic.Random            (genericArbitraryU)
import           Test.QuickCheck           (Arbitrary(arbitrary), Gen)
import qualified Data.Ratio as Ratio
import System.Log.Slog

mkRatio :: Integral a => a -> a -> Ratio a
mkRatio = (Ratio.%)

atEith :: (MonadError Text m, Show a) => Text -> [a] -> Int -> m a
atEith str xs i = note
    ("No " <> str <> " at index " <> show i <> ", values are: "
     <> show xs)
    (atMay xs i)

traceText :: Text -> a -> a
traceText = trace

-- Stolen from relude.
flap :: Functor f => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff
{-# INLINE flap #-}

-- Sequence.

seqLookupEith :: (MonadError Text m, Show a) => Text -> Seq a -> Int -> m a
seqLookupEith str xs i = note
    ("No " <> str <> " at index " <> show i <> ", values are: "
     <> show xs)
    (Seq.lookup i xs)

seqLastMay :: Seq a -> Maybe a
seqLastMay = \case
    _ :|> x -> Just x
    _ -> Nothing

seqHeadMay :: Seq a -> Maybe a
seqHeadMay = \case
    x :<| _ -> Just x
    _ -> Nothing

seqMapMaybe :: (a -> Maybe b) -> Seq a -> Seq b
seqMapMaybe f = Seq.fromList . mapMaybe f . toList

-- Concepts.

data HDirection
    = Leftward
    | Rightward
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data VDirection
    = Upward
    | Downward
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data Direction
    = Forward
    | Backward
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data Axis
    = Horizontal
    | Vertical
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data MoveMode
    = Put
    | Set
    deriving stock (Show)

data BoxDim
     = BoxWidth
     | BoxHeight
     | BoxDepth
     deriving stock (Show, Eq, Generic)
     deriving anyclass (ToJSON)
