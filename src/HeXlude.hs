module Hexlude
    ( module Protolude
    , module Describe
    , module Data.Sequence

    , module Data.Generics.Sum.Typed
    , module Data.Generics.Product.Typed

    , module Generic.Random
    , module Test.QuickCheck

    , module Optics

    , field

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
import           Protolude                 hiding ((%), group, catch, to)

import           Control.Arrow             ((>>>))
import           Optics                    (Lens', lens, (%), (^.), (%~), (.~), (?~), view, to, prism)
import           Data.Sequence             (Seq (..), (<|), (|>), singleton)
import qualified Data.Sequence             as Seq
import           Debug.Describe            as Describe
import           Data.Generics.Product     (field)
import           Data.Generics.Product.Typed
import           Data.Generics.Sum.Typed
import           Generic.Random            (genericArbitraryU)
import           Test.QuickCheck           (Arbitrary(arbitrary), Gen)
import qualified Data.Ratio as Ratio

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
    deriving stock (Show, Eq)

data VDirection
    = Upward
    | Downward
    deriving stock (Show, Eq)

data Direction
    = Forward
    | Backward
    deriving stock (Show, Eq)

data Axis
    = Horizontal
    | Vertical
    deriving stock (Show, Eq)

data MoveMode
    = Put
    | Set
    deriving stock (Show)

data BoxDim
     = BoxWidth
     | BoxHeight
     | BoxDepth
     deriving stock (Show, Eq)
