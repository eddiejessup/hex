module Hexlude
  ( module Protolude,
    module Data.Sequence,
    module Data.Tree,
    module Data.Generics.Sum,
    module Data.Generics.Product,
    module Generic.Random,
    module Test.QuickCheck,
    module Describe,
    module System.Log.Slog,
    module Data.Aeson,
    module Optics.Lens,
    module Optics.Operators,
    module Optics.Getter,
    module Optics.Optic,
    module Optics.State,
    uses,
    mkRatio,
    seqLastMay,
    seqHeadMay,
    seqMapMaybe,
    id,
    (>>>),
    traceText,
    flap,
    HDirection (..),
    VDirection (..),
    Direction (..),
    Axis (..),
    MoveMode (..),
    BoxDim (..),
  )
where

import Control.Arrow ((>>>))
import Data.Aeson (ToJSON (..))
import Data.Generics.Product (HasType, field, typed)
import Data.Generics.Sum (AsType, injectTyped)
import qualified Data.Ratio as Ratio
import Data.Sequence (Seq (..), singleton, (<|), (|>))
import qualified Data.Sequence as Seq
import Data.Tree (Tree (..))
import Debug.Describe as Describe
import Generic.Random (genericArbitraryU)
import Optics.Getter (A_Getter, Getter, to, view, views)
import Optics.Lens (Lens', lens)
import Optics.Operators ((%~), (.~), (?~), (^.))
import Optics.Optic (Is, Optic', (%))
import Optics.State (assign', modifying', use)
import Protolude hiding (HasField, catch, getField, group, isAscii, isHexDigit, isSpace, log, option, stdin, take, to, try, yield, (%))
import System.Log.Slog
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import Prelude (id)

mkRatio :: Integral a => a -> a -> Ratio a
mkRatio = (Ratio.%)

traceText :: Text -> a -> a
traceText = trace

-- Optics.

uses :: (MonadState s m, Is k A_Getter) => Optic' k is s a -> (a -> r) -> m r
uses g f = gets (views g f)

-- Stolen from relude.
flap :: Functor f => f (a -> b) -> a -> f b
flap ff x = (\f -> f x) <$> ff
{-# INLINE flap #-}

-- Sequence.

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
