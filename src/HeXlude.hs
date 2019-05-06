{-# LANGUAGE PatternSynonyms #-}

module HeXlude
    ( module Protolude
    , module Readable
    , module Data.Sequence
    , module HeX.Unit
    , seqLookupEith
    , seqLastMay
    , seqHeadMay
    , module Data.Witherable
    , id
    , liftMaybe
    , liftThrow
    , (>>>)
    , atEith
    , fail
    , maybeToFail
    , traceText
    , mconcat
    , mconcatMap

    , TeXIntVal
    , LenVal
    , newNBitInt
    , EightBitInt(..)
    , newEightBitInt
    , FourBitInt(..)
    , newFourBitInt

    , HDirection(..)
    , VDirection(..)
    , Direction(..)
    , Axis(..)
    , MoveMode(..)
    , BoxDim(..)
    , Dimensioned(..)
    , naturalWidth
    , naturalHeight
    , naturalDepth
    , axisNaturalSpan

    , TaggedContainer(FDirected, BDirected)
    , ForwardDirected
    , BackwardDirected
    , fUndirected
    , bUndirected
    , revForwardSeq
    , revBackwardSeq
    , taggedSeqtoList
    , (.<-), (<-.), (->.), (.->)
    )
where

import           Prelude                   (id)
import           Protolude                 hiding (catMaybes, filter, group,
                                            mapMaybe, mconcat)

import           Control.Applicative       (Alternative, empty)
import           Control.Arrow             ((>>>))
import           Control.Monad.Except      (MonadError, liftIO, throwError)
import           Control.Monad.Fail        (MonadFail, fail)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import           Data.Sequence             (Seq (..), (<|), (|>))
import qualified Data.Sequence             as Seq
import           Data.Witherable           (Filterable (..))
import           Debug.Readable            as Readable
import           GHC.Generics              (Generic)

import           HeX.Unit

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e = \case
    Nothing -> throwError e
    Just a -> pure a

liftThrow :: (MonadIO m, MonadError e m) => e -> MaybeT IO a -> m a
liftThrow e v = (liftIO $ runMaybeT v) >>= liftMaybe e

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
mconcat xs = foldl' (<>) mempty xs

mconcatMap :: (Monoid c, Foldable t, Functor t) => (a -> c) -> t a -> c
mconcatMap f = mconcat . (f <$>)

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





type TeXIntVal = Int
type LenVal = Int

newNBitInt :: Alternative f => (Int -> a) -> Int ->  Int -> f a
newNBitInt f nBits n
    | n < 0 = empty
    | n >= (2 ^ nBits) = empty
    | otherwise = pure $ f n

-- 8-bit.

newtype EightBitInt = EightBitInt TeXIntVal
    deriving (Show, Eq, Generic, Enum)

instance Hashable EightBitInt

instance Bounded EightBitInt where
    minBound = EightBitInt 0
    maxBound = EightBitInt (2 ^ (8 :: Int) - 1)

newEightBitInt :: Alternative f => Int -> f EightBitInt
newEightBitInt = newNBitInt EightBitInt 8

-- 4-bit.

newtype FourBitInt = FourBitInt TeXIntVal
    deriving (Show, Eq, Generic, Enum)

instance Hashable FourBitInt

instance Bounded FourBitInt where
    minBound = FourBitInt 0
    maxBound = FourBitInt (2 ^ (4 :: Int) - 1)

newFourBitInt :: Alternative f => Int -> f FourBitInt
newFourBitInt = newNBitInt FourBitInt 4

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

class Dimensioned a where
    naturalLength :: BoxDim -> a -> LenVal

naturalWidth, naturalHeight, naturalDepth :: Dimensioned a => a -> LenVal
naturalWidth  = naturalLength BoxWidth
naturalHeight = naturalLength BoxHeight
naturalDepth  = naturalLength BoxDepth

axisNaturalSpan :: Dimensioned a => Axis -> a -> LenVal
axisNaturalSpan Vertical   a = naturalHeight a + naturalDepth a
axisNaturalSpan Horizontal a = naturalWidth a




newtype TaggedContainer n t a = TaggedContainer {untagged :: t a}
    deriving Show
    deriving stock   (Foldable, Traversable)
    deriving newtype (Functor, Applicative, Monad, Alternative, Filterable, Semigroup, Monoid)

data Forward
data Backward

revForwardContainer :: (t a -> t a) -> ForwardDirected t a -> BackwardDirected t a
revForwardContainer rev (TaggedContainer xs) = TaggedContainer (rev xs)

revBackwardContainer :: (t a -> t a) -> BackwardDirected t a -> ForwardDirected t a
revBackwardContainer rev (TaggedContainer xs) = TaggedContainer (rev xs)

fUndirected :: ForwardDirected t a -> t a
fUndirected = untagged

bUndirected :: BackwardDirected t a -> t a
bUndirected = untagged

type ForwardDirected = TaggedContainer Forward
type BackwardDirected = TaggedContainer Backward

type ForwardSeq = ForwardDirected Seq
type BackwardSeq = BackwardDirected Seq

mapTaggedContainer :: (t a -> u a) -> TaggedContainer n t a -> TaggedContainer n u a
mapTaggedContainer f (TaggedContainer xs) = TaggedContainer (f xs)

taggedSeqtoList :: TaggedContainer n Seq a -> TaggedContainer n [] a
taggedSeqtoList = mapTaggedContainer toList

revForwardSeq :: ForwardSeq a -> BackwardSeq a
revForwardSeq = revForwardContainer Seq.reverse

revBackwardSeq :: BackwardSeq a -> ForwardSeq a
revBackwardSeq = revBackwardContainer Seq.reverse

infixr 5 .<-
infixl 5 <-.
infixl 5 ->.
infixr 5 .->

(.<-) :: a -> BackwardSeq a -> BackwardSeq a
x .<- (TaggedContainer xs) = TaggedContainer (x <| xs)

(->.) :: ForwardSeq a -> a -> ForwardSeq a
(TaggedContainer xs) ->. x = TaggedContainer (xs |> x)

(<-.) :: BackwardSeq a -> a -> BackwardSeq a
(TaggedContainer xs) <-. x = TaggedContainer (xs |> x)

(.->) :: a -> ForwardSeq a -> ForwardSeq a
x .-> (TaggedContainer xs) = TaggedContainer (x <| xs)

pattern FDirected :: t a -> ForwardDirected t a
pattern FDirected v = TaggedContainer v

pattern BDirected :: t a -> BackwardDirected t a
pattern BDirected v = TaggedContainer v
