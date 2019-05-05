{-# LANGUAGE UndecidableInstances #-}

module Debug.Readable where

import           Prelude
import qualified Protolude as Pr

monoidIntercalate :: (Foldable t, Monoid a) => a -> t a -> a
monoidIntercalate d = go
  where
    go xs = Pr.fromMaybe mempty (Pr.foldl' f Nothing xs)

    f Nothing x = Just x
    f (Just acc) x = Just (acc <> d <> x)

class Readable a where
    describe :: a -> Pr.Text

instance (Readable a, Foldable t, Functor t) => Readable (t a) where
    describe = describeLined

showT :: Show a => a -> Pr.Text
showT v = Pr.toS $ show v

describeIntercalated :: (Readable a, Foldable t, Functor t) => Pr.Text -> t a -> Pr.Text
describeIntercalated d = monoidIntercalate d . (describe <$>)

describeLined :: (Readable a, Foldable t, Functor t) => t a -> Pr.Text
describeLined = describeIntercalated "\n"

describeDoubleLined :: (Readable a, Foldable t, Functor t) => t a -> Pr.Text
describeDoubleLined = describeIntercalated "\n\n"
