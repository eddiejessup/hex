{-# LANGUAGE UndecidableInstances #-}
module Debug.Readable where

import qualified Data.Text as Txt
import Protolude

monoidIntercalate :: (Foldable t, Monoid a) => a -> t a -> a
monoidIntercalate d = go
  where
    go xs = fromMaybe mempty (foldl' f Nothing xs)
    f Nothing x = Just x
    f (Just acc) x = Just (acc <> d <> x)

class Readable a where

  describe :: a -> Text

instance Readable Int where

  describe = show

describeIntercalated :: (Readable a, Foldable t, Functor t) => Text -> t a -> Text
describeIntercalated d = monoidIntercalate d . (describe <$>)

describeLined :: (Readable a, Foldable t, Functor t) => t a -> Text
describeLined = describeIntercalated "\n"

describeDoubleLined :: (Readable a, Foldable t, Functor t) => t a -> Text
describeDoubleLined = describeIntercalated "\n\n"

describeListHeaded :: (Readable a, Foldable t, Functor t) => Int -> Text -> t a -> Text
describeListHeaded nrTabs title xs =
  title <> " {{{" <> delim <>
    describeIntercalated delim xs <>
    "\n}}} " <>
    title
  where
    tabs = Txt.replicate nrTabs "\t"
    delim = "\n" <> tabs
