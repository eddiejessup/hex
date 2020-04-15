{-# LANGUAGE UndecidableInstances #-}
module Debug.Readable where

import qualified Data.Text as Tx
import Protolude
import qualified Text.Megaparsec           as P

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
    tabs = Tx.replicate nrTabs "\t"
    delim = "\n" <> tabs

-- Parsec.

instance Readable (P.ParseError s Void) => Readable (P.ParseErrorBundle s Void) where
    describe P.ParseErrorBundle {P.bundleErrors} =
        "ParseErrorBundle[bundleErrors=" <> Tx.intercalate ", " (toList (describe <$> bundleErrors)) <> "]"

instance Readable (P.ErrorItem (P.Token s)) => Readable (P.ParseError s Void) where
    describe = \case
        P.TrivialError _ Nothing expecteds ->
            "Got nothing, expected one of: " <> describeFoldable expecteds
        P.TrivialError _ (Just gotErrIt) expectedErrIts ->
            "Got " <> describe gotErrIt <> ", expected one of: " <> describeFoldable expectedErrIts

instance Readable t => Readable (P.ErrorItem t) where
    describe = \case
        P.Tokens ts ->
            "Tokens[" <> describeFoldable ts <> "]"
        P.Label cs ->
            "Label \"" <> Tx.pack (toList cs) <> "\""
        P.EndOfInput ->
            "EndOfInput"

describeFoldable :: (Readable a, Foldable t) => t a -> Text
describeFoldable ts = "{" <> Tx.intercalate ", " (describe <$> toList ts) <> "}"
