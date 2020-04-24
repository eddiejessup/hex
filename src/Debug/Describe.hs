{-# LANGUAGE UndecidableInstances #-}
module Debug.Describe where

import qualified Data.Text as Tx
import Protolude
import qualified Text.Megaparsec           as P

monoidIntercalate :: (Foldable t, Monoid a) => a -> t a -> a
monoidIntercalate d = go
  where
    go xs = fromMaybe mempty (foldl' f Nothing xs)
    f Nothing x = Just x
    f (Just acc) x = Just (acc <> d <> x)

class Describe a where

  describe :: a -> [(Int, Text)]

instance Describe Int where

  describe v = [(0, show v)]

describeRel :: Describe a => Int -> a -> [(Int, Text)]
describeRel n v =
    describe v <&> \(i, t) -> (i + n, t)

singleLine t = [(0, t)]
-- describeIntercalated :: (Describe a, Foldable t, Functor t) => Text -> t a -> Text
-- describeIntercalated d = monoidIntercalate d . (describe <$>)

-- describeLined :: (Describe a, Foldable t, Functor t) => t a -> Text
-- describeLined = describeIntercalated "\n"

-- describeDoubleLined :: (Describe a, Foldable t, Functor t) => t a -> Text
-- describeDoubleLined = describeIntercalated "\n\n"

-- describeListHeaded :: (Describe a, Foldable t, Functor t) => Int -> Text -> t a -> Text
-- describeListHeaded nrTabs title xs =
--   title <> " {{{" <> delim <>
--     describeIntercalated delim xs <>
--     "\n}}} " <>
--     title
--   where
--     tabs = Tx.replicate nrTabs "\t"
--     delim = "\n" <> tabs

-- Parsec.

instance Describe (P.ParseError s Void) => Describe (P.ParseErrorBundle s Void) where
    describe P.ParseErrorBundle { P.bundleErrors } =
        [ (0, "ParseErrorBundle")
        , (1, "bundleErrors")
        ] <> describeNamedRelFoldable1 "bundleErrors" bundleErrors

instance Describe (P.ErrorItem (P.Token s)) => Describe (P.ParseError s Void) where
    describe (P.TrivialError _ maySaw expecteds) =
        [ (0, "TrivialError")
        ]
        <> describeNamedRel1 "Saw" maySaw
        <> describeNamedRelFoldable1 "Expected one of" expecteds

instance Describe a => Describe (Maybe a) where
    describe Nothing = singleLine "Nothing"
    describe (Just a) =
        describePrepended 0 "Just" a

describePrepended n prefix a =
    let lns = describeRel n a
        in case uncons lns of
            Nothing -> singleLine $ prefix <> " ?"
            Just ((n, a), rst) -> (n, prefix <> " " <> a) : rst

describeNamedRel :: Describe a => Int -> Text -> a -> [(Int, Text)]
describeNamedRel n name x =
    [(n, name)] <> describeRel (n + 1) x

describeNamedRel1 :: Describe a => Text -> a -> [(Int, Text)]
describeNamedRel1 = describeNamedRel 1

describeRelFoldable n = concatMap (describeRel n)

describeNamedRelFoldable :: (Foldable t, Describe a) => Int -> Text -> t a -> [(Int, Text)]
describeNamedRelFoldable n name xs =
    [(n, name)] <> describeRelFoldable (n + 1) xs

describeNamedRelFoldable1 :: (Foldable t, Describe a) => Text -> t a -> [(Int, Text)]
describeNamedRelFoldable1 = describeNamedRelFoldable 1

instance Describe t => Describe (P.ErrorItem t) where
    describe = \case
        P.Tokens ts ->
            describeNamedRelFoldable 0 "ErrorItem/Tokens" ts
        P.Label cs ->
            [(0, "ErrorItem/Label '" <> Tx.pack (toList cs) <> "'")]

        P.EndOfInput ->
            [(0, "ErrorItem/EndOfInput")]

renderDescribed :: Describe a => a -> Text
renderDescribed = renderLines . describe

renderLines lns
    = Tx.intercalate "\n" $ lns <&> \(n, t) -> Tx.replicate n " " <> t

quote v = "'" <> v <> "'"
