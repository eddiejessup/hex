{-# LANGUAGE UndecidableInstances #-}
module Debug.Describe where

import qualified Data.Text as Tx
import Protolude
import qualified Text.Megaparsec as P
import qualified Test.QuickCheck as QC

class Describe a where

  describe :: a -> [(Int, Text)]

-- Helpers.
describeRel :: Describe a => Int -> a -> [(Int, Text)]
describeRel n v =
  describe v <&> \(i, t) -> (i + n, t)

singleLine :: Text -> [(Int, Text)]
singleLine t = [(0, t)]

describePrepended :: Describe a => Int -> Text -> a -> [(Int, Text)]
describePrepended n prefix a =
  let lns = describeRel n a
  in case uncons lns of
       Nothing -> singleLine $ prefix <> " ?"
       Just ((_n, tx), rst) -> (_n, prefix <> " " <> tx) : rst

describeNamedRel :: Describe a => Int -> Text -> a -> [(Int, Text)]
describeNamedRel n name x =
  [(n, name)] <> describeRel (n + 1) x

describeNamedRel1 :: Describe a => Text -> a -> [(Int, Text)]
describeNamedRel1 = describeNamedRel 1

describeRelFoldable :: (Foldable t, Describe a) => Int -> t a -> [(Int, Text)]
describeRelFoldable n = concatMap (describeRel n)

describeNamedRelFoldable :: (Foldable t, Describe a) => Int -> Text -> t a -> [(Int, Text)]
describeNamedRelFoldable n name xs =
  [(n, name)] <> describeRelFoldable (n + 1) xs

describeNamedRelFoldable1 :: (Foldable t, Describe a) => Text -> t a -> [(Int, Text)]
describeNamedRelFoldable1 = describeNamedRelFoldable 1

renderDescribed :: Describe a => a -> Text
renderDescribed = renderLines . describe

renderLines :: [(Int, Text)] -> Text
renderLines lns =
  Tx.intercalate "\n" $ lns <&> \(n, t) -> Tx.replicate n " " <> t

quote :: Text -> Text
quote v = "'" <> v <> "'"

-- Instances.
instance Describe Int where

  describe = singleLine . show

instance Describe a => Describe (Maybe a) where

  describe Nothing = singleLine "Nothing"
  describe (Just a) =
    describePrepended 0 "Just" a

-- Parsec.
instance Describe (P.ParseError s Void) => Describe (P.ParseErrorBundle s Void) where

  describe P.ParseErrorBundle {P.bundleErrors} =
    [ (0, "ParseErrorBundle")
    , (1, "bundleErrors")
    ] <>
      describeNamedRelFoldable1 "bundleErrors" bundleErrors

instance Describe (P.ErrorItem (P.Token s)) => Describe (P.ParseError s Void) where

  describe (P.TrivialError _ maySaw expecteds) =
    [ (0, "TrivialError")
    ] <>
      describeNamedRel1 "Saw" maySaw <>
      describeNamedRelFoldable1 "Expected one of" expecteds

instance Describe t => Describe (P.ErrorItem t) where

  describe = \case
    P.Tokens ts ->
      describeNamedRelFoldable 0 "ErrorItem/Tokens" ts
    P.Label cs ->
      singleLine $ "ErrorItem/Label " <> quote (Tx.pack (toList cs))
    P.EndOfInput ->
      singleLine "ErrorItem/EndOfInput"

-- Arbitrary.
describeVectorOf :: Describe a => Int -> QC.Gen a -> IO ()
describeVectorOf n g = do
  xs <- QC.generate (QC.vectorOf n g)
  putText $ renderLines $ describeRelFoldable 0 xs
