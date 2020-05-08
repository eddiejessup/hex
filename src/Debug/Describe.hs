{-# LANGUAGE UndecidableInstances #-}
module Debug.Describe where

import qualified Data.Text as Tx
import Protolude
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

-- Arbitrary.
describeVectorOf :: Describe a => Int -> QC.Gen a -> IO ()
describeVectorOf n g = do
  xs <- QC.generate (QC.vectorOf n g)
  putText $ renderLines $ describeRelFoldable 0 xs
