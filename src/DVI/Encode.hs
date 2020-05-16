module DVI.Encode where

import qualified Data.ByteString as BS
import Hexlude

class DVIEncodable a where

  dviEncode :: a -> ByteString

dviEncLength :: DVIEncodable a => a -> Int
dviEncLength = fromIntegral . BS.length . dviEncode

instance (DVIEncodable a, Foldable t) => DVIEncodable (t a) where

  dviEncode = BS.concat . fmap dviEncode . toList
