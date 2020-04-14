module DVI.Encode where

import qualified Data.ByteString as BS
import Hexlude

class Encodable a where

  encode :: a -> ByteString

encLength :: Encodable a => a -> Int
encLength = fromIntegral . BS.length . encode

instance (Encodable a, Foldable t) => Encodable (t a) where

  encode = BS.concat . fmap encode . toList
