module DVI.Encode where

import           HeXlude

import qualified Data.ByteString as BS

class Encodable a where
    encode :: a -> ByteString

encLength :: Encodable a => a -> Int
encLength = fromIntegral . BS.length . encode

instance Encodable a => Encodable [a] where
    encode = BS.concat . fmap encode
