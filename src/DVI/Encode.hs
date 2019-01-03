module DVI.Encode where

import qualified Data.ByteString.Lazy          as BLS

class Encodable a where
    encode :: a -> BLS.ByteString

encLength :: Encodable a => a -> Int
encLength = fromIntegral . BLS.length . encode

encLengths :: Encodable a => Functor f => f a -> f Int
encLengths = fmap encLength

encStarts :: Encodable a => [a] -> [Int]
encStarts = scanl (+) 0 . encLengths

encEnds :: Encodable a => [a] -> [Int]
encEnds = scanl1 (+) . encLengths

instance Encodable a => Encodable [a] where
    encode = BLS.concat . fmap encode
