module Data.Byte where

import HeXlude

data Signedness = Signed | Unsigned

data SignableInt = SignableInt !Signedness !Int

isSignedNrExpressibleInNBits :: Int -> Int -> Bool
isSignedNrExpressibleInNBits nrBits n =
    let x = 2 ^ (nrBits - 1)
    in  (-x <= n) && (n <= x - 1)

toSignableInt :: Signedness -> Int -> Either Text SignableInt
toSignableInt Unsigned n
    | n < 0 = Left $ "Number argument for unsigned is negative: " <> show n
    | otherwise = Right $ SignableInt Unsigned n
toSignableInt Signed n =
    pure $ SignableInt Signed n

toSignedInt, toUnsignedInt :: Int -> Either Text SignableInt
toSignedInt = toSignableInt Signed
toUnsignedInt = toSignableInt Unsigned

bytesNeededUnsigned :: Int -> Int
bytesNeededUnsigned n =
    1 + floor ((logBase 256.0 $ fromIntegral $ abs n) :: Double)

bytesNeeded :: SignableInt -> Int
bytesNeeded = \case
    SignableInt _ 0 -> 1
    SignableInt Unsigned n -> bytesNeededUnsigned n
    SignableInt Signed n ->
        let nrBytesUnsigned = bytesNeededUnsigned n
        in  if isSignedNrExpressibleInNBits (8 * nrBytesUnsigned) n
                then nrBytesUnsigned
                else nrBytesUnsigned + 1
