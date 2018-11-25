module Math.Byte where

isSignedNrExpressibleInNBits :: Int -> Int -> Bool
isSignedNrExpressibleInNBits nrBits n =
  let x = 2 ^ (nrBits - 1)
  in (-x <= n) && (n <= x - 1)

data Signedness = Signed | Unsigned
data SignableInt = SignableInt Signedness Int

toSignableInt :: Signedness -> Int -> Either String SignableInt
toSignableInt Signed n
  | n < 0 = fail "Number argument for unsigned is negative"
  | otherwise = pure $ SignableInt Signed n
toSignableInt s n = pure $ SignableInt s n

toSignedInt, toUnsignedInt :: Int -> Either String SignableInt
toSignedInt = toSignableInt Signed
toUnsignedInt = toSignableInt Unsigned

bytesNeededUnsigned :: Int -> Int
bytesNeededUnsigned n =
  1 + floor ((logBase 256.0 $ fromIntegral $ abs n) :: Double)

bytesNeeded :: SignableInt -> Int
bytesNeeded (SignableInt Unsigned 0) = 1
bytesNeeded (SignableInt Unsigned n) = bytesNeededUnsigned n
bytesNeeded (SignableInt Signed 0) = 1
bytesNeeded (SignableInt Signed n) =
    let nrBytesUnsigned = bytesNeededUnsigned n
        needExtraByte = not $ isSignedNrExpressibleInNBits (8 * nrBytesUnsigned) n
    in nrBytesUnsigned + if needExtraByte then 1 else 0
