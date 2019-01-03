{-# LANGUAGE LambdaCase #-}

module Data.Byte where

data Signedness = Signed | Unsigned

data SignableInt = SignableInt !Signedness !Int

isSignedNrExpressibleInNBits :: Int -> Int -> Bool
isSignedNrExpressibleInNBits nrBits n =
    let
        x = 2 ^ (nrBits - 1)
    in
        (-x <= n) && (n <= x - 1)

toSignableInt :: Signedness -> Int -> Either String SignableInt
toSignableInt Signed n
    | n < 0 = fail "Number argument for unsigned is negative"
    | otherwise = pure $ SignableInt Signed n
toSignableInt s n =
    pure $ SignableInt s n

toSignedInt, toUnsignedInt :: Int -> Either String SignableInt
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
        let
            nrBytesUnsigned = bytesNeededUnsigned n
        in
            if isSignedNrExpressibleInNBits (8 * nrBytesUnsigned) n
                then nrBytesUnsigned
                else nrBytesUnsigned + 1
