module Data.Byte where

import           HeXlude

newtype ByteError = ByteError Text
    deriving (Show)

data Signedness = Signed | Unsigned

data SignableInt a = SignableInt Signedness a

newtype UnsignedVal a = UnsignedVal a

isSignedNrExpressibleInNBits :: Integral a => Int -> a -> Bool
isSignedNrExpressibleInNBits nrBits n =
    let x = 2 ^ (nrBits - 1)
    in
        (-x <= n) && (n <= x - 1)

toUnsigned
    :: (Num a, Ord a, Show a, MonadErrorAnyOf e m '[ByteError])
    => a
    -> m (UnsignedVal a)
toUnsigned n
    | n < 0 = throwM $ ByteError $ "Number argument for unsigned is negative: " <> show n
    | otherwise = pure (UnsignedVal n)

bytesNeededUnsigned :: (FiniteBits a, Integral a) => a -> Int
bytesNeededUnsigned n
    | n > 0 = 1 + logBase2 n `div` 8
    | n == 0 = 1
    | otherwise = panic $ "Asked for bytes needed for negative number: " <> show (fromIntegral n :: Int)
  where
    logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

bytesNeededSigned :: (Integral a, FiniteBits a) => a -> Int
bytesNeededSigned = \case
    0 ->
        1
    n ->
        let nrBytesUnsigned = bytesNeededUnsigned n
        in
            if isSignedNrExpressibleInNBits (8 * nrBytesUnsigned) n
            then nrBytesUnsigned
            else nrBytesUnsigned + 1
