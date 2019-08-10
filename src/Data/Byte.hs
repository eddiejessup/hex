module Data.Byte where

import           HeXlude

newtype ByteError = ByteError Text
    deriving (Show)

data Signedness = Signed | Unsigned

data SignableInt = SignableInt !Signedness !Int

isSignedNrExpressibleInNBits :: Int -> Int -> Bool
isSignedNrExpressibleInNBits nrBits n =
    let x = 2 ^ (nrBits - 1)
    in
        (-x <= n) && (n <= x - 1)

toSignableInt
    :: MonadErrorAnyOf e m '[ByteError]
    => Signedness
    -> Int
    -> m SignableInt
toSignableInt Unsigned n
    | n < 0 = throwM $ ByteError $ "Number argument for unsigned is negative: " <> show n
    | otherwise = pure $ SignableInt Unsigned n
toSignableInt Signed n = pure $ SignableInt Signed n

toSignedInt
    :: MonadErrorAnyOf e m '[ByteError]
    => Int
    -> m SignableInt
toSignedInt = toSignableInt Signed

toUnsignedInt
    :: MonadErrorAnyOf e m '[ByteError]
    => Int
    -> m SignableInt
toUnsignedInt = toSignableInt Unsigned

bytesNeededUnsigned :: Int -> Int
bytesNeededUnsigned n = 1
    + floor ((logBase 256.0 $ fromIntegral $ abs n) :: Double)

bytesNeeded :: SignableInt -> Int
bytesNeeded = \case
    SignableInt _ 0        -> 1
    SignableInt Unsigned n -> bytesNeededUnsigned n
    SignableInt Signed n   ->
        let nrBytesUnsigned = bytesNeededUnsigned n
        in
            if isSignedNrExpressibleInNBits (8 * nrBytesUnsigned) n
            then nrBytesUnsigned
            else nrBytesUnsigned + 1
