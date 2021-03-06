module TFM.LigKern where

import           Hexlude

import qualified Data.Binary.Get as B.G

import           TFM.Common

kernOp :: Int
kernOp = 128

data LigatureOp = LigatureOp
    { ligatureChar
    , charsToPassOver :: Int
    , deleteCurrentChar
    , deleteNextChar :: Bool
    } deriving stock (Show)

newtype KernOp = KernOp Rational
    deriving stock (Show)

data LigKernInstr = LigKernInstr
    { stop      :: Bool
    , nextChar  :: Int
    , operation :: Either LigatureOp KernOp
    } deriving stock (Show)

readLigKern :: MonadError Text m => [Rational] -> LigKernCommand -> m LigKernInstr
readLigKern kerns (LigKernCommand _skipByte _nextChar _opByte _remainder) =
    do
    op <- if _opByte >= kernOp
        then do
            let idx = 256 * (_opByte - kernOp) + _remainder
            k <- note ("No kern at index " <> show idx) $ atMay kerns idx
            pure $ Right $ KernOp k
        else pure $ Left LigatureOp
            { ligatureChar      = _remainder
            , charsToPassOver   = _opByte `shift` 2
            , deleteCurrentChar = (_opByte .&. 0x02) == 0
            , deleteNextChar    = (_opByte .&. 0x01) == 0
            }
    pure LigKernInstr
        { stop = _skipByte >= 128
        , nextChar = _nextChar
        , operation = op
        }

    -- TODO: Implement
    -- when (firstSkipByte == 255) $ Left "Unsupported: Right boundary characters"
    -- when (firstSkipByte > 128) $ Left "Unsupported: Large LigKern arrays"
    -- when (lastSkipByte == 255) $ Left "Unsupported: Left boundary characters"
  -- where
    -- edgesMay xs = do
    --     hd <- headMay xs
    --     lst <- lastMay xs
    --     pure (hd, lst)

data LigKernCommand = LigKernCommand
    { skipByte
    , commandNextChar
    , opByte
    , remainder :: Int
    } deriving stock (Show)

getLigKernCommand :: B.G.Get LigKernCommand
getLigKernCommand =
    LigKernCommand <$> getWord8Int <*> getWord8Int <*> getWord8Int <*> getWord8Int
