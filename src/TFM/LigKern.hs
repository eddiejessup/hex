module TFM.LigKern where

import qualified Data.Binary.Get as B.G
import           Data.Bits ( shift, (.&.) )

import           TFM.Common

kernOp :: Int
kernOp = 128

data LigatureOp = LigatureOp
    { ligatureChar
    , charsToPassOver :: Int
    , deleteCurrentChar
    , deleteNextChar :: Bool
    } deriving (Show)

data KernOp = KernOp Rational
    deriving (Show)

data LigKernInstr = LigKernInstr
    { stop :: Bool
    , nextChar :: Int
    , operation :: Either LigatureOp KernOp
    } deriving (Show)

readLigKern :: [Rational] -> LigKernCommand -> LigKernInstr
readLigKern kerns (LigKernCommand _skipByte _nextChar _opByte _remainder) =
    LigKernInstr
        { stop = _skipByte >= 128
        , nextChar = _nextChar
        , operation = if _opByte >= kernOp
            then Right $ KernOp $ kerns !! (256 * (_opByte - kernOp) + _remainder)
            else Left LigatureOp
                { ligatureChar      = _remainder
                , charsToPassOver   = _opByte `shift` 2
                , deleteCurrentChar = (_opByte .&. 0x02) == 0
                , deleteNextChar    = (_opByte .&. 0x01) == 0
                }
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
    } deriving (Show)

getLigKernCommand :: B.G.Get LigKernCommand
getLigKernCommand =
    LigKernCommand <$> getWord8Int <*> getWord8Int <*> getWord8Int <*> getWord8Int
