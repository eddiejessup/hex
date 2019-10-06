{-# OPTIONS_GHC -fno-warn-orphans #-}

module HeX.Orphans where

import Data.Foldable
import Data.Sequence
import Data.Hashable

instance Hashable a => Hashable (Seq a) where
    hashWithSalt s x = hashWithSalt s (toList x)
