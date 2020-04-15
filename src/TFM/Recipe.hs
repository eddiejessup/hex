module TFM.Recipe where

import           Hexlude

import qualified Data.Binary.Get as B.G

import           TFM.Common

data ExtensibleRecipe = ExtensibleRecipe
    { top
    , middle
    , bottom
    , repeater :: Int
    } deriving stock (Show)

getExtensibleRecipe :: B.G.Get ExtensibleRecipe
getExtensibleRecipe =
    ExtensibleRecipe <$> getWord8Int <*> getWord8Int <*> getWord8Int <*> getWord8Int
