{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.SyntaxCommand where

import qualified HeX.Categorise                as Cat
import           HeX.Parse.Common
import           HeX.Parse.Helpers
import           HeX.Parse.Inhibited
import           HeX.Parse.Token

parseCSNameArgs :: InhibitableStream s => SimpParser s [Cat.CharCode]
parseCSNameArgs = parseManyChars <* (skipSatisfiedEquals $ SyntaxCommandArg EndCSNameTok)
