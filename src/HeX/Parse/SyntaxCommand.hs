module HeX.Parse.SyntaxCommand where

import HeXlude

import qualified HeX.Categorise                as Cat
import           HeX.Parse.Common
import           HeX.Parse.Helpers
import           HeX.Parse.Inhibited
import           HeX.Parse.Token

parseCSNameArgs :: InhibitableStream s => SimpParser s (ForwardDirected [] Cat.CharCode)
parseCSNameArgs =
    FDirected <$> (parseManyChars <* skipSatisfiedEquals (SyntaxCommandArg EndCSNameTok))
