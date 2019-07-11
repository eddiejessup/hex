{-# LANGUAGE RankNTypes #-}

module HeX.Parse.SyntaxCommand where

import           HeXlude

import qualified HeX.Categorise         as Cat
import           HeX.Parse.Stream.Class
import           HeX.Parse.Token

parseCSNameArgs :: TeXParser s (ForwardDirected [] Cat.CharCode)
parseCSNameArgs =
    FDirected <$> (parseManyChars <* skipSatisfiedEquals (SyntaxCommandArg EndCSNameTok))
