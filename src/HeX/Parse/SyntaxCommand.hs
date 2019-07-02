{-# LANGUAGE RankNTypes #-}

module HeX.Parse.SyntaxCommand where

import HeXlude

import qualified HeX.Categorise                as Cat
import           HeX.Parse.Inhibited
import           HeX.Parse.Token

parseCSNameArgs :: TeXPrimParser s (ForwardDirected [] Cat.CharCode)
parseCSNameArgs =
    FDirected <$> (parseManyChars <* skipSatisfiedEquals (SyntaxCommandArg EndCSNameTok))
