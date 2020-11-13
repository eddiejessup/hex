module Hex.Parse.CommandParser.SyntaxCommand where

import           Hexlude

import qualified Hex.Config.Codes       as Code
import           Hex.Parse.TokenParser.Class
import           Hex.Parse.TokenParser.Combinators
import           Hex.Resolve.Token

parseCSNameArgs :: MonadTokenParse m => m [Code.CharCode]
parseCSNameArgs =
    do
    cs <- parseManyChars
    satisfyEquals (SyntaxCommandArg EndCSNameTok)
    pure cs
