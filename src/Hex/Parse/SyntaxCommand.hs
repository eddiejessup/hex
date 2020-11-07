{-# LANGUAGE RankNTypes #-}

module Hex.Parse.SyntaxCommand where

import           Hexlude

import qualified Hex.Config.Codes       as Code
import           Hex.Parse.Stream.Class
import           Hex.Resolve.Token

parseCSNameArgs :: TeXParseCtx st e m => m [Code.CharCode]
parseCSNameArgs =
    do
    cs <- parseManyChars
    satisfyEquals (SyntaxCommandArg EndCSNameTok)
    pure cs
