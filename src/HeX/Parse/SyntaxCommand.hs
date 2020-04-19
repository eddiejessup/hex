{-# LANGUAGE RankNTypes #-}

module Hex.Parse.SyntaxCommand where

import           Hexlude

import qualified Data.Sequence          as Seq

import qualified Hex.Config.Codes       as Code
import           Hex.Parse.Stream.Class
import           Hex.Resolve.Token

parseCSNameArgs :: TeXParser s st e m (Seq Code.CharCode)
parseCSNameArgs =
    do
    cs <- Seq.fromList <$> parseManyChars
    satisfyEquals (SyntaxCommandArg EndCSNameTok)
    pure cs
