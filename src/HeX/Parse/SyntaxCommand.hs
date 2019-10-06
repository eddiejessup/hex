{-# LANGUAGE RankNTypes #-}

module HeX.Parse.SyntaxCommand where

import           HeXlude

import qualified Data.Sequence          as Seq

import qualified HeX.Config.Codes       as Code
import           HeX.Parse.Stream.Class
import           HeX.Parse.Token

parseCSNameArgs :: TeXParser s e m (Seq Code.CharCode)
parseCSNameArgs =
    do
    cs <- Seq.fromList <$> parseManyChars
    satisfyEquals (SyntaxCommandArg EndCSNameTok)
    pure cs
