{-# LANGUAGE RankNTypes #-}

module HeX.Parse.SyntaxCommand where

import           HeXlude

import qualified Data.Sequence          as Seq

import qualified HeX.Categorise         as Cat
import           HeX.Parse.Stream.Class
import           HeX.Parse.Token

parseCSNameArgs :: TeXParser s e m (Seq Cat.CharCode)
parseCSNameArgs =
    do
    cs <- Seq.fromList <$> parseManyChars
    satisfyEquals (SyntaxCommandArg EndCSNameTok)
    pure cs
