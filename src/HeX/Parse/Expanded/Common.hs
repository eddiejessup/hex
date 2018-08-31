{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Parse.Expanded.Common where

import qualified Data.Char as C
import Data.Functor (($>))
import Data.Maybe (isJust)
import qualified Text.Megaparsec as P

import qualified HeX.Lex as Lex

import HeX.Parse.Helpers
import HeX.Parse.Lexed
import qualified HeX.Parse.Resolved as R

import HeX.Parse.Expanded.Stream

