{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.TokenParser.Class where

import Hex.Evaluate
import qualified Hex.Lex as Lex
import Hex.Resolve
import Hexlude hiding (many)
import Path (Rel, File, Path)

data ParseError
  = ParseErrorWithMsg Text
  | EndOfInput
  | ExplicitFailure
  deriving stock Show

data ExpandedToken
  = ExpandedPrimitiveToken PrimitiveToken
  | ExpandedSyntaxCommand SyntaxCommandHeadToken (Seq Lex.Token)

class MonadPlus m => MonadTokenParse m where

  parseError :: ParseError -> m a

  satisfyThen :: (PrimitiveToken -> Maybe a) -> m a

  withInhibition :: m a -> m a

  takeWhileP :: (PrimitiveToken -> Bool) -> m (Seq PrimitiveToken)

  takeLexToken :: m Lex.Token

  takeAndResolveLexToken :: m (Lex.Token, ResolvedToken)

  takeAndExpandResolvedToken :: m (Lex.Token, ExpandedToken)

  pushSkipState :: ConditionBodyState -> m ()

  peekSkipState :: m (Maybe ConditionBodyState)

  popSkipState :: m (Maybe ConditionBodyState)

  inputPath :: Path Rel File -> m ()

  insertLexTokens :: Seq Lex.Token -> m ()
