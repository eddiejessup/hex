{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.Stream.Class where

import qualified Data.Sequence as Seq
import qualified Hex.Lex as Lex
import Hexlude hiding (many)
import Path (Abs, File, Path)
import qualified Data.ByteString as BS

class TeXStream s where

  insertLexToken :: s -> Lex.Token -> s

  extractLexToken :: (MonadError e m, AsType Lex.LexError e) => s -> m (Maybe (Lex.Token, s))

  addTokenSource :: s -> TokenSource -> s

data TokenSource
  = TokenSource
      { sourcePath :: Maybe (Path Abs File)
      , sourceCharCodes :: BS.ByteString
      , sourceLexTokens :: Seq Lex.Token
      }
  deriving stock (Show, Generic)

instance Describe TokenSource where

  describe TokenSource {sourcePath, sourceCharCodes, sourceLexTokens} =
      [ (0, "TokenSource")
      ,   (1, "path " <> quote (show sourcePath))
      ,   (1, "codes")
      ,     (2, decodeUtf8 (BS.take 50 sourceCharCodes))
      ]
      <> describeNamedRelFoldable1 "lexTokens" (Seq.take 10 sourceLexTokens)

newTokenSource :: Maybe (Path Abs File) -> BS.ByteString -> TokenSource
newTokenSource maybePath cs = TokenSource maybePath cs mempty

newtype ExpansionError = ExpansionError Text
  deriving stock Show

insertLexTokensToStream :: TeXStream s => s -> Seq Lex.Token -> s
insertLexTokensToStream s (ts :|> t) = insertLexTokensToStream (insertLexToken s t) ts
insertLexTokensToStream s Empty = s
