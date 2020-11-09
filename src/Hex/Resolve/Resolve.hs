module Hex.Resolve.Resolve where

import           Hexlude

import qualified Hex.Config.Codes    as Code
import qualified Hex.Lex             as Lex
import           Hex.Resolve.Token
import Data.Conduit
import qualified Data.Conduit.Combinators as C

data ResolutionMode = Resolving | NotResolving
    deriving stock ( Show, Eq )

resolveToken :: (Lex.ControlSequenceLike -> Maybe ResolvedToken)
             -> ResolutionMode
             -> Lex.Token
             -> Maybe ResolvedToken
resolveToken _ NotResolving t = pure $ PrimitiveToken $ UnresolvedTok t
resolveToken csLookup Resolving t = case t of
    Lex.ControlSequenceToken cs -> csLookup $ Lex.ControlSequenceProper cs
    Lex.CharCatToken (Lex.CharCat c Code.Active) -> csLookup $ Lex.ActiveCharacter c
    _ -> pure $ PrimitiveToken $ UnresolvedTok t

resolveTokenConduit ::
  Monad m
  => (Lex.ControlSequenceLike -> Maybe ResolvedToken)
  -> ResolutionMode
  -> ConduitT Lex.Token (Maybe ResolvedToken) m ()
resolveTokenConduit csLookup resMode = C.map (resolveToken csLookup resMode)
