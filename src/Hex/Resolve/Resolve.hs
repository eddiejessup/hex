module Hex.Resolve.Resolve where

import           Hexlude

import qualified Hex.Config.Codes    as Code
import qualified Hex.Lex             as Lex
import           Hex.Resolve.Token
import           Hex.Resolve.Map
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.HashMap.Strict (lookup)

data ResolutionMode = Resolving | NotResolving
    deriving stock ( Show, Eq )

newtype ResolutionError = ResolutionError Text
  deriving stock Show

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

defaultResolveTokenConduit :: Monad m => ConduitT Lex.Token (Maybe ResolvedToken) m ()
defaultResolveTokenConduit = resolveTokenConduit (\k -> lookup k defaultCSMap) Resolving
