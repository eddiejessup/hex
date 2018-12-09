{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Expanded.Stream where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Proxy
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( (!?) )
import qualified Text.Megaparsec               as P

import           HeX.Categorise                 ( CharCode )
import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers
import           HeX.Parse.Lexed
import qualified HeX.Parse.Lexed.Inhibited     as Inh
import           HeX.Parse.Resolved             ( PrimitiveToken )
import           HeX.Parse.Resolved            as R
import           HeX.Parse.Expanded.Common

newtype ExpandedStream =
  ExpandedStream R.ResolvedStream
  deriving (Show)

type SimpExpandParser = SimpParser ExpandedStream

newExpandStream :: [CharCode] -> R.CSMap -> ExpandedStream
newExpandStream cs = ExpandedStream . newResolvedStream cs

type PrimitiveTokens = [PrimitiveToken]

-- Set the character code of each character token to its
-- \uccode or \lccode value, if that value is non-zero.
-- Don't change the category code.
changeCase :: VDirection -> Lex.Token -> Lex.Token
changeCase dir (Lex.CharCatToken (Lex.CharCat char cat)) =
  Lex.CharCatToken $ Lex.CharCat (switch dir char) cat
 where
  switch R.Upward   = toUpper
  switch R.Downward = toLower
changeCase _ t = t

-- Things I can't easily parse outside this module, because of the recursive
-- parsing in ExpandedStream.
-- Constraining only the stream token type won't work easily either, because
-- the functions currently depend on the ExpandedStream type per se.
parseInhibited :: SimpLexParser a -> SimpExpandParser a
parseInhibited p = do
  P.State { stateInput = ExpandedStream (R.ResolvedStream lStream csMap) } <-
    P.getParserState
  case easyRunParser p lStream of
    (_                         , Left s ) -> error $ "ohnoes: " ++ show s
    (P.State lStream' pos prc w, Right v) -> do
      P.setParserState
        (P.State (ExpandedStream $ R.ResolvedStream lStream' csMap) pos prc w)
      pure v

parseGeneralText :: SimpExpandParser BalancedText
parseGeneralText = do
  skipManySatisfied isFillerItem
  -- TODO: Maybe other things can act as left braces.
  skipSatisfied isExplicitLeftBrace
  parseInhibited $ parseBalancedText Discard
 where
  isFillerItem R.Relax = True
  isFillerItem t       = isSpace t

parseCSNameArgs :: SimpExpandParser [CharCode]
parseCSNameArgs =
  parseManyChars <* skipSatisfiedEquals (R.SyntaxCommandArg R.EndCSName)

renderMacroText :: [MacroTextToken] -> Map.Map Digit Inh.MacroArgument -> [Lex.Token]
renderMacroText [] _ = []
renderMacroText (t:ts) args = render t
  where
    render (MacroTextLexToken x) =
      x:rest
    render (MacroTextParamToken dig) =
      case args !? dig of
        Nothing -> error "No such parameter"
        Just (MacroArgument arg) -> arg ++ rest

    rest = renderMacroText ts args

instance P.Stream ExpandedStream where
  type Token ExpandedStream = PrimitiveToken
  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens ExpandedStream = PrimitiveTokens
  -- These basically clarify that, for us, a 'tokens' is a list of type
  -- 'token'.
  -- tokenToChunk :: Proxy s -> Token s -> Tokens s
  -- To make a 'token' into a 'tokens', wrap it in a list.
  tokenToChunk Proxy = pure
  -- tokensToChunk :: Proxy s -> [Token s] -> Tokens s
  -- A list of type 'token' is equivalent to a 'tokens', and vice versa.
  tokensToChunk Proxy = id
  -- chunkToTokens :: Proxy s -> Tokens s -> [Token s]
  chunkToTokens Proxy = id
  -- chunkLength :: Proxy s -> Tokens s -> Int
  -- The length of a chunk is the number of elements in it (it's a list).
  chunkLength Proxy = length
  -- chunkEmpty :: Proxy s -> Tokens s -> Bool
  -- A chunk is empty if it has no elements.
  chunkEmpty Proxy = null
  -- Stub implementation: leave position unchanged.
  advance1 Proxy _ pos _ = pos
  advanceN Proxy _ pos _ = pos
  -- take1_ :: s -> Maybe (Token s, s)
  take1_ (ExpandedStream rs)
    -- Get the next resolved token.
   = do
    (rt, rs') <- P.take1_ rs
    let es' = ExpandedStream rs'
    case rt of
      -- If it's a primitive token, provide that.
      PrimitiveToken pt -> pure (pt, es')
      -- If it indicates the start of a syntax command.
      -- Parse the remainder of the syntax command.
      SyntaxCommandHead c -> case c of
        (ChangeCaseToken direction) ->
          case easyRunParser parseGeneralText es' of
            (_, Left parseError) -> error $ "Error while parsing changecase command: " ++ show parseError
            (P.State es'' _ _ _, Right (BalancedText caseToks)) ->
              -- Now perform take1_ on the stream after parsing, with the new
              -- tokens inserted.
              (P.take1_ . insertLexTokensE es'') $ changeCase direction <$> caseToks
        (MacroToken m@MacroContents {replacementTokens=(MacroText replaceToks)}) ->
            case easyRunParser (parseInhibited $ parseMacroArgs m) es' of
              (_, Left parseError) -> error $ "Error while parsing macro arguments: " ++ show parseError
              (P.State es'' _ _ _, Right args) ->
                let renderedToks = renderMacroText replaceToks args
                in (P.take1_ . insertLexTokensE es'') renderedToks
        CSName ->
          case easyRunParser parseCSNameArgs es' of
            (_, Left parseError) -> error $ "Error while parsing csname arguments: " ++ show parseError
            (P.State es'' _ _ _, Right charToks) ->
              -- TODO: if control sequence doesn't exist, define one that holds
              -- '\relax'.
              (P.take1_ . insertLexTokenE es'' . Lex.ControlSequenceToken . Lex.ControlSequence) charToks

  takeN_ = undefined
  takeWhile_ = undefined

insertLexTokenE :: ExpandedStream -> Lex.Token -> ExpandedStream
insertLexTokenE (ExpandedStream rs) t = ExpandedStream (insertLexTokenR rs t)

insertLexTokensE :: ExpandedStream -> [Lex.Token] -> ExpandedStream
insertLexTokensE (ExpandedStream rs) ts =
  ExpandedStream (insertLexTokensR rs ts)
