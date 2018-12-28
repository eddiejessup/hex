{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module HeX.Parse.Expanded.Stream where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Proxy
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( (!?) )
import qualified Data.Set                      as Set
import qualified Data.List.NonEmpty            as NE
import qualified Data.Foldable                 as Fold
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

copyPosState :: P.PosState s -> s' -> P.PosState s'
copyPosState posState tgtStream
  =
    let
      P.PosState { pstateInput = _
                 , pstateOffset = offset
                 , pstateSourcePos = sourcePos
                 , pstateTabWidth = tabWidth
                 , pstateLinePrefix = linePrefix } = posState
    in
      P.PosState { pstateInput = tgtStream
                 , pstateOffset = offset
                 , pstateSourcePos = sourcePos
                 , pstateTabWidth = tabWidth
                 , pstateLinePrefix = linePrefix }

copyState :: P.State s -> s' -> P.State s'
copyState state tgtStream
  =
    let
      P.State { stateInput = _
              , stateOffset = offset
              , statePosState = posState } = state
    in
      P.State { stateInput = tgtStream
              , stateOffset = offset
              , statePosState = copyPosState posState tgtStream }

parseInhibited :: SimpLexParser a -> SimpExpandParser a
parseInhibited p = do
  P.State { stateInput = ExpandedStream (R.ResolvedStream lexStream csMap)
          , stateOffset = _
          , statePosState = _ } <- P.getParserState
  case easyRunParser p lexStream of
    (_, Left s) -> error $ "ohnoes: " ++ show s
    (newLexState, Right v) -> do
      let newLexStream = P.stateInput newLexState
      let newExpStream = ExpandedStream $ R.ResolvedStream newLexStream csMap
      P.setParserState $ copyState newLexState newExpStream
      pure v

parseGeneralText :: SimpExpandParser BalancedText
parseGeneralText = do
  skipManySatisfied isFillerItem
  -- TODO: Maybe other things can act as left braces.
  skipSatisfied isExplicitLeftBrace
  parseInhibited $ parseBalancedText Discard

parseCSNameArgs :: SimpExpandParser [CharCode]
parseCSNameArgs = do
  chars <- parseManyChars
  skipSatisfiedEquals (R.SyntaxCommandArg R.EndCSName)
  pure chars

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

sinfulRunParser :: SimpParser s a -> s -> (P.State s, a)
sinfulRunParser p stream =
  case easyRunParser p stream of
    (_, Left _) -> error "Error while parsing command"
    (state, Right a) -> (state, a)

instance P.Stream ExpandedStream where
  type Token ExpandedStream = PrimitiveToken

  -- 'Tokens' is synonymous with 'chunk' containing 'token's.
  type Tokens ExpandedStream = [PrimitiveToken]

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
          let (P.State es'' _ _, BalancedText caseToks) = sinfulRunParser parseGeneralText es'
          -- Perform take1_ on the stream after parsing, with the new
          -- tokens inserted.
          in (P.take1_ . insertLexTokensE es'') $ changeCase direction <$> caseToks
        (MacroToken m@MacroContents {replacementTokens=(MacroText replaceToks)}) ->
          let
            (P.State es'' _ _, args) = sinfulRunParser (parseInhibited $ parseMacroArgs m) es'
            renderedToks = renderMacroText replaceToks args
          in
            (P.take1_ . insertLexTokensE es'') renderedToks
        CSName ->
          let
            (P.State es'' _ _, charToks) = sinfulRunParser parseCSNameArgs es'
              -- TODO: if control sequence doesn't exist, define one that holds
              -- '\relax'.
          in
            (P.take1_ . insertLexTokenE es'' . Lex.ControlSequenceToken . Lex.ControlSequence) charToks

  takeN_ = undefined

  takeWhile_ = undefined

  showTokens Proxy = show

  reachOffset _ _freshState = (freshSourcePos, "", _freshState)

insertLexTokenE :: ExpandedStream -> Lex.Token -> ExpandedStream
insertLexTokenE (ExpandedStream rs) t = ExpandedStream (insertLexTokenR rs t)

insertLexTokensE :: ExpandedStream -> [Lex.Token] -> ExpandedStream
insertLexTokensE (ExpandedStream rs) ts =
  ExpandedStream (insertLexTokensR rs ts)

instance Eq ExpandedStream where
  _ == _ = True

instance Ord (P.ParseErrorBundle ExpandedStream ()) where
  compare _ _ = EQ

instance P.ShowErrorComponent (P.ParseErrorBundle ExpandedStream ()) where
  showErrorComponent (P.ParseErrorBundle errs posState) =
    Fold.concat $ NE.intersperse "\n\n" $ P.showErrorComponent <$> errs

instance Ord (P.ParseError ExpandedStream ()) where
  compare _ _ = EQ

instance P.ShowErrorComponent (P.ParseError ExpandedStream ()) where
  showErrorComponent (P.TrivialError offset (Just (P.Tokens unexpecteds)) expecteds) =
    "Error at " ++ show offset ++ ".\n"
    ++ "Found unexpected tokens: " ++ show (NE.toList unexpecteds) ++ ".\n"
    ++ "Expected one of: " ++ show (Set.toList expecteds)
