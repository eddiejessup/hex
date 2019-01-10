{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module HeX.Parse.Stream where

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

import           HeX.Concept
import qualified HeX.Categorise                as Cat
import qualified HeX.Lex                       as Lex
import           HeX.Parse.Helpers
import           HeX.Parse.Token
import           HeX.Parse.Common
import           HeX.Parse.Inhibited
import           HeX.Parse.Resolve

data ExpandedStream = ExpandedStream { codes :: [Cat.CharCode]
                                     , lexTokens :: [Lex.Token]
                                     , lexState :: Lex.LexState
                                     , ccMap :: Cat.CharCatMap
                                     , csMap :: CSMap
                                     , expansionMode :: ExpansionMode }
  deriving (Show)

type SimpExpandParser = SimpParser ExpandedStream
type NullSimpExpandParser = SimpExpandParser ()

newExpandStream :: [Cat.CharCode] -> CSMap -> ExpandedStream
newExpandStream cs _csMap = ExpandedStream { codes = cs
                                         , lexTokens = []
                                         , lexState = Lex.LineBegin
                                         , ccMap = Cat.usableCharCatMap
                                         , csMap = _csMap
                                         , expansionMode = Expanding }

-- TODO: This use of reverse is pure sloth; fix later.
insertLexTokens :: ExpandedStream -> [Lex.Token] -> ExpandedStream
insertLexTokens s ts = Fold.foldl' insertLexToken s $ reverse ts

insertLexToken :: ExpandedStream -> Lex.Token -> ExpandedStream
insertLexToken s t = s {lexTokens = t : lexTokens s}

-- Inhibition.

setExpansion :: ExpansionMode -> NullSimpExpandParser
setExpansion m = P.updateParserState $ setStateExpansion
  where
    setStateExpansion est@P.State{stateInput=es}
      = est{P.stateInput=es { expansionMode = m }}

inhibitExpansion, enableExpansion :: NullSimpExpandParser
inhibitExpansion = setExpansion NotExpanding
enableExpansion = setExpansion Expanding

parseInhibited :: SimpExpandParser a -> SimpExpandParser a
parseInhibited p = do
  inhibitExpansion
  v <- p
  enableExpansion
  pure v

-- Interface.

parseBalancedText :: TerminusPolicy -> SimpExpandParser BalancedText
parseBalancedText = parseInhibited . unsafeParseBalancedText

parseMacroArgs :: MacroContents -> SimpExpandParser (Map.Map Digit MacroArgument)
parseMacroArgs = parseInhibited . unsafeParseMacroArgs

parseCharLike :: SimpExpandParser Integer
parseCharLike = parseInhibited unsafeParseCharLike

parseCSName :: SimpExpandParser Lex.ControlSequenceLike
parseCSName = parseInhibited unsafeParseCSName

parseParamText :: SimpExpandParser ([Lex.Token], MacroParameters)
parseParamText = parseInhibited unsafeParseParamText

parseMacroText :: SimpExpandParser MacroText
parseMacroText = parseInhibited unsafeParseMacroText




-- Expanding syntax commands.

-- TODO: Move these parsers outside the module.

parseGeneralText :: SimpExpandParser BalancedText
parseGeneralText = do
  skipManySatisfied isFillerItem
  -- TODO: Maybe other things can act as left braces.
  skipSatisfied $ primTokHasCategory Lex.BeginGroup
  parseBalancedText Discard

parseCSNameArgs :: SimpExpandParser [Cat.CharCode]
parseCSNameArgs = do
  chars <- parseManyChars
  skipSatisfiedEquals (SyntaxCommandArg EndCSNameTok)
  pure chars

expandCSName
  :: ()
  -> String
  -> [Lex.Token]
expandCSName _ charToks
  -- TODO: if control sequence doesn't exist, define one that holds
  -- '\relax'.
  = [Lex.ControlSequenceToken $ Lex.ControlSequence charToks]

expandMacro
  :: MacroContents
  -> Map.Map Digit MacroArgument
  -> [Lex.Token]
expandMacro MacroContents {replacementTokens=(MacroText replaceToks)} args
  = renderMacroText replaceToks
  where
    renderMacroText [] = []
    renderMacroText (t:ts) = render t
      where
        render (MacroTextLexToken x) =
          x:rest
        render (MacroTextParamToken dig) =
          case args !? dig of
            Nothing -> error "No such parameter"
            Just (MacroArgument arg) -> arg ++ rest

        rest = renderMacroText ts

-- Change the case of the parsed tokens.
expandChangeCase
  :: VDirection
  -> BalancedText
  -> [Lex.Token]
expandChangeCase direction (BalancedText caseToks)
  = changeCase direction <$> caseToks
  where
    -- Set the character code of each character token to its
    -- \uccode or \lccode value, if that value is non-zero.
    -- Don't change the category code.
    changeCase dir (Lex.CharCatToken (Lex.CharCat char cat)) =
      Lex.CharCatToken $ Lex.CharCat (switch dir char) cat
    changeCase _ t = t

    switch Upward   = toUpper
    switch Downward = toLower


runSyntaxCommand
  :: a
  -> SimpExpandParser b
  -> ExpandedStream
  -> (a -> b -> [Lex.Token])
  -> Maybe (PrimitiveToken, ExpandedStream)
runSyntaxCommand com parser inputStream f =
  case easyRunParser parser inputStream of
    (P.State resultStream _ _, Left err) ->
      pure (SubParserError $ show err, resultStream)
    (P.State resultStream _ _, Right a) ->
      -- Expand the command, using the command and the result of the parse,
      -- insert the result into the remaining stream, then try again to get a
      -- primitive token.
      (P.take1_ . insertLexTokens resultStream) $ f com a




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
  -- If we've no input, signal that we are done.
  take1_ ExpandedStream { codes = [] } = Nothing
  take1_ stream@(ExpandedStream cs lexBuff _lexState _ccMap _csMap expMode) = do
    -- Get the next lex token, and update our stream.
    (lt, es') <- case lexBuff of
      -- If there is a lex token in the buffer, use that.
      (lt:lts) ->
        pure (lt, stream { lexTokens = lts })
      -- If the lex token buffer is empty, extract a token and use it.
      [] -> do
        let getCC = Cat.extractCharCat (Cat.catLookup _ccMap)
        (lt, _lexState', cs') <- Lex.extractToken getCC _lexState cs
        pure (lt, stream { codes = cs', lexState = _lexState' })
    -- Resolve the lex token, and inspect the result.
    case resolveToken _csMap expMode lt of
      -- If it's a primitive token, provide that.
      PrimitiveToken pt -> pure (pt, es')
      -- If it indicates the start of a syntax command.
      -- Parse the remainder of the syntax command.
      SyntaxCommandHead c -> case c of
        (ChangeCaseTok direction) ->
          runSyntaxCommand direction parseGeneralText es' expandChangeCase
        (MacroTok m) ->
          runSyntaxCommand m (parseMacroArgs m) es' expandMacro
        CSNameTok ->
          runSyntaxCommand () parseCSNameArgs es' expandCSName

  takeN_ = undefined

  takeWhile_ = undefined

  showTokens Proxy = show

  reachOffset _ _freshState = (freshSourcePos, "", _freshState)

instance Eq ExpandedStream where
  _ == _ = True

instance Ord (ParseErrorBundle ExpandedStream) where
  compare _ _ = EQ

instance P.ShowErrorComponent (ParseErrorBundle ExpandedStream) where
  showErrorComponent (P.ParseErrorBundle errs _) =
    Fold.concat $ NE.intersperse "\n\n" $ P.showErrorComponent <$> errs

instance Ord (ParseError ExpandedStream) where
  compare _ _ = EQ

instance P.ShowErrorComponent (ParseError ExpandedStream) where
  showErrorComponent (P.TrivialError offset (Just (P.Tokens unexpecteds)) expecteds) =
    "Error at " ++ show offset ++ ".\n"
    ++ "Found unexpected tokens: " ++ show (NE.toList unexpecteds) ++ ".\n"
    ++ "Expected one of: " ++ show (Set.toList expecteds)

  showErrorComponent (P.TrivialError offset Nothing expecteds) =
    "Error at " ++ show offset ++ ".\n"
    ++ "Found no unexpected tokens.\n"
    ++ "Expected one of: " ++ show (Set.toList expecteds)

  showErrorComponent (P.TrivialError offset (Just P.EndOfInput) expecteds) =
    "Error at " ++ show offset ++ ".\n"
    ++ "Found end of input.\n"
    ++ "Expected one of: " ++ show (Set.toList expecteds)
