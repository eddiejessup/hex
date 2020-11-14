{-# LANGUAGE UndecidableInstances #-}

module Hex.Test.Parse where

import qualified Control.Monad.Combinators as PC
import qualified Data.Map.Strict as Map
import Data.Path (MonadInput (..), PathError (..))
import qualified Data.Sequence as Seq
import qualified Hex.Config as Conf
import Hex.Config.Codes
import Hex.Evaluate (ConditionBodyState, EvaluationError)
import Hex.Lex
import qualified Hex.Lex as Lex
import Hex.Parse.AST
import Hex.Parse.CommandParser.Assignment (parseAssignment)
import Hex.Parse.Stream.Class
import qualified Hex.Parse.Stream.Class as S
import Hex.Parse.Stream.Parse ()
import Hex.Parse.TokenParser.Class (MonadTokenParse (..))
import qualified Hex.Parse.TokenParser.Class as P
import qualified Hex.Parse.TokenParser.Combinators as P
import Hex.Parse.TokenParser.ParseT
import Hex.Resolve
import qualified Hex.Resolve.Resolve as R
import Hexlude
import Path (File, Path, Rel)
import qualified System.Log.FastLogger as Log
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Parse"
    [ testEmpty,
      testRelax,
      testLetter,
      testCSName,
      testString,
      testChangeCase,
      testMacroDef
    ]

wordOrd :: Char -> Word8
wordOrd = fromIntegral . ord

ccOrd :: Char -> CharCode
ccOrd = CharCode . wordOrd

cc :: Char -> CoreCatCode -> Token
cc c cat = CharCatToken $ CharCat (ccOrd c) cat

letterWord :: [Char] -> [Token]
letterWord chrs = chrs <&> flip cc Letter

cs :: [Char] -> ControlSequence
cs = mkControlSequence . fmap ccOrd

csT :: [Char] -> Token
csT = ControlSequenceToken . cs

oCC :: Char -> CoreCatCode -> PrimitiveToken
oCC c cat = UnresolvedTok $ cc c cat

oLetterWord :: [Char] -> [PrimitiveToken]
oLetterWord chrs = chrs <&> flip oCC Letter

-- No instance for (MonadTokenParse (TeXParseT TestStream IO))
--     arising from a use of ‘anySingle’
data TestStream = TestStream [Lex.Token]

data TestError
  = ConfigAppError Conf.ConfigError
  | EvaluationAppError EvaluationError
  | PathAppError PathError
  | ExpansionAppError S.ExpansionError
  | ResolutionAppError R.ResolutionError
  | ParseAppError P.ParseError
  | LexAppError LexError
  deriving stock (Show, Generic)

instance TeXStream TestStream where
  insertLexToken (TestStream ts) t = TestStream (t : ts)

  extractLexToken (TestStream ts) = pure $ case ts of
    t : ts' -> Just (t, TestStream ts')
    [] -> Nothing

  addTokenSource (TestStream _ts) _tokSrc = panic "Cannot add token source"

newtype FailingInputT m a = FailingInputT {unFailingInputT :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadState s, MonadError e)

instance (Monad m, MonadError e m, AsType PathError e) => MonadInput (FailingInputT m) where
  findPath _policy tgt _searchDirs = throwError $ injectTyped $ PathError $ "Could not find file: " <> show tgt

  readPathBytes path = throwError $ injectTyped $ PathError $ "Could not read file: " <> show path

newtype TestApp a = TestApp {unTestApp :: TeXParseT TestStream (FailingInputT (ExceptT TestError (StateT Conf.Config Identity))) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState Conf.Config,
      MonadError TestError,
      Alternative,
      MonadPlus
    )

instance P.MonadTokenParse TestApp where
  parseError :: P.ParseError -> TestApp a
  parseError = TestApp . parseError

  satisfyThen :: (PrimitiveToken -> Maybe a) -> TestApp a
  satisfyThen = TestApp . satisfyThen

  withInhibition :: TestApp a -> TestApp a
  withInhibition = TestApp . withInhibition . unTestApp

  takeWhileP :: (PrimitiveToken -> Bool) -> TestApp (Seq PrimitiveToken)
  takeWhileP = TestApp . takeWhileP

  takeLexToken :: TestApp Lex.Token
  takeLexToken = TestApp takeLexToken

  takeAndResolveLexToken :: TestApp (Lex.Token, ResolvedToken)
  takeAndResolveLexToken = TestApp takeAndResolveLexToken

  takeAndExpandResolvedToken :: TestApp (Lex.Token, P.ExpandedToken)
  takeAndExpandResolvedToken = TestApp takeAndExpandResolvedToken

  pushSkipState :: ConditionBodyState -> TestApp ()
  pushSkipState = TestApp . pushSkipState

  peekSkipState :: TestApp (Maybe ConditionBodyState)
  peekSkipState = TestApp peekSkipState

  popSkipState :: TestApp (Maybe ConditionBodyState)
  popSkipState = TestApp popSkipState

  inputPath :: Path Rel File -> TestApp ()
  inputPath = TestApp . inputPath

  insertLexTokens :: Seq Lex.Token -> TestApp ()
  insertLexTokens = TestApp . insertLexTokens

runTestApp ::
  TestApp a ->
  Conf.Config ->
  TestStream ->
  Either TestError (TestStream, a)
runTestApp app c s =
  let parseT = unTestApp app
      inputT = runTeXParseTEmbedded parseT s
      exceptT = unFailingInputT inputT
      stateT = runExceptT exceptT
   in runIdentity $ evalStateT stateT c

assertResultsEqual :: (Eq a, Show a) => [Lex.Token] -> TestApp a -> a -> Assertion
assertResultsEqual inp parser expected = do
  loggerSet <- Log.newStdoutLoggerSet Log.defaultBufSize
  let conf = Conf.pureConfig loggerSet
  let stream = TestStream inp
  case runTestApp parser conf stream of
    Left e -> assertFailure $ show e
    Right (TestStream remain, res) -> do
      assertEqual "Result match" expected res
      assertEqual "No tokens remain in output" [] remain

testEmpty :: TestTree
testEmpty =
  testCase "No input maps to no output primitive-tokens" $ do
    assertResultsEqual [] (PC.many P.anySingle) []

testRelax :: TestTree
testRelax =
  testCase "'\\relax' maps to one 'Relax' token" $ do
    assertResultsEqual [csT "relax"] (PC.many P.anySingle) [RelaxTok]

testLetter :: TestTree
testLetter =
  testCase "'a' letter maps to one character token" $ do
    let letterCC = cc 'a' Letter
    assertResultsEqual [letterCC] (PC.many P.anySingle) [UnresolvedTok letterCC]

testCSName :: TestTree
testCSName =
  testCase "\\csname works" $ do
    let inp = [csT "csname"] <> letterWord "relax" <> [csT "endcsname"]
    assertResultsEqual inp (PC.many P.anySingle) [RelaxTok]

testString :: TestTree
testString =
  testCase "\\string works" $ do
    let inp = [csT "string", csT "relax"]
    let expected =
          [ oCC '\\' Other,
            oCC 'r' Other,
            oCC 'e' Other,
            oCC 'l' Other,
            oCC 'a' Other,
            oCC 'x' Other
          ]
    assertResultsEqual inp (PC.many P.anySingle) expected

testChangeCase :: TestTree
testChangeCase =
  testCase "Change-case works" $ do
    let inpU = [csT "uppercase", cc '{' BeginGroup] <> letterWord "foo" <> [cc '}' EndGroup]
    assertResultsEqual inpU (PC.many P.anySingle) (oLetterWord "FOO")

    let inpL = [csT "lowercase", cc '{' BeginGroup] <> letterWord "FOO" <> [cc '}' EndGroup]
    assertResultsEqual inpL (PC.many P.anySingle) (oLetterWord "foo")

-- Assignments.

testMacroDef :: TestTree
testMacroDef =
  testCase "Macro definition works" $ do
    let replToks = letterWord "foo"

        macroCS = cs "a"

        inp =
          [ csT "def",
            ControlSequenceToken macroCS,
            cc '{' BeginGroup
          ]
            <> replToks
            <> [ cc '}' EndGroup
               ]

        macroConts =
          MacroContents
            { preParamTokens = mempty,
              parameters = Map.empty,
              long = False,
              outer = False,
              replacementTokens = MacroText (Seq.fromList (MacroTextLexToken <$> replToks))
            }

        assignBody = ControlSequenceDefinition (ControlSequenceProper macroCS) (MacroTarget macroConts)

        assign = Assignment {body = DefineControlSequence assignBody, scope = Local}

    assertResultsEqual
      inp
      parseAssignment
      assign

    assertResultsEqual
      ([csT "global"] <> inp)
      parseAssignment
      (assign & typed @ScopeFlag .~ Global)
