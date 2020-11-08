module Hex.Test.Lex where

import Data.Conduit
import Data.Conduit.Combinators
import Hex.Categorise
import Hex.Config.Codes
import Hex.Lex
import Protolude hiding (yield)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lex"
    [ testSimple,
      testControlWord,
      testControlSymbol,
      testControlSymbolThenLetters,
      testTrailingEscape,
      testManySpaces,
      testComments
    ]

ccOrd :: Char -> CharCode
ccOrd = CharCode . wordOrd

wordOrd :: Char -> Word8
wordOrd = fromIntegral . ord

inCC :: Char -> CatCode -> RawCharCat
inCC c = RawCharCat (ccOrd c)

outCC :: Char -> CoreCatCode -> Token
outCC c cat = CharCatToken $ CharCat (ccOrd c) cat

run :: LexState -> [RawCharCat] -> Either LexError [Token]
run lexState inp = runIdentity `first` evalState (runExceptT (runCond inp)) lexState
  where
    runCond :: [RawCharCat] -> ExceptT (Identity LexError) (StateT LexState Identity) [Token]
    runCond s =
      runConduit $
        yieldMany s
          .| extractToken
          .| sinkList

assertResultsEqual :: Maybe LexState -> [RawCharCat] -> Either LexError [Token] -> Assertion
assertResultsEqual mayLexState inp expected = do
  let testStates = case mayLexState of
        Nothing -> [LineBegin, LineMiddle, SkippingBlanks]
        Just lexState -> [lexState]
  for_ testStates $ \lexState -> do
    let res = run lexState inp
    assertEqual
      ("With initial state '" <> show lexState <> "', Results match")
      expected
      res

testSimple :: TestTree
testSimple =
  testCase "One letter maps to one letter" $
    assertResultsEqual
      Nothing
      [ inCC 'a' (CoreCatCode Letter)
      ]

      (Right
        [ outCC 'a' Letter
        ])

testControlWord :: TestTree
testControlWord =
  testCase "'\\aa' maps to one control sequence" $
    assertResultsEqual
      Nothing
      [ inCC '\\' Escape,
        inCC 'a' (CoreCatCode Letter),
        inCC 'a' (CoreCatCode Letter)
      ]
      (Right
        [ ControlSequenceToken $ ControlSequence "aa"
        ])

testControlSymbol :: TestTree
testControlSymbol =
  testCase "'\\.' maps to one control sequence" $
    assertResultsEqual
      Nothing
      [ inCC '\\' Escape,
        inCC '.' (CoreCatCode Other)
      ]
      (Right
        [ ControlSequenceToken $ ControlSequence "."
        ])

testControlSymbolThenLetters :: TestTree
testControlSymbolThenLetters =
  testCase "'\\.hi' maps to one control sequence, then some letters" $
    assertResultsEqual
      Nothing
      [ inCC '\\' Escape,
        inCC '.' (CoreCatCode Other),
        inCC 'h' (CoreCatCode Letter),
        inCC 'i' (CoreCatCode Letter)
      ]
      (Right
        [ ControlSequenceToken $ ControlSequence ".",
          outCC 'h' Letter,
          outCC 'i' Letter
        ])

testTrailingEscape :: TestTree
testTrailingEscape =
  testCase "Trailing escape-character causes error" $ do
    let inp =
          [ inCC 'h' (CoreCatCode Letter),
            inCC '\\' Escape
          ]

    assertResultsEqual
      Nothing
      inp
      (Left TrailingEscape)


testManySpaces :: TestTree
testManySpaces =
  testCase "Multiple spaces map to a single space" $ do
    assertResultsEqual
      (Just LineBegin)
      [ inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space)
      ]
      (Right [])

    assertResultsEqual
      (Just SkippingBlanks)
      [ inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space)
      ]
      (Right [])

    assertResultsEqual
      (Just LineMiddle)
      [ inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space),
        inCC ' ' (CoreCatCode Space)
      ]
      (Right
        [ outCC ' ' Space
        ])

testComments :: TestTree
testComments =
  testCase "Comments are ignored" $ do
    assertResultsEqual
      Nothing
      [ inCC '#' Comment,
        inCC 'a' (CoreCatCode Letter),
        inCC '\n' EndOfLine,
        inCC 'b' (CoreCatCode Letter)
      ]
      (Right
        [ parToken,
          outCC 'b' Letter
        ])
