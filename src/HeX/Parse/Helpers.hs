{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Helpers where

import           Data.Functor                   ( ($>) )
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import qualified Data.Set                      as Set
import qualified Text.Megaparsec               as P

-- Helpers.
type SimpParser s = P.Parsec () s

type ParseError s = (P.ParseError (P.Token s) ())

type MatchToken s = P.Token s -> Bool

type NullSimpParser s = SimpParser s ()

easyRunParser :: SimpParser s a -> s -> (P.State s, Either (ParseError s) a)
easyRunParser p s = P.runParser' p (freshState s)
  where
    freshState stream =
      let pos = P.SourcePos "" (P.mkPos 1) (P.mkPos 1)
      in P.State stream (pos :| []) 0 (P.mkPos 1)

satisfy :: P.Stream s => MatchToken s -> SimpParser s (P.Token s)
satisfy f = P.token testTok Nothing
  where
    testTok x =
      if f x
        then Right x
        else Left (Just (P.Tokens (x :| [])), Set.empty)

getToken :: P.Stream s => SimpParser s (P.Token s)
getToken = P.token Right Nothing

manySatisfied :: P.Stream s => MatchToken s -> SimpParser s [P.Token s]
manySatisfied testTok = P.many $ satisfy testTok

satisfyThen :: P.Stream s => (P.Token s -> Maybe a) -> SimpParser s a
satisfyThen f = P.token testTok Nothing
  where
    testTok x =
      case f x of
        Just y -> Right y
        Nothing -> Left (Just (P.Tokens (x :| [])), Set.empty)

-- Skipping.

skipSatisfied :: P.Stream s => MatchToken s -> NullSimpParser s
skipSatisfied f = P.token testTok Nothing
  where
    testTok x =
      if f x
        then Right ()
        else Left (Just (P.Tokens (x :| [])), Set.empty)

skipSatisfiedEquals :: P.Stream s => P.Token s -> NullSimpParser s
skipSatisfiedEquals t = skipSatisfied (== t)

skipOptional :: P.Stream s => SimpParser s a -> NullSimpParser s
skipOptional p = P.optional p $> ()

skipOneOptionalSatisfied :: P.Stream s => MatchToken s -> NullSimpParser s
skipOneOptionalSatisfied = skipOptional . skipSatisfied

skipManySatisfied :: P.Stream s => MatchToken s -> NullSimpParser s
skipManySatisfied = P.skipMany . skipSatisfied

skipSatisfiedChunk :: P.Stream s => [P.Token s] -> NullSimpParser s
skipSatisfiedChunk [] = pure ()
skipSatisfiedChunk (x:xs) = do
  skipSatisfiedEquals x
  skipSatisfiedChunk xs
