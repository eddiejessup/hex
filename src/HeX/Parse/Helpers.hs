{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module HeX.Parse.Helpers where

import           Data.Functor                   ( ($>) )
import qualified Data.Set                      as Set
import qualified Text.Megaparsec               as P

-- Helpers.
type SimpParser s = P.Parsec () s

type ParseErrorBundle s = P.ParseErrorBundle s ()

type MatchToken s = P.Token s -> Bool

type NullSimpParser s = SimpParser s ()

freshSourcePos :: P.SourcePos
freshSourcePos = P.initialPos "elmo_source"

freshPosState :: s -> P.PosState s
freshPosState stream = P.PosState { pstateInput = stream
                                  , pstateOffset = 0
                                  , pstateSourcePos = freshSourcePos
                                  , pstateTabWidth = P.mkPos 4
                                  , pstateLinePrefix = ">!> " }

freshState :: s -> P.State s
freshState stream = P.State { stateInput = stream
                            , stateOffset = 0
                            , statePosState = freshPosState stream }

easyRunParser :: SimpParser s a -> s -> (P.State s, Either (ParseErrorBundle s) a)
easyRunParser p s = P.runParser' p (freshState s)

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True x = Just x
boolToMaybe False _ = Nothing

predToMaybe :: (a -> Bool) -> a -> Maybe a
predToMaybe f x = boolToMaybe (f x) x

manySatisfied :: P.Stream s => MatchToken s -> SimpParser s [P.Token s]
manySatisfied testTok = P.many $ P.satisfy testTok

satisfyThen :: P.Stream s => (P.Token s -> Maybe a) -> SimpParser s a
satisfyThen f = P.token f Set.empty

-- Skipping.

skipSatisfied :: P.Stream s => MatchToken s -> NullSimpParser s
skipSatisfied f = satisfyThen testTok
  where
    testTok x = if f x then Just () else Nothing

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
