module HeX.Parse.Helpers where

import HeXlude

import           Data.Functor                   ( ($>) )
import qualified Data.Set                      as Set
import qualified Text.Megaparsec               as P

type ErrorComp = ()

type SimpParser s = P.Parsec ErrorComp s

type ParseError s = P.ParseError s ErrorComp
type ParseErrorBundle s = P.ParseErrorBundle s ErrorComp

type MatchToken s = P.Token s -> Bool

type NullSimpParser s = SimpParser s ()

freshSourcePos :: P.SourcePos
freshSourcePos = P.initialPos "elmo_source"

freshPosState :: s -> P.PosState s
freshPosState stream = P.PosState
    { P.pstateInput = stream
    , P.pstateOffset = 0
    , P.pstateSourcePos = freshSourcePos
    , P.pstateTabWidth = P.mkPos 4
    , P.pstateLinePrefix = ">!> "
    }

freshState :: s -> P.State s
freshState stream = P.State
    { P.stateInput = stream
    , P.stateOffset = 0
    , P.statePosState = freshPosState stream
    }

easyRunParser :: SimpParser s a -> s -> (P.State s, Either (ParseErrorBundle s) a)
easyRunParser p s = P.runParser' p (freshState s)

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True x  = Just x
boolToMaybe False _ = Nothing

predToMaybe :: (a -> Bool) -> a -> Maybe a
predToMaybe f x = boolToMaybe (f x) x

manySatisfied :: P.Stream s => MatchToken s -> SimpParser s [P.Token s]
manySatisfied testTok = P.many $ P.satisfy testTok

satisfyThen :: P.Stream s => (P.Token s -> Maybe a) -> SimpParser s a
satisfyThen f = P.token f Set.empty

manySatisfiedThen :: P.Stream s => (P.Token s -> Maybe a) -> SimpParser s [a]
manySatisfiedThen f = P.many $ satisfyThen f

-- Skipping.

skipSatisfied :: P.Stream s => MatchToken s -> NullSimpParser s
skipSatisfied f = satisfyThen $ \x -> if f x then Just () else Nothing

skipSatisfiedEquals :: P.Stream s => P.Token s -> NullSimpParser s
skipSatisfiedEquals t = skipSatisfied (== t)

skipOptional :: P.Stream s => SimpParser s a -> NullSimpParser s
skipOptional p = P.optional p $> ()

skipOneOptionalSatisfied :: P.Stream s => MatchToken s -> NullSimpParser s
skipOneOptionalSatisfied = skipOptional . skipSatisfied

skipManySatisfied :: P.Stream s => MatchToken s -> NullSimpParser s
skipManySatisfied = P.skipMany . skipSatisfied

skipSatisfiedChunk :: P.Stream s => [P.Token s] -> NullSimpParser s
skipSatisfiedChunk []     = pure ()
skipSatisfiedChunk (x:xs) = skipSatisfiedEquals x >> skipSatisfiedChunk xs

copyState :: P.State s -> s' -> P.State s'
copyState _state tgtStream =
    let P.State { P.stateInput = _
                , P.stateOffset = offset
                , P.statePosState = posState
                } = _state
    in  P.State { P.stateInput = tgtStream
                , P.stateOffset = offset
                , P.statePosState = copyPosState posState
                }
  where
    copyPosState posState =
        let P.PosState { P.pstateInput = _
                       , P.pstateOffset = offset
                       , P.pstateSourcePos = sourcePos
                       , P.pstateTabWidth = tabWidth
                       , P.pstateLinePrefix = linePrefix
                       } = posState
        in  P.PosState { P.pstateInput = tgtStream
                       , P.pstateOffset = offset
                       , P.pstateSourcePos = sourcePos
                       , P.pstateTabWidth = tabWidth
                       , P.pstateLinePrefix = linePrefix
                       }
