{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Harden where

import qualified Lex
import qualified Cat

data ParseToken
  = StartParagraph { indent :: Bool }
  | EndParagraph
  | Space
  | ExplicitCharacter Int
  | Relax
  | TempDFont
  | TempSFont
  deriving Show

extractTokenInner :: Cat.CharCatMap -> Lex.LexState -> Lex.Token -> [Cat.CharCode] -> Maybe (ParseToken, Lex.LexState, [Cat.CharCode])
extractTokenInner ccMap lexState tok1 cs
  | Lex.CharCat{cat=Lex.Space} <- tok1 =
    Just (Space, lexState, cs)
  | Lex.CharCat{char=char, cat=Lex.Letter} <- tok1 =
    Just (ExplicitCharacter char, lexState, cs)
  | Lex.CharCat{char=char, cat=Lex.Other} <- tok1 =
    Just (ExplicitCharacter char, lexState, cs)
  | Lex.ControlSequence (Lex.ControlWord "par") <- tok1 =
    Just (EndParagraph, lexState, cs)
  | Lex.ControlSequence (Lex.ControlWord "indent") <- tok1 =
    Just (StartParagraph{indent=True}, lexState, cs)
  | Lex.ControlSequence (Lex.ControlWord "noindent") <- tok1 =
    Just (StartParagraph{indent=False}, lexState, cs)
  | Lex.ControlSequence (Lex.ControlWord "relax") <- tok1 =
    Just (Relax, lexState, cs)
  -- Temporary pragmatism.
  | Lex.ControlSequence (Lex.ControlWord "dfont") <- tok1 =
    Just (TempDFont, lexState, cs)
  | Lex.ControlSequence (Lex.ControlWord "sfont") <- tok1 =
    Just (TempSFont, lexState, cs)
  | otherwise
     = error $ "Unknown lex token: " ++ show tok1

extractToken :: Cat.CharCatMap -> Lex.LexState -> [Cat.CharCode] -> Maybe (ParseToken, Lex.LexState, [Cat.CharCode])
extractToken _ _ [] = Nothing
extractToken ccMap lexState0 cs = do
  (tok1, lexState1, rest) <- Lex.extractToken ccMap lexState0 cs
  extractTokenInner ccMap lexState1 tok1 rest
