module HeX.Parse.Resolved.SyntaxCommand where

import qualified Text.Megaparsec as P

import HeX.Parse.Helpers

import HeX.Parse.Lexed

import HeX.Parse.Resolved.Stream
import HeX.Parse.Resolved.Token

data SyntaxCommand
  = ChangeCase VDirection
               BalancedText
  | ExpandMacro Macro
  | PassPrimitiveToken PrimitiveToken

parseSyntaxCommand :: SimpResolveParser SyntaxCommand
parseSyntaxCommand = P.choice [parsePrimitiveToken]

parsePrimitiveToken :: SimpResolveParser SyntaxCommand
parsePrimitiveToken = satisfyThen tokToPrim
  where
    tokToPrim (PrimitiveToken p) = Just $ PassPrimitiveToken p
    tokToPrim _ = Nothing

-- parseChangeCase :: SimpResolveParser SyntaxCommand
-- parseChangeCase = ChangeCase <$> satisfyThen tokToDirection <*> parseGeneralText
--   where
--     tokToDirection (ChangeCaseToken d) = Just d
--     tokToDirection _ = Nothing
