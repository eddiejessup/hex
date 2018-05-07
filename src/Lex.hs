{-# LANGUAGE DuplicateRecordFields #-}

module Lex where

import qualified Cat

data LexToken = CharCat Cat.CharCat
              | ControlSequenceCall {name :: String, len :: Int }
              deriving (Show)

data LexState = SkippingBlanks | LineMiddle | LineBegin deriving (Eq)

tokeniseCatCodes =
    [ Cat.BeginGroup
    , Cat.EndGroup
    , Cat.MathShift
    , Cat.AlignTab
    , Cat.Parameter
    , Cat.Superscript
    , Cat.Subscript
    , Cat.Letter
    , Cat.Other
    , Cat.Active ]

collectUntil :: (a -> Bool) -> [a] -> ([a], [a])
collectUntil tester (this:rest) =
    if tester this
    then
        ([], rest)
    else
        let (restBefore, after) = collectUntil tester rest
        in
            (this:restBefore, after)

extractLexTokens :: [Cat.CharCat] -> LexState -> [LexToken]
extractLexTokens [] _ = []
extractLexTokens (this@Cat.CharCat{cat=cat, len=len}:rest) state
    | cat == Cat.Escape =
        let
            ccNameFirst:remainCharCatsFirst = rest
            catNameFirst = Cat.cat ccNameFirst
            nextState =
                if catNameFirst `elem` [Cat.Space, Cat.Letter]
                    then SkippingBlanks
                    else LineMiddle
            (ccNameRest, nextCharCats) =
                if catNameFirst == Cat.Letter
                    then collectUntil ((Cat.Letter /=) . Cat.cat) remainCharCatsFirst
                    else ([], remainCharCatsFirst)
            csCCs = ccNameFirst:ccNameRest
            csName = map Cat.char csCCs
            tokLength = sum $ map Cat.len (this:csCCs)
            tok = ControlSequenceCall {name=csName, len=tokLength}
        in
            tok:extractLexTokens nextCharCats nextState
    | cat `elem` tokeniseCatCodes =
        (CharCat this):extractRest LineMiddle
    | cat == Cat.Comment =
        let
            (_, nextCharCats) = collectUntil ((Cat.EndOfLine ==) . Cat.cat) rest
        in
            extractLexTokens nextCharCats LineBegin
    | (state == SkippingBlanks) && (cat `elem` [Cat.Space, Cat.EndOfLine]) =
        extractRest SkippingBlanks
    | (state == LineBegin) && (cat == Cat.Space) =
        extractRest LineBegin
    -- Empty line: Make a paragraph.
    | (state == LineBegin) && (cat == Cat.EndOfLine) =
        let
            tok = ControlSequenceCall {name="par", len=Cat.len this}
        in
            tok:extractRest LineBegin
    | (state == LineMiddle) && (cat `elem` [Cat.Space, Cat.EndOfLine]) =
        let
            nextState = if cat == Cat.Space then SkippingBlanks else LineBegin
            tok = CharCat Cat.CharCat {Cat.char=' ', Cat.cat=Cat.Space, Cat.len=Cat.len this}
        in tok:extractRest nextState
    where
        extractRest = extractLexTokens rest

process charCats = extractLexTokens charCats LineBegin
