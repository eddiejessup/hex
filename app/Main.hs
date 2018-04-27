module Main where

import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as C
import qualified Cat


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

commonCatCodes = Cat.Comment:Cat.Escape:tokeniseCatCodes

data LexToken = CharCat Cat.CharCat | ControlSequenceCall {name :: String, len :: Int } deriving (Show)

collectUntil :: (v -> Bool) -> (a -> (v, a)) -> a -> ([v], a)
collectUntil tester processor state =
    let (vNew, stateNew) = processor state
    in
        if tester vNew
        then
            ([], stateNew)
        else
            let (vRest, stateEnd) = collectUntil tester processor stateNew
            in
                (vNew:vRest, stateEnd)

extractLexTokenCommon :: (Char -> Cat.CatCode) -> [Char] -> [LexToken]
extractLexTokenCommon toCat chars
    | cat == Cat.Escape =
        let
            (ccNameFirst, remainCharsFirst) = extract remainChars
            catNameFirst = Cat.cat ccNameFirst
            nextFn =
                if catNameFirst `elem` [Cat.Space, Cat.Letter]
                    then extractLexTokenSkippingBlanks
                    else extractLexTokenLineMiddle
            (ccNameRest, nextChars) =
                if catNameFirst == Cat.Letter
                    then collectUntil (\cc -> Cat.cat cc /= Cat.Letter) extract remainCharsFirst
                    else ([], remainCharsFirst)
            csCCs = ccNameFirst:ccNameRest
            csName = map Cat.char csCCs
            tokLength = sum $ map Cat.len (cc:csCCs)
            tok = ControlSequenceCall {name=csName, len=tokLength}
        in tok : nextFn toCat nextChars
    | cat `elem` tokeniseCatCodes =
        (CharCat cc):extractLexTokenLineMiddle toCat remainChars
    | cat == Cat.Comment =
        let (_, nextChars) = collectUntil (\cc -> Cat.cat cc == Cat.EndOfLine) extract remainChars
        in extractLexTokenLineBegin toCat nextChars
    where
        extract = Cat.extractCharCatTrio toCat
        (cc, remainChars) = extract chars
        cat = Cat.cat cc

extractLexTokenLineBegin :: (Char -> Cat.CatCode) -> [Char] -> [LexToken]
extractLexTokenLineBegin _ [] = []
extractLexTokenLineBegin toCat chars
    | cat `elem` commonCatCodes = extractLexTokenCommon toCat chars
    | cat == Cat.Space = extractLexTokenLineBegin toCat remainChars
    | cat == Cat.EndOfLine =
        let tok = ControlSequenceCall {name="par", len=Cat.len cc}
        in tok:extractLexTokenLineBegin toCat remainChars
    where
        extract = Cat.extractCharCatTrio toCat
        (cc, remainChars) = extract chars
        cat = Cat.cat cc

extractLexTokenLineMiddle :: (Char -> Cat.CatCode) -> [Char] -> [LexToken]
extractLexTokenLineMiddle _ [] = []
extractLexTokenLineMiddle toCat chars
    | cat `elem` commonCatCodes = extractLexTokenCommon toCat chars
    | cat `elem` [Cat.Space, Cat.EndOfLine] =
        let
            nextFn = if cat == Cat.Space
                then extractLexTokenSkippingBlanks
                else extractLexTokenLineBegin
            tok = CharCat Cat.CharCat {Cat.char=' ', Cat.cat=Cat.Space, Cat.len=Cat.len cc}
        in tok:nextFn toCat remainChars
    where
        extract = Cat.extractCharCatTrio toCat
        (cc, remainChars) = extract chars
        cat = Cat.cat cc

extractLexTokenSkippingBlanks :: (Char -> Cat.CatCode) -> [Char] -> [LexToken]
extractLexTokenSkippingBlanks _ [] = []
extractLexTokenSkippingBlanks toCat chars
    | cat `elem` commonCatCodes = extractLexTokenCommon toCat chars
    | cat `elem` [Cat.Space, Cat.EndOfLine] = extractLexTokenSkippingBlanks toCat remainChars
    where
        extract = Cat.extractCharCatTrio toCat
        (cc, remainChars) = extract chars
        cat = Cat.cat cc

main = do
    let chrs = map C.chr [0..127]
    let chrCats = map (\chr -> (chr, Cat.defaultCatCode chr)) chrs
    let defMap = Map.fromList chrCats

    -- Add in some useful extras beyond the technical defaults.
    let extras = [('^', Cat.Superscript)]
    let usableMap = foldl (\m (k, v) -> Map.insert k v m) defMap extras

    let a:b:bs = chrs

    let toCat = Cat.toCatCode usableMap
    -- print $ Cat.process toCat ['a', 'b', 'c', '^']
    -- print $ Cat.process toCat ['a', 'b', 'c', '^', '^']
    -- print $ Cat.process toCat ['a', 'b', 'c', '^', '^', 'z', 't']

    contents <- readFile "test.tex"
    putStrLn $ List.intercalate "\n" $ map show $ extractLexTokenLineBegin toCat contents
    -- print $ process toCat ['a', 'b', 'c', '^', '^', 'z', 't']    
