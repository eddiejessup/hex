{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Paragraph where

import qualified Data.Char as C

import qualified Cat
import qualified Lex

data DesiredLength = Natural | Spread Int | To Int
                   deriving (Show)

data VList = VList { contents :: [VListElement]
                   , desiredLength :: DesiredLength } deriving (Show)

data HList = HList { contents :: [HListElement]
                   , desiredLength :: DesiredLength } deriving (Show)

data Rule = Rule { width :: Int 
                 , height :: Int
                 , depth :: Int } deriving (Show)
data Kern = Kern { dimen :: Int } deriving (Show)
data FontDefinition = FontDefinition { fontNr :: Int
                                     , fontPath :: FilePath
                                     , fontName :: FilePath
                                     , scaleFactorRatio :: Double } deriving (Show)
data FontSelection = FontSelection { fontNr :: Int } deriving (Show)
data Character = Character { code :: Int } deriving (Show)
              -- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

-- Can't have these things in a box, only a list.

data Glue = Glue { dimen :: Int, stretch :: Int, shrink :: Int } deriving (Show)
data Penalty = Penalty { size :: Int } deriving (Show)

             -- TODO: WhatsIt, Leaders, Mark, Insertion
              -- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

data HListElement = HVList VList
              | HHList HList
              | HRule Rule
              | HGlue Glue
              | HKern Kern
              | HPenalty Penalty
              | HFontDefinition FontDefinition
              | HFontSelection FontSelection
              | HCharacter Character
              deriving (Show)

data VListElement = VVList VList
              | VHList HList
              | VRule Rule
              | VGlue Glue
              | VKern Kern
              | VPenalty Penalty
              | VFontDefinition FontDefinition
              | VFontSelection FontSelection
              deriving (Show)

extractHElems :: [Lex.LexToken] -> ([HListElement], [Lex.LexToken])
extractHElems [] = ([], [])
extractHElems (Lex.CharCat Cat.CharCat{char=char, cat=cat}:rest)
    | cat `elem` [Cat.Letter, Cat.Other] = (HCharacter Character{code=C.ord char}:nextResult, nextRemain)
    | cat == Cat.Space = (HGlue Glue{dimen=300000, stretch=0, shrink=0}:nextResult, nextRemain)
    | otherwise = nextRet
    where
        nextRet = extractHElems rest
        (nextResult, nextRemain) = nextRet
extractHElems (Lex.ControlSequenceCall{name=name}:rest)
    | name == "font" =
        let
            fontDef = HFontDefinition FontDefinition{fontNr=1, fontPath="cmr10.tfm", fontName="cmr10", scaleFactorRatio=1.0}
            fontSel = HFontSelection FontSelection{fontNr=1}
        in
            (fontDef:fontSel:nextResult, nextRemain)
    | name == "par" = ([], rest)
    | otherwise = nextRet
    where
        nextRet = extractHElems rest
        (nextResult, nextRemain) = nextRet

extractVElems :: [Lex.LexToken] -> ([VListElement], [Lex.LexToken])
extractVElems [] = ([], [])
extractVElems (a@Lex.CharCat{}:rest) =
    let
        (hConts, hRemain) = extractHElems (a:rest)
        hList = VHList HList{contents=hConts, desiredLength=Natural}
        down = VGlue Glue{dimen=1000000, stretch=0, shrink=0}
        (nextResult, vRemain) = extractVElems hRemain
    in
        (hList:down:nextResult, vRemain)
extractVElems (Lex.ControlSequenceCall{name=name}:rest)
    | name == "font" =
        let
            fontDef = VFontDefinition FontDefinition{fontNr=1, fontPath="cmr10.tfm", fontName="cmr10", scaleFactorRatio=1.0}
            fontSel = VFontSelection FontSelection{fontNr=1}
        in
            (fontDef:fontSel:nextResult, nextRemain)
    | name == "par" = extractVElems rest
    | otherwise = nextRet
    where
        nextRet = extractVElems rest
        (nextResult, nextRemain) = nextRet
