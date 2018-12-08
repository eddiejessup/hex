module HeX.Parse.Resolved.Resolve where

import qualified Data.HashMap.Strict           as HMap
import qualified Data.Map.Strict               as Map

import qualified HeX.Lex                       as Lex

import           HeX.Parse.Lexed
import           HeX.Parse.Resolved.Token

theFontNr :: Int
theFontNr = 1

type CSMap = HMap.HashMap Lex.ControlSequenceLike ResolvedToken

defaultCSMap :: CSMap
defaultCSMap =
  HMap.fromList
    [ ( Lex.ControlSequenceProper (Lex.ControlSequence "relax")
      , PrimitiveToken Relax)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "ignorespaces")
      , PrimitiveToken IgnoreSpaces)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "uppercase")
      , SyntaxCommandHead $ ChangeCaseToken Upward)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "lowercase")
      , SyntaxCommandHead $ ChangeCaseToken Downward)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "penalty")
      , PrimitiveToken AddPenalty)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "kern")
      , PrimitiveToken AddKern)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "vskip")
      , PrimitiveToken $ ModedCommand Vertical AddSpecifiedGlue)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "hskip")
      , PrimitiveToken $ ModedCommand Horizontal AddSpecifiedGlue)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "hfil")
      , PrimitiveToken $ ModedCommand Horizontal $ AddPresetGlue Fil)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "vfil")
      , PrimitiveToken $ ModedCommand Vertical $ AddPresetGlue Fil)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "hfill")
      , PrimitiveToken $ ModedCommand Horizontal $ AddPresetGlue Fill)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "vfill")
      , PrimitiveToken $ ModedCommand Vertical $ AddPresetGlue Fill)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "hfilneg")
      , PrimitiveToken $ ModedCommand Horizontal $ AddPresetGlue FilNeg)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "vfilneg")
      , PrimitiveToken $ ModedCommand Vertical $ AddPresetGlue FilNeg)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "hss")
      , PrimitiveToken $ ModedCommand Horizontal $ AddPresetGlue StretchOrShrink)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "vss")
      , PrimitiveToken $ ModedCommand Vertical $ AddPresetGlue StretchOrShrink)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "indent")
      , PrimitiveToken $ StartParagraph {indent = True})
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "noindent")
      , PrimitiveToken $ StartParagraph {indent = False})
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "par")
      , PrimitiveToken EndParagraph)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "hrule")
      , PrimitiveToken $ ModedCommand Vertical AddRule)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "vrule")
      , PrimitiveToken $ ModedCommand Horizontal AddRule)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "font")
      , PrimitiveToken MacroToFont)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "csname")
      , SyntaxCommandHead CSName)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "endcsname")
      , PrimitiveToken $ SyntaxCommandArg EndCSName)
    -- Temporary pragmatism.
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "selectfont")
      , PrimitiveToken $ TokenForFont theFontNr)
    , (Lex.ControlSequenceProper (Lex.ControlSequence "end"), PrimitiveToken End)
    -- Macro prefixes.
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "global")
      , PrimitiveToken Global)
    , (Lex.ControlSequenceProper (Lex.ControlSequence "long"), PrimitiveToken Long)
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "outer")
      , PrimitiveToken Outer)
    -- Macro def types.
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "def")
      , PrimitiveToken $ DefineMacro {global = False, expand = False})
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "edef")
      , PrimitiveToken $ DefineMacro {global = False, expand = True})
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "gdef")
      , PrimitiveToken $ DefineMacro {global = True, expand = False})
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "xdef")
      , PrimitiveToken $ DefineMacro {global = True, expand = True})
    , ( Lex.ControlSequenceProper (Lex.ControlSequence "amacro")
      , SyntaxCommandHead $
        MacroToken $
        MacroContents [] Map.empty $
        MacroText
          [ MacroTextLexToken $ Lex.ControlSequenceToken $ Lex.ControlSequence "uppercase"
          , MacroTextLexToken $ Lex.CharCatToken $ Lex.CharCat '{' Lex.BeginGroup
          , MacroTextLexToken $ Lex.CharCatToken $ Lex.CharCat 'c' Lex.Letter
          ])
    ]
