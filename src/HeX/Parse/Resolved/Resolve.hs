module HeX.Parse.Resolved.Resolve where

import qualified Data.HashMap.Strict as HMap

import qualified HeX.Lex as Lex

import HeX.Parse.Lexed
import HeX.Parse.Resolved.Token

theFontNr :: Int
theFontNr = 1

type CSMap = HMap.HashMap Lex.ControlSequenceLike ResolvedToken

defaultCSMap :: CSMap
defaultCSMap =
  HMap.fromList
    [ ( Lex.ControlSequenceProper (Lex.ControlWord "relax")
      , PrimitiveToken Relax)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "ignorespaces")
      , PrimitiveToken IgnoreSpaces)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "uppercase")
      , SyntaxCommandHead $ ChangeCaseToken Upward)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "lowercase")
      , SyntaxCommandHead $ ChangeCaseToken Downward)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "penalty")
      , PrimitiveToken AddPenalty)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "kern")
      , PrimitiveToken AddKern)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "vskip")
      , PrimitiveToken $ ModedCommand Vertical AddSpecifiedGlue)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "hskip")
      , PrimitiveToken $ ModedCommand Horizontal AddSpecifiedGlue)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "hfil")
      , PrimitiveToken $ ModedCommand Horizontal $ AddPresetGlue Fil)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "vfil")
      , PrimitiveToken $ ModedCommand Vertical $ AddPresetGlue Fil)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "hfill")
      , PrimitiveToken $ ModedCommand Horizontal $ AddPresetGlue Fill)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "vfill")
      , PrimitiveToken $ ModedCommand Vertical $ AddPresetGlue Fill)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "hfilneg")
      , PrimitiveToken $ ModedCommand Horizontal $ AddPresetGlue FilNeg)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "vfilneg")
      , PrimitiveToken $ ModedCommand Vertical $ AddPresetGlue FilNeg)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "hss")
      , PrimitiveToken $ ModedCommand Horizontal $ AddPresetGlue StretchOrShrink)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "vss")
      , PrimitiveToken $ ModedCommand Vertical $ AddPresetGlue StretchOrShrink)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "indent")
      , PrimitiveToken $ StartParagraph {indent = True})
    , ( Lex.ControlSequenceProper (Lex.ControlWord "noindent")
      , PrimitiveToken $ StartParagraph {indent = False})
    , ( Lex.ControlSequenceProper (Lex.ControlWord "par")
      , PrimitiveToken EndParagraph)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "hrule")
      , PrimitiveToken $ ModedCommand Vertical AddRule)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "vrule")
      , PrimitiveToken $ ModedCommand Horizontal AddRule)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "font")
      , PrimitiveToken MacroToFont)
    -- Temporary pragmatism.
    , ( Lex.ControlSequenceProper (Lex.ControlWord "selectfont")
      , PrimitiveToken $ TokenForFont theFontNr)
    , (Lex.ControlSequenceProper (Lex.ControlWord "end"), PrimitiveToken End)
    -- Macro prefixes.
    , ( Lex.ControlSequenceProper (Lex.ControlWord "global")
      , PrimitiveToken Global)
    , (Lex.ControlSequenceProper (Lex.ControlWord "long"), PrimitiveToken Long)
    , ( Lex.ControlSequenceProper (Lex.ControlWord "outer")
      , PrimitiveToken Outer)
    -- Macro def types.
    , ( Lex.ControlSequenceProper (Lex.ControlWord "def")
      , PrimitiveToken $ DefineMacro {global = False, expand = False})
    , ( Lex.ControlSequenceProper (Lex.ControlWord "edef")
      , PrimitiveToken $ DefineMacro {global = False, expand = True})
    , ( Lex.ControlSequenceProper (Lex.ControlWord "gdef")
      , PrimitiveToken $ DefineMacro {global = True, expand = False})
    , ( Lex.ControlSequenceProper (Lex.ControlWord "xdef")
      , PrimitiveToken $ DefineMacro {global = True, expand = True})
    , ( Lex.ControlSequenceProper (Lex.ControlWord "amacro")
      , SyntaxCommandHead $
        MacroToken $
        Macro [] $
        BalancedText
          [ Lex.ControlSequence $ Lex.ControlWord "uppercase"
          , Lex.CharCatToken $ Lex.CharCat 123 Lex.BeginGroup
          , Lex.CharCatToken $ Lex.CharCat 99 Lex.Letter
          ])
    ]
