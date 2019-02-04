{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Box.Elem
  ( FontDefinition(..)
  , FontSelection(..)
  , Rule(..)
  , Character(..)
  , module HeX.Box.Elem
  )
where

import           Safe.Foldable                  ( maximumDef )

import           HeXPrelude
import           HeX.Type
import           HeX.Concept
import qualified HeX.Unit                      as Unit

import           DVI.Document                   ( FontDefinition(..)
                                                , FontSelection(..)
                                                , Rule(..)
                                                , Character(..)
                                                )

data DesiredLength
    = Natural
    | Spread LenVal
    | To LenVal
    deriving (Show)

newtype Kern = Kern { kernDimen :: LenVal }
    deriving (Show)

newtype SetGlue = SetGlue { glueDimen :: LenVal }
    deriving (Show)

instance Readable SetGlue where
    describe SetGlue {..} = "[" ++ Unit.showSP glueDimen ++ "]"

data BoxContents
    = HBoxContents [HBoxElem]
    | VBoxContents [VBoxElem]
    deriving (Show)

data Box = Box
    { contents :: BoxContents
    , desiredLength :: DesiredLength
    } deriving (Show)

instance Dimensioned BoxContents where
    naturalLength dim b = case (dim, b) of
        (Width,  (HBoxContents cs)) -> sumLength cs
        (_,      (HBoxContents cs)) -> maxLength cs
        (Width,  (VBoxContents cs)) -> maxLength cs
        (Height, (VBoxContents cs)) -> sumLength cs
        (Depth,  (VBoxContents _)) -> error "TODO"
      where
        subLengths cs = naturalLength dim <$> cs
        maxLength cs = maximumDef 0 $ subLengths cs
        sumLength cs = sum $ subLengths cs

instance Dimensioned Box where
    naturalLength dim b = case (dim, b) of
        (Width,  Box (HBoxContents _) (To to)) -> to
        (Height, Box (VBoxContents _) (To to)) -> to

        (Width,  Box bc@(HBoxContents _) (Spread spread)) -> spread + (naturalLength Width bc)
        (Height, Box bc@(VBoxContents _) (Spread spread)) -> spread + (naturalLength Height bc)

        -- TODO: Look up and implement specification.
        (Depth,  Box (VBoxContents _) _) -> error "Not implemented: Depth of VBox"

        (_,      Box bc _) -> naturalLength dim bc

data BaseElem
    = ElemBox Box
    | ElemRule Rule
    | ElemFontDefinition FontDefinition
    | ElemFontSelection FontSelection
    | ElemKern Kern
    deriving (Show)

spacerNaturalLength :: Axis -> TypoDim -> Int -> Int
spacerNaturalLength ax dim d = case (ax, dim) of
    (Vertical,  Height) -> d
    (Horizontal, Width) -> d
    _                   -> 0

axisBaseElemNaturalLength :: Axis -> TypoDim -> BaseElem -> Int
axisBaseElemNaturalLength ax dim e = case e of
    ElemFontSelection _  -> 0
    ElemFontDefinition _ -> 0
    ElemBox r            -> naturalLength dim r
    ElemRule r           -> naturalLength dim r
    ElemKern k           -> spacerNaturalLength ax dim $ kernDimen k

data VBoxElem
    = VBoxBaseElem BaseElem
    | BoxGlue SetGlue
    deriving (Show)

axisVBoxElemNaturalLength :: Axis -> TypoDim -> VBoxElem -> Int
axisVBoxElemNaturalLength ax dim e = case e of
    VBoxBaseElem be -> axisBaseElemNaturalLength ax dim be
    BoxGlue g       -> spacerNaturalLength ax dim $ glueDimen g

instance Dimensioned VBoxElem where
    naturalLength = axisVBoxElemNaturalLength Vertical

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HBaseElem
    = ElemCharacter Character
    deriving (Show)

instance Dimensioned HBaseElem where
    naturalLength dim (ElemCharacter c) = naturalLength dim c

data HBoxElem
    = HVBoxElem VBoxElem
    | HBoxHBaseElem HBaseElem
    deriving (Show)

instance Dimensioned HBoxElem where
    naturalLength dim (HVBoxElem e) = axisVBoxElemNaturalLength Horizontal dim e
    naturalLength dim (HBoxHBaseElem e) = naturalLength dim e

newtype Page = Page [VBoxElem]
    deriving (Show)
