module Hex.Box.Elem
  ( DVI.D.FontDefinition (..)
  , DVI.D.FontSelection (..)
  , DVI.D.Rule (..)
  , DVI.D.Character (..)
  , module Hex.Box.Elem
  )
where

import qualified DVI.Document as DVI.D
import qualified Data.Text as Text
import Hex.Config.Codes
import Hex.Quantity
import Hexlude

data DesiredLength = Natural | Spread Length | To Length
  deriving stock Show

newtype Kern = Kern {kernDimen :: Length}
  deriving stock Show
  deriving newtype ToJSON

newtype SetGlue a = SetGlue {glueDimen :: a}
  deriving stock (Show, Generic)

deriving anyclass instance ToJSON a => ToJSON (SetGlue a)

instance Describe (SetGlue Length) where

  describe SetGlue { glueDimen } =
    singleLine $ "Glue " <> quote (showSP glueDimen)

newtype HBox = HBox (Seq HBoxElem)
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass ToJSON

instance Describe HBox where

  describe (HBox elems) =
    describeNamedRelFoldable 0 "HBox" (condenseChary elems toMaybeChar)
    where
      toMaybeChar = \case
        HBoxHBaseElem (ElemCharacter DVI.D.Character {DVI.D.char}) ->
          Just $ unsafeCodeAsChar char
        _ ->
          Nothing

-- Just used to show an HList more compactly.
data Sentential a = Sentence Text | NonSentence a
  deriving stock (Show, Functor, Foldable)

instance Describe a => Describe (Sentential a) where

  describe = \case
    Sentence s -> singleLine (quote s)
    NonSentence a -> describeRel 0 a

condenseChary :: Foldable t => t a -> (a -> Maybe Char) -> Seq (Sentential a)
condenseChary elems toMaybeChar = foldl' append mempty elems
  where
    append r e = case toMaybeChar e of
      Just c -> case r of
        Empty ->
          singleton $ Sentence $ Text.singleton c
        (xs :|> Sentence cs) ->
          xs :|> Sentence (Text.snoc cs c)
        _ ->
          r :|> Sentence (Text.singleton c)
      Nothing ->
        r :|> NonSentence e

newtype VBox = VBox (Seq VBoxElem)
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass ToJSON

instance Describe VBox where

  describe (VBox vElems) =
    describeNamedRelFoldable 0 "VBox" vElems

data BoxContents
  = HBoxContents HBox
  | VBoxContents VBox
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Box a = Box {contents :: a, boxWidth, boxHeight, boxDepth :: Length}
  deriving stock (Show, Generic, Functor, Foldable)

deriving anyclass instance ToJSON a => ToJSON (Box a)

instance Dimensioned (Box BoxContents) where

  naturalLength dim (Box _ wid hei dep) = case dim of
    BoxWidth -> wid
    BoxHeight -> hei
    BoxDepth -> dep

data BaseElem
  = ElemBox (Box BoxContents)
  | ElemRule DVI.D.Rule
  | ElemFontDefinition DVI.D.FontDefinition
  | ElemFontSelection DVI.D.FontSelection
  | ElemKern Kern
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

spacerNaturalLength :: Axis -> BoxDim -> Length -> Length
spacerNaturalLength ax dim d = case (ax, dim) of
  (Vertical, BoxHeight) -> d
  (Horizontal, BoxWidth) -> d
  _ -> 0

axisBaseElemNaturalLength :: Axis -> BoxDim -> BaseElem -> Length
axisBaseElemNaturalLength ax dim e = case e of
  ElemFontSelection _ -> Length 0
  ElemFontDefinition _ -> Length 0
  ElemBox r -> naturalLength dim r
  ElemRule r -> naturalLength dim r
  ElemKern k -> spacerNaturalLength ax dim $ kernDimen k

data VBoxElem = VBoxBaseElem BaseElem | BoxGlue (SetGlue Length)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

axisVBoxElemNaturalLength :: Axis -> BoxDim -> VBoxElem -> Length
axisVBoxElemNaturalLength ax dim e = case e of
  VBoxBaseElem be -> axisBaseElemNaturalLength ax dim be
  BoxGlue g -> spacerNaturalLength ax dim $ glueDimen g

instance Dimensioned VBoxElem where

  naturalLength = axisVBoxElemNaturalLength Vertical

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
newtype HBaseElem = ElemCharacter DVI.D.Character
  deriving stock Show
  deriving newtype (ToJSON)

instance Dimensioned HBaseElem where

  naturalLength dim (ElemCharacter c) = naturalLength dim c

data HBoxElem = HVBoxElem VBoxElem | HBoxHBaseElem HBaseElem
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Dimensioned HBoxElem where

  naturalLength dim (HVBoxElem e) =
    axisVBoxElemNaturalLength Horizontal dim e
  naturalLength dim (HBoxHBaseElem e) = naturalLength dim e

newtype Page = Page (Box VBox)
  deriving stock Show

-- Display
instance Describe BaseElem where

  describe = \case
    ElemBox box ->
      [ (0, "BaseElem/ElemBox")
      ]
      <> describeRel 1 box
    ElemRule rule ->
      [ (0, "BaseElem/ElemRule")
      , (1, show rule)
      ]
    ElemFontDefinition fontDef ->
      [ (0, "BaseElem/ElemFontDefinition")
      ] <> describeRel 1 fontDef
    ElemFontSelection fontSel ->
      [ (0, "BaseElem/ElemFontSelection")
      ] <> describeRel 1 fontSel
    ElemKern (Kern d) ->
      [ (0, "BaseElem/ElemKern " <> quote (showSP d))
      ]

instance Describe HBaseElem where

  describe = \case
    ElemCharacter c ->
      [ (0, "HBaseElem/ElemCharacter")
      ] <> describeRel 1 c

instance Describe VBoxElem where

  describe = \case
    VBoxBaseElem baseElem ->
      [ (0, "VBoxElem/VBoxBaseElem")
      ] <> describeRel 1 baseElem
    BoxGlue sg ->
      [ (0, "VBoxElem/BoxGlue")
      ] <> describeRel 1 sg

instance Describe HBoxElem where

  describe = \case
    HVBoxElem (BoxGlue sg) ->
      [ (0, "HBoxElem/BoxGlue")
      ] <> describeRel 1 sg
    HVBoxElem vBoxElem ->
      [ (0, "HBoxElem/HVBoxElem")
      ] <> describeRel 1 vBoxElem
    HBoxHBaseElem hBaseElem ->
      [ (0, "HBoxElem/HBoxHBaseElem")
      ] <> describeRel 1 hBaseElem

instance Describe a => Describe (Box a) where

  describe Box {contents, boxWidth, boxHeight, boxDepth} =
    [ (0, "Box")
    ]
    <> describeNamedRel1 "Width" boxWidth
    <> describeNamedRel1 "Height" boxHeight
    <> describeNamedRel1 "Depth" boxDepth
    <> describeNamedRel1 "Contents" contents

instance Describe DesiredLength where

  describe = \case
    Natural -> singleLine "DesiredLength/Natural"
    Spread sp -> singleLine $ "DesiredLength/Spread " <> quote (showSP sp)
    To sp -> singleLine $ "DesiredLength/To " <> quote (showSP sp)

instance Describe BoxContents where

  describe = \case
    HBoxContents hBox -> describeNamedRel 0 "BoxContents/H" hBox
    VBoxContents vBox -> describeNamedRel 0 "BoxContents/V" vBox

instance Describe Page where

  describe (Page vBoxElems) =
    describeNamedRelFoldable 0 "Page" vBoxElems
