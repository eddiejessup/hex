module HeX.Box.Elem
  ( DVI.D.FontDefinition (..)
  , DVI.D.FontSelection (..)
  , DVI.D.Rule (..)
  , DVI.D.Character (..)
  , module HeX.Box.Elem
  )
where

import qualified DVI.Document as DVI.D
import qualified Data.Text as Text
import HeX.Config.Codes
import HeX.Quantity
import HeXlude

data DesiredLength = Natural | Spread Length | To Length
  deriving Show

newtype Kern = Kern {kernDimen :: Length}
  deriving Show

newtype SetGlue a = SetGlue {glueDimen :: a}
  deriving Show

instance Readable (SetGlue Length) where

  describe SetGlue {glueDimen} = "Glue<" <> showSP glueDimen <> ">"

newtype HBox = HBox (Seq HBoxElem)
  deriving (Show, Semigroup, Monoid)

instance Readable HBox where

  describe (HBox elems) =
    describeListHeaded 1 "HBox" (condenseChary elems toMaybeChar)
    where
      toMaybeChar = \case
        HBoxHBaseElem (ElemCharacter DVI.D.Character {DVI.D.char}) ->
          Just $ codeAsChar char
        _ ->
          Nothing

-- Just used to show an HList more compactly.
data Sentential a = Sentence Text | NonSentence a
  deriving (Show, Functor, Foldable)

instance Readable a => Readable (Sentential a) where

  describe = \case
    Sentence s -> "\"" <> s <> "\""
    NonSentence a -> describe a

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
  deriving (Show, Semigroup, Monoid)

instance Readable VBox where

  describe (VBox vElems) =
    describeListHeaded 1 "VBox" vElems

data BoxContents
  = HBoxContents HBox
  | VBoxContents VBox
  deriving Show

data Box a = Box {contents :: a, boxWidth, boxHeight, boxDepth :: Length}
  deriving (Show, Functor, Foldable)

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
  deriving Show

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
  deriving Show

axisVBoxElemNaturalLength :: Axis -> BoxDim -> VBoxElem -> Length
axisVBoxElemNaturalLength ax dim e = case e of
  VBoxBaseElem be -> axisBaseElemNaturalLength ax dim be
  BoxGlue g -> spacerNaturalLength ax dim $ glueDimen g

instance Dimensioned VBoxElem where

  naturalLength = axisVBoxElemNaturalLength Vertical

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
newtype HBaseElem = ElemCharacter DVI.D.Character
  deriving Show

instance Dimensioned HBaseElem where

  naturalLength dim (ElemCharacter c) = naturalLength dim c

data HBoxElem = HVBoxElem VBoxElem | HBoxHBaseElem HBaseElem
  deriving Show

instance Dimensioned HBoxElem where

  naturalLength dim (HVBoxElem e) =
    axisVBoxElemNaturalLength Horizontal dim e
  naturalLength dim (HBoxHBaseElem e) = naturalLength dim e

newtype Page = Page (Box VBox)
  deriving Show

-- Display
instance Readable BaseElem where

  describe = \case
    ElemBox box -> describe box
    ElemRule rule -> show rule
    ElemFontDefinition fontDef -> describe fontDef
    ElemFontSelection fontSel -> describe fontSel
    ElemKern (Kern d) -> "Kern<" <> showSP d <> ">"

instance Readable HBaseElem where

  describe = \case
    ElemCharacter c -> describe c

instance Readable VBoxElem where

  describe = \case
    VBoxBaseElem baseElem ->
      describe baseElem
    BoxGlue sg ->
      "VGlue<" <> describe sg <> ">"

instance Readable HBoxElem where

  describe = \case
    HVBoxElem (BoxGlue sg) ->
      "HGlue<" <> describe sg <> ">"
    HVBoxElem vBoxElem ->
      describe vBoxElem
    HBoxHBaseElem hBaseElem ->
      describe hBaseElem

instance Readable a => Readable (Box a) where

  describe Box {contents, boxWidth, boxHeight, boxDepth} =
    "Box<w=" <> describe boxWidth <> ", h=" <> describe boxHeight <> ", d=" <> describe boxDepth <> "> [[[" <>
      "\n" <>
      describe contents <>
      "\n]]] Box"

instance Readable DesiredLength where

  describe = \case
    Natural -> "Natural"
    Spread sp -> "Spread<" <> showSP sp <> ">"
    To sp -> "To<" <> showSP sp <> ">"

instance Readable BoxContents where

  describe = \case
    HBoxContents hBox -> describe hBox
    VBoxContents vBox -> describe vBox

instance Readable Page where

  describe (Page vBoxElems) =
    describeListHeaded 1 "Page" vBoxElems
