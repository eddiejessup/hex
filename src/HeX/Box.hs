{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Box where

import           Path                           ( Abs
                                                , File
                                                , Path
                                                )

import           TFM                            ( TexFont )

import qualified HeX.Unit                      as Unit
import HeX.Dimensioned                          ( Dimensioned(..) )

data Direction
  = Horizontal
  | Vertical
  deriving (Show)

data DesiredLength
  = Natural
  | Spread Int
  | To Int
  deriving (Show)

data Rule = Rule
  { width :: Int
  , height :: Int
  , depth :: Int
  } deriving (Show)

instance Dimensioned Rule where
  naturalWidth Rule {width = w} = w
  naturalHeight Rule {height = h} = h
  naturalDepth Rule {depth = d} = d

newtype Kern = Kern
  { kernDimen :: Int
  } deriving (Show)

data FontDefinition = FontDefinition
  { fontNr :: Int
  , fontPath :: Path Abs File
  , fontName :: String
  , scaleFactorRatio :: Rational
  , fontInfo :: TexFont
  }

instance Show FontDefinition where
  show FontDefinition {fontName = name} = "FontDefinition {" ++ name ++ "}"

newtype FontSelection = FontSelection
  { fontNr :: Int
  } deriving (Show)

data Character = Character
  { char :: Char
  , width :: Int
  , height :: Int
  , depth :: Int
  }

instance Show Character where
  show c = "'" ++ [char c] ++ "'"

instance Dimensioned Character where
  naturalWidth Character {width = w} = w
  naturalHeight Character {height = h} = h
  naturalDepth Character {depth = d} = d

newtype SetGlue = SetGlue
  { glueDimen :: Int
  }

instance Show SetGlue where
  show (SetGlue d) = "[" ++ Unit.showSP d ++ "]"

data BoxContents
  = HBoxContents [HBoxElem]
  | VBoxContents [VBoxElem]
  deriving (Show)

data Box = Box
  { contents :: BoxContents
  , desiredLength :: DesiredLength
  } deriving (Show)

instance Dimensioned Box where
  naturalWidth Box {contents = (HBoxContents cs), desiredLength = Natural} =
    sum $ fmap naturalWidth cs
  naturalWidth Box {contents = (HBoxContents _), desiredLength = To to} = to
  naturalWidth Box {contents = (VBoxContents [])} = 0
  naturalWidth Box {contents = (VBoxContents cs)} =
    maximum $ fmap naturalWidth cs
  naturalHeight Box {contents = (HBoxContents [])} = 0
  naturalHeight Box {contents = (HBoxContents cs)} =
    maximum $ naturalHeight <$> cs
  naturalHeight Box {contents = (VBoxContents cs), desiredLength = Natural} =
    sum $ fmap naturalHeight cs
  naturalHeight Box {contents = (VBoxContents _), desiredLength = To to} = to
  naturalDepth Box {contents = (HBoxContents [])} = 0
  naturalDepth Box {contents = (HBoxContents cs)} =
    maximum $ naturalDepth <$> cs
  -- TODO:
  -- NaturalDepth
  -- TODO.
  -- Spread

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HBoxElem
  = HVBoxElem VBoxElem
  | BoxCharacter Character
  deriving (Show)

instance Dimensioned HBoxElem where
  naturalWidth (HVBoxElem (BoxChild b)) = naturalWidth b
  naturalWidth (HVBoxElem (BoxRule r)) = naturalWidth r
  naturalWidth (HVBoxElem (BoxGlue g)) = glueDimen g
  naturalWidth (HVBoxElem (BoxKern k)) = kernDimen k
  naturalWidth (HVBoxElem (BoxFontDefinition _)) = 0
  naturalWidth (HVBoxElem (BoxFontSelection _)) = 0
  naturalWidth (BoxCharacter c) = naturalWidth c

  naturalHeight (HVBoxElem (BoxChild b)) = naturalHeight b
  naturalHeight (HVBoxElem (BoxRule r)) = naturalHeight r
  naturalHeight (HVBoxElem (BoxGlue _)) = 0
  naturalHeight (HVBoxElem (BoxKern _)) = 0
  naturalHeight (HVBoxElem (BoxFontDefinition _)) = 0
  naturalHeight (HVBoxElem (BoxFontSelection _)) = 0
  naturalHeight (BoxCharacter c) = naturalHeight c

  naturalDepth (HVBoxElem (BoxChild b)) = naturalDepth b
  naturalDepth (HVBoxElem (BoxRule r)) = naturalDepth r
  naturalDepth (HVBoxElem (BoxGlue _)) = 0
  naturalDepth (HVBoxElem (BoxKern _)) = 0
  naturalDepth (HVBoxElem (BoxFontDefinition _)) = 0
  naturalDepth (HVBoxElem (BoxFontSelection _)) = 0
  naturalDepth (BoxCharacter c) = naturalDepth c

data VBoxElem
  = BoxChild Box
  | BoxRule Rule
  | BoxGlue SetGlue
  | BoxKern Kern
  | BoxFontDefinition FontDefinition
  | BoxFontSelection FontSelection
  deriving (Show)

instance Dimensioned VBoxElem where
  naturalWidth (BoxChild b) = naturalWidth b
  naturalWidth (BoxRule r) = naturalWidth r
  naturalWidth (BoxGlue _) = 0
  naturalWidth (BoxKern _) = 0
  naturalWidth (BoxFontDefinition _) = 0
  naturalWidth (BoxFontSelection _) = 0

  naturalHeight (BoxChild b) = naturalHeight b
  naturalHeight (BoxRule r) = naturalHeight r
  naturalHeight (BoxGlue g) = glueDimen g
  naturalHeight (BoxKern k) = kernDimen k
  naturalHeight (BoxFontDefinition _) = 0
  naturalHeight (BoxFontSelection _) = 0

  naturalDepth (BoxChild b) = naturalDepth b
  naturalDepth (BoxRule r) = naturalDepth r
  naturalDepth (BoxGlue _) = 0
  naturalDepth (BoxKern _) = 0
  naturalDepth (BoxFontDefinition _) = 0
  naturalDepth (BoxFontSelection _) = 0

newtype Page = Page [VBoxElem]
  deriving (Show)
