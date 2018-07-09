{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module BoxDraw where

import qualified Data.Char as C
import Path (Path, Abs, File)

import qualified Unit
import qualified DVI.Encode as DVIE
import qualified TFM.Main as TFMM

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

newtype Kern = Kern{kernDimen :: Int} deriving (Show)

data FontDefinition = FontDefinition
  { fontNr :: Int
  , fontPath :: Path Abs File
  , fontName :: String
  , scaleFactorRatio :: Rational
  , fontInfo :: TFMM.TexFont
  } deriving (Show)

newtype FontSelection = FontSelection{fontNr :: Int} deriving (Show)

data Character = Character
  { code :: Int
  , width :: Int
  , height :: Int
  , depth :: Int
  }

instance Show Character where
  show c = "'" ++ [C.chr $ code c] ++ "'"

newtype SetGlue = SetGlue{glueDimen :: Int}

instance Show SetGlue where
  show (SetGlue d) = "[" ++ Unit.showSP d ++ "]"

data VBox = VBox
  { contents :: [VBoxElem]
  , desiredLength :: DesiredLength
  } deriving (Show)

data HBox = HBox
  { contents :: [HBoxElem]
  , desiredLength :: DesiredLength
  } deriving (Show)

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

data HBoxElem
  = HVBox VBox
  | HHBox HBox
  | HRule Rule
  | HGlue SetGlue
  | HKern Kern
  | HFontDefinition FontDefinition
  | HFontSelection FontSelection
  | HCharacter Character
  deriving (Show)

data VBoxElem
  = VVBox VBox
  | VHBox HBox
  | VRule Rule
  | VGlue SetGlue
  | VKern Kern
  | VFontDefinition FontDefinition
  | VFontSelection FontSelection
  deriving (Show)

newtype Page = Page [VBoxElem] deriving Show

class Dimensioned a where
  naturalWidth :: a -> Int
  naturalHeight :: a -> Int

instance Dimensioned Rule where
  naturalWidth Rule{width=w} = w
  naturalHeight Rule{height=h} = h

instance Dimensioned Character where
  naturalWidth Character{width=w} = w
  naturalHeight Character{height=h} = h

instance Dimensioned HBoxElem where
  naturalWidth (HVBox v) = naturalWidth v
  naturalWidth (HHBox h) = naturalWidth h
  naturalWidth (HRule r) = naturalWidth r
  naturalWidth (HGlue g) = glueDimen g
  naturalWidth (HKern k) = kernDimen k
  naturalWidth (HFontDefinition _) = 0
  naturalWidth (HFontSelection _) = 0
  naturalWidth (HCharacter c) = naturalWidth c

  naturalHeight (HVBox v) = naturalHeight v
  naturalHeight (HHBox h) = naturalHeight h
  naturalHeight (HRule r) = naturalHeight r
  naturalHeight (HGlue _) = 0
  naturalHeight (HKern _) = 0
  naturalHeight (HFontDefinition _) = 0
  naturalHeight (HFontSelection _) = 0
  naturalHeight (HCharacter c) = naturalHeight c

instance Dimensioned VBoxElem where
  naturalWidth (VVBox v) = naturalWidth v
  naturalWidth (VHBox h) = naturalWidth h
  naturalWidth (VRule r) = naturalWidth r
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0

  naturalHeight (VVBox v) = naturalHeight v
  naturalHeight (VHBox h) = naturalHeight h
  naturalHeight (VRule r) = naturalHeight r
  naturalHeight (VGlue g) = glueDimen g
  naturalHeight (VKern k) = kernDimen k
  naturalHeight (VFontDefinition _) = 0
  naturalHeight (VFontSelection _) = 0

instance Dimensioned VBox where
  naturalHeight VBox{contents=cs, desiredLength=d} =
    case d of
      Natural -> sum $ fmap naturalWidth cs
      To to -> to
      -- TODO.
      -- Spread spread -> 1010

  naturalWidth VBox{contents=[]} = 0
  naturalWidth VBox{contents=cs} = maximum $ fmap naturalWidth cs

instance Dimensioned HBox where
  naturalWidth HBox{contents=cs, desiredLength=d} =
    case d of
      Natural -> sum $ fmap naturalWidth cs
      To to -> to
      -- TODO.
      -- Spread spread -> 1010

  naturalHeight HBox{contents=[]} = 0
  naturalHeight HBox{contents=cs} = maximum $ fmap naturalHeight cs

class DVIAble a where
  toDVI :: a -> [DVIE.Instruction]

instance DVIAble VBox where
  toDVI VBox{contents=cs} = concatMap toDVI cs

instance DVIAble HBox where
  toDVI HBox{contents=cs} = concatMap toDVI cs

-- TODO: Don't know how to handle depth.
instance DVIAble Rule where
  toDVI Rule{width=w, height=h, depth=d} = [DVIE.Rule{height=h + d, width=w, move=True}]

instance DVIAble SetGlue where
  toDVI SetGlue {} = []

instance DVIAble Kern where
  toDVI Kern {} = []

instance DVIAble FontDefinition where
  toDVI FontDefinition { fontNr = fNr
                         , fontPath = path
                         , fontName = name
                         , fontInfo = info
                         , scaleFactorRatio = scale
                         }
   = [ DVIE.DefineFont
        { fontInfo = info
        , fontPath = name
        , fontNr = fNr
        , scaleFactorRatio = scale
        }
      ]

instance DVIAble FontSelection where
  toDVI FontSelection {fontNr = fNr} = [DVIE.SelectFont fNr]

instance DVIAble Character where
  toDVI Character {code = c} = [DVIE.Character {charNr = c, move = True}]

instance DVIAble HBoxElem where
  toDVI (HVBox e) =
    [DVIE.PushStack] ++ toDVI e ++ [DVIE.PopStack, DVIE.MoveRight {distance = naturalWidth e}]
  toDVI (HHBox e) =
    [DVIE.PushStack] ++ toDVI e ++ [DVIE.PopStack, DVIE.MoveRight {distance=naturalWidth e}]
    -- TODO: Rule.
  toDVI (HGlue g) = [DVIE.MoveRight {distance = glueDimen g}]
  toDVI (HKern k) = [DVIE.MoveRight {distance = kernDimen k}]
  toDVI (HRule r) =
    [DVIE.PushStack] ++ toDVI r ++ [DVIE.PopStack, DVIE.MoveRight {distance = naturalWidth r}]
  toDVI (HFontDefinition e) = toDVI e
  toDVI (HFontSelection e) = toDVI e
  toDVI (HCharacter e) = toDVI e

instance DVIAble VBoxElem where
  toDVI (VVBox e) =
    [DVIE.PushStack] ++ toDVI e ++ [DVIE.PopStack, DVIE.MoveDown {distance = naturalHeight e}]
  toDVI (VHBox e) =
    [DVIE.PushStack] ++ toDVI e ++ [DVIE.PopStack, DVIE.MoveDown {distance = naturalHeight e}]
    -- TODO: Rule.
  toDVI (VGlue g) = [DVIE.MoveDown {distance = glueDimen g}]
  toDVI (VKern k) = [DVIE.MoveDown {distance = kernDimen k}]
  toDVI (VRule r) =
    [DVIE.PushStack] ++ toDVI r ++ [DVIE.PopStack, DVIE.MoveDown {distance = naturalHeight r}]
  toDVI (VFontDefinition e) = toDVI e
  toDVI (VFontSelection e) = toDVI e

instance DVIAble Page where
  toDVI (Page vs) = DVIE.BeginNewPage:concatMap toDVI vs

instance DVIAble [Page] where
  toDVI = concatMap toDVI
