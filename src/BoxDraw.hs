{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module BoxDraw where

import qualified Data.Char as C
import Path (Path, Abs, File)

import qualified Unit
import qualified DVI.Encode as DVIE
import qualified TFM.Main as TFMM

data Direction = Horizontal | Vertical deriving Show

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
  }

instance Show FontDefinition where
  show FontDefinition{fontName=name} = "FontDefinition {" ++ name ++ "}"

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

data BoxContents = HBoxContents [HBoxElem] | VBoxContents [VBoxElem]
  deriving Show

data Box = Box
  { contents :: BoxContents
  , desiredLength :: DesiredLength
  } deriving (Show)

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

data HBoxElem
  = HChild Box
  | HRule Rule
  | HGlue SetGlue
  | HKern Kern
  | HFontDefinition FontDefinition
  | HFontSelection FontSelection
  | HCharacter Character
  deriving (Show)

data VBoxElem
  = VChild Box
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
  naturalDepth :: a -> Int

instance Dimensioned Rule where
  naturalWidth Rule{width=w} = w
  naturalHeight Rule{height=h} = h
  naturalDepth Rule{depth=d} = d

instance Dimensioned Character where
  naturalWidth Character{width=w} = w
  naturalHeight Character{height=h} = h
  naturalDepth Character{depth=d} = d

instance Dimensioned HBoxElem where
  naturalWidth (HChild b) = naturalWidth b
  naturalWidth (HRule r) = naturalWidth r
  naturalWidth (HGlue g) = glueDimen g
  naturalWidth (HKern k) = kernDimen k
  naturalWidth (HFontDefinition _) = 0
  naturalWidth (HFontSelection _) = 0
  naturalWidth (HCharacter c) = naturalWidth c

  naturalHeight (HChild b) = naturalHeight b
  naturalHeight (HRule r) = naturalHeight r
  naturalHeight (HGlue _) = 0
  naturalHeight (HKern _) = 0
  naturalHeight (HFontDefinition _) = 0
  naturalHeight (HFontSelection _) = 0
  naturalHeight (HCharacter c) = naturalHeight c

  naturalDepth (HChild b) = naturalDepth b
  naturalDepth (HRule r) = naturalDepth r
  naturalDepth (HGlue _) = 0
  naturalDepth (HKern _) = 0
  naturalDepth (HFontDefinition _) = 0
  naturalDepth (HFontSelection _) = 0
  naturalDepth (HCharacter c) = naturalDepth c

instance Dimensioned VBoxElem where
  naturalWidth (VChild b) = naturalWidth b
  naturalWidth (VRule r) = naturalWidth r
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0

  naturalHeight (VChild b) = naturalHeight b
  naturalHeight (VRule r) = naturalHeight r
  naturalHeight (VGlue g) = glueDimen g
  naturalHeight (VKern k) = kernDimen k
  naturalHeight (VFontDefinition _) = 0
  naturalHeight (VFontSelection _) = 0

  naturalDepth (VChild b) = naturalDepth b
  naturalDepth (VRule r) = naturalDepth r
  naturalDepth (VGlue _) = 0
  naturalDepth (VKern _) = 0
  naturalDepth (VFontDefinition _) = 0
  naturalDepth (VFontSelection _) = 0

instance Dimensioned Box where
  naturalWidth Box{contents=(HBoxContents cs), desiredLength=Natural} = sum $ fmap naturalWidth cs
  naturalWidth Box{contents=(HBoxContents _), desiredLength=To to} = to
  naturalWidth Box{contents=(VBoxContents [])} = 0
  naturalWidth Box{contents=(VBoxContents cs)} = maximum $ fmap naturalWidth cs

  naturalHeight Box{contents=(HBoxContents [])} = 0
  naturalHeight Box{contents=(HBoxContents cs)} = maximum $ naturalHeight <$> cs
  naturalHeight Box{contents=(VBoxContents cs), desiredLength=Natural} = sum $ fmap naturalHeight cs
  naturalHeight Box{contents=(VBoxContents _), desiredLength=To to} = to

  naturalDepth Box{contents=(HBoxContents [])} = 0
  naturalDepth Box{contents=(HBoxContents cs)} = maximum $ naturalDepth <$> cs
  -- TODO:
  -- NaturalDepth

  -- TODO.
  -- Spread


class DVIAble a where
  toDVI :: a -> [DVIE.Instruction]

instance DVIAble Box where
  toDVI Box{contents=HBoxContents cs} = concatMap toDVI cs
  toDVI Box{contents=VBoxContents cs} = concatMap toDVI cs

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
  toDVI (HChild b) =
    [DVIE.PushStack] ++ toDVI b ++ [DVIE.PopStack, DVIE.MoveRight {distance = naturalWidth b}]
    -- TODO: Rule.
  toDVI (HGlue g) = [DVIE.MoveRight {distance = glueDimen g}]
  toDVI (HKern k) = [DVIE.MoveRight {distance = kernDimen k}]
  toDVI (HRule r) =
    [DVIE.PushStack] ++ toDVI r ++ [DVIE.PopStack, DVIE.MoveRight {distance = naturalWidth r}]
  toDVI (HFontDefinition e) = toDVI e
  toDVI (HFontSelection e) = toDVI e
  toDVI (HCharacter e) = toDVI e

instance DVIAble VBoxElem where
  toDVI (VChild b) =
    [DVIE.PushStack] ++ toDVI b ++ [DVIE.PopStack, DVIE.MoveDown {distance = naturalHeight b + naturalDepth b}]
    -- TODO: Rule.
  toDVI (VGlue g) = [DVIE.MoveDown {distance = glueDimen g}]
  toDVI (VKern k) = [DVIE.MoveDown {distance = kernDimen k}]
  toDVI (VRule r) =
    [DVIE.PushStack] ++ toDVI r ++ [DVIE.PopStack, DVIE.MoveDown {distance = naturalHeight r + naturalDepth r}]
  toDVI (VFontDefinition e) = toDVI e
  toDVI (VFontSelection e) = toDVI e

instance DVIAble Page where
  toDVI (Page vs) = DVIE.BeginNewPage:concatMap toDVI vs

instance DVIAble [Page] where
  toDVI = concatMap toDVI
