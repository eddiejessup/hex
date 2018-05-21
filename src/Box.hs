{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Box where

import qualified DVI.Write as DVIW

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

data Kern = Kern
  { dimen :: Int
  } deriving (Show)

data FontDefinition = FontDefinition
  { fontNr :: Int
  , fontPath :: FilePath
  , fontName :: FilePath
  , scaleFactorRatio :: Rational
  , fontInfo :: TFMM.TexFont
  } deriving (Show)

data FontSelection = FontSelection
  { fontNr :: Int
  } deriving (Show)

data Character = Character
  { code :: Int
  , width :: Int
  , height :: Int
  , depth :: Int
  } deriving (Show)

data SetGlue = SetGlue
  { dimen :: Int
  } deriving (Show)

data VBox = VBox
  { contents :: [VBoxElement]
  , desiredLength :: DesiredLength
  } deriving (Show)

data HBox = HBox
  { contents :: [HBoxElement]
  , desiredLength :: DesiredLength
  } deriving (Show)

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust

data HBoxElement
  = HVBox VBox
  | HHBox HBox
  | HRule Rule
  | HGlue SetGlue
  | HKern Kern
  | HFontDefinition FontDefinition
  | HFontSelection FontSelection
  | HCharacter Character
  deriving (Show)

data VBoxElement
  = VVBox VBox
  | VHBox HBox
  | VRule Rule
  | VGlue SetGlue
  | VKern Kern
  | VFontDefinition FontDefinition
  | VFontSelection FontSelection
  deriving (Show)

data Page = Page [VBoxElement]

class Dimensioned a where
  naturalWidth :: a -> Int

instance Dimensioned HBoxElement where
  naturalWidth (HVBox v) = naturalWidth v
  naturalWidth (HHBox h) = naturalWidth h
  naturalWidth (HRule Rule{width=w}) = w
  naturalWidth (HGlue SetGlue{dimen=d}) = d
  naturalWidth (HKern Kern{dimen=d}) = d
  naturalWidth (HFontDefinition _) = 0
  naturalWidth (HFontSelection _) = 0
  naturalWidth (HCharacter Character{width=w}) = w

instance Dimensioned VBoxElement where
  naturalWidth (VVBox v) = naturalWidth v
  naturalWidth (VHBox h) = naturalWidth h
  naturalWidth (VRule Rule{width=w}) = w
  naturalWidth (VGlue _) = 0
  naturalWidth (VKern _) = 0
  naturalWidth (VFontDefinition _) = 0
  naturalWidth (VFontSelection _) = 0

instance Dimensioned VBox where
  naturalWidth VBox{contents=cs, desiredLength=d} =
    case d of
      Natural -> maximum $ fmap naturalWidth cs
      To to -> to
      -- TODO.
      -- Spread spread -> 1010

instance Dimensioned HBox where
  naturalWidth HBox{contents=cs, desiredLength=d} =
    case d of
      Natural -> sum $ fmap naturalWidth cs
      To to -> to
      -- TODO.
      -- Spread spread -> 1010

class DVIAble a where
  toDVI :: a -> [DVIW.Instruction]

instance DVIAble VBox where
  toDVI VBox{contents=cs} = concatMap toDVI cs

instance DVIAble HBox where
  toDVI HBox{contents=cs} = concatMap toDVI cs

-- TODO: Rule.

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
   = [ DVIW.DefineFont
        { fontInfo = info
        , fontPath = name
        , fontNr = fNr
        , scaleFactorRatio = scale
        }
      ]

instance DVIAble FontSelection where
  toDVI FontSelection {fontNr = fNr} = [DVIW.SelectFont fNr]

instance DVIAble Character where
  toDVI Character {code = c} = [DVIW.Character {charNr = c, move = True}]

instance DVIAble HBoxElement where
    -- TODO: Move by amount dependent on contents.
  toDVI (HVBox e) = [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveRight {distance = 200000}]
    -- TODO: Move by amount dependent on contents.
  toDVI (HHBox e) = [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveRight {distance = 200000}]
    -- TODO: Rule.
  toDVI (HGlue SetGlue {dimen = d}) = [DVIW.MoveRight {distance = d}]
  toDVI (HKern Kern {dimen = d}) = [DVIW.MoveRight {distance = d}]
  toDVI (HFontDefinition e@FontDefinition {}) = toDVI e
  toDVI (HFontSelection e@FontSelection {}) = toDVI e
  toDVI (HCharacter e@Character {}) = toDVI e

instance DVIAble VBoxElement where
    -- TODO: Move by amount dependent on contents.
  toDVI (VVBox e) = [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveDown {distance = 200000}]
    -- TODO: Move by amount dependent on contents.
  toDVI (VHBox e) = [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveDown {distance = 200000}]
    -- TODO: Rule.
  toDVI (VGlue SetGlue {dimen = d}) = [DVIW.MoveDown {distance = d}]
  toDVI (VKern Kern {dimen = d}) = [DVIW.MoveDown {distance = d}]
  toDVI (VFontDefinition e) = toDVI e
  toDVI (VFontSelection e) = toDVI e

instance DVIAble Page where
  toDVI (Page vs) = [DVIW.BeginNewPage] ++ concatMap toDVI vs

instance DVIAble [Page] where
  toDVI ps = concatMap toDVI ps
