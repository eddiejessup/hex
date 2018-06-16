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
  { kernDimen :: Int
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
  { glueDimen :: Int
  } deriving (Show)

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

data Page = Page [VBoxElem] deriving Show

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

instance DVIAble HBoxElem where
  toDVI (HVBox e) =
    [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveRight {distance = naturalWidth e}]
  toDVI (HHBox e) =
    [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveRight {distance=naturalWidth e}]
    -- TODO: Rule.
  toDVI (HGlue g) = [DVIW.MoveRight {distance = glueDimen g}]
  toDVI (HKern k) = [DVIW.MoveRight {distance = kernDimen k}]
  toDVI (HFontDefinition e@FontDefinition {}) = toDVI e
  toDVI (HFontSelection e@FontSelection {}) = toDVI e
  toDVI (HCharacter e@Character {}) = toDVI e

instance DVIAble VBoxElem where
  toDVI (VVBox e) =
    [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveDown {distance = naturalHeight e}]
  toDVI (VHBox e) =
    [DVIW.PushStack] ++ toDVI e ++ [DVIW.PopStack, DVIW.MoveDown {distance = naturalHeight e}]
    -- TODO: Rule.
  toDVI (VGlue g) = [DVIW.MoveDown {distance = glueDimen g}]
  toDVI (VKern k) = [DVIW.MoveDown {distance = kernDimen k}]
  toDVI (VFontDefinition e) = toDVI e
  toDVI (VFontSelection e) = toDVI e

instance DVIAble Page where
  toDVI (Page vs) = [DVIW.BeginNewPage] ++ concatMap toDVI vs

instance DVIAble [Page] where
  toDVI ps = concatMap toDVI ps
