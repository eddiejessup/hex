{-# LANGUAGE DuplicateRecordFields #-}

module HeX.Box 
  ( FontDefinition(..)
  , FontSelection(..)
  , Rule(..)
  , Character(..)
  , module HeX.Box
  )
where

import           Safe.Foldable                  ( maximumDef )
import           Data.Concept

import           DVI.Document                   ( FontDefinition(..)
                                                , FontSelection(..)
                                                , Rule(..)
                                                , Character(..)
                                                )

import qualified HeX.Unit                      as Unit

data DesiredLength
    = Natural
    -- TODO: Implement Spread.
    -- | Spread Int
    | To Int
    deriving (Show)

newtype Kern = Kern { kernDimen :: Int }
    deriving (Show)

newtype SetGlue = SetGlue { glueDimen :: Int }

instance Show SetGlue where
    show g = "[" ++ Unit.showSP (glueDimen g) ++ "]"

data BoxContents
    = HBoxContents [HBoxElem]
    | VBoxContents [VBoxElem]
    deriving (Show)

data Box = Box
    { contents :: BoxContents
    , desiredLength :: DesiredLength
    } deriving (Show)

instance Dimensioned Box where
    naturalWidth b = case b of
        Box (HBoxContents _) (To to)  -> to
        Box (HBoxContents cs) Natural -> sum $ naturalWidth <$> cs
        Box (VBoxContents cs) _       -> maximumDef 0 $ naturalWidth <$> cs

    naturalHeight b = case b of
        Box (VBoxContents _) (To to)  -> to
        Box (VBoxContents cs) Natural -> sum $ naturalHeight <$> cs
        Box (HBoxContents cs) _       -> maximumDef 0 $ naturalHeight <$> cs

    naturalDepth b = case b of
        -- TODO: Look up and implement specification.
        Box (VBoxContents _) _  -> error "Not implemented: Depth of VBox"
        Box (HBoxContents cs) _ -> maximumDef 0 $ naturalDepth <$> cs

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HBoxElem
    = HVBoxElem VBoxElem
    | BoxCharacter Character
    deriving (Show)

instance Dimensioned HBoxElem where
    naturalWidth e = case e of
        BoxCharacter b                  -> naturalWidth b
        HVBoxElem (BoxChild b)          -> naturalWidth b
        HVBoxElem (BoxRule b)           -> naturalWidth b
        HVBoxElem (BoxGlue g)           -> glueDimen g
        HVBoxElem (BoxKern k)           -> kernDimen k
        HVBoxElem (BoxFontDefinition _) -> 0
        HVBoxElem (BoxFontSelection _)  -> 0

    naturalHeight e = case e of
        BoxCharacter b                  -> naturalHeight b
        HVBoxElem (BoxChild b)          -> naturalHeight b
        HVBoxElem (BoxRule b)           -> naturalHeight b
        HVBoxElem (BoxGlue _)           -> 0
        HVBoxElem (BoxKern _)           -> 0
        HVBoxElem (BoxFontDefinition _) -> 0
        HVBoxElem (BoxFontSelection _)  -> 0

    naturalDepth e = case e of
        BoxCharacter b                  -> naturalDepth b
        HVBoxElem (BoxChild b)          -> naturalDepth b
        HVBoxElem (BoxRule b)           -> naturalDepth b
        HVBoxElem (BoxGlue _)           -> 0
        HVBoxElem (BoxKern _)           -> 0
        HVBoxElem (BoxFontDefinition _) -> 0
        HVBoxElem (BoxFontSelection _)  -> 0

data VBoxElem
    = BoxChild Box
    | BoxRule Rule
    | BoxGlue SetGlue
    | BoxKern Kern
    | BoxFontDefinition FontDefinition
    | BoxFontSelection FontSelection
    deriving (Show)

instance Dimensioned VBoxElem where
    naturalWidth e = case e of
        BoxChild b          -> naturalWidth b
        BoxRule r           -> naturalWidth r
        BoxGlue _           -> 0
        BoxKern _           -> 0
        BoxFontDefinition _ -> 0
        BoxFontSelection _  -> 0

    naturalHeight e = case e of
        BoxChild b          -> naturalHeight b
        BoxRule r           -> naturalHeight r
        BoxGlue g           -> glueDimen g
        BoxKern k           -> kernDimen k
        BoxFontDefinition _ -> 0
        BoxFontSelection _  -> 0

    naturalDepth e = case e of
        BoxChild b          -> naturalDepth b
        BoxRule r           -> naturalDepth r
        BoxGlue _           -> 0
        BoxKern _           -> 0
        BoxFontDefinition _ -> 0
        BoxFontSelection _  -> 0

newtype Page = Page [VBoxElem]
    deriving (Show)
