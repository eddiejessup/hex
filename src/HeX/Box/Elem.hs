module HeX.Box.Elem
    ( FontDefinition(..)
    , FontSelection(..)
    , Rule(..)
    , Character(..)
    , module HeX.Box.Elem
    ) where

import           HeXlude

import           DVI.Document ( Character(..)
                              , FontDefinition(..)
                              , FontSelection(..)
                              , Rule(..)
                              )

import qualified HeX.Unit     as Unit

data DesiredLength = Natural | Spread LenVal | To LenVal
    deriving ( Show )

data VBoxAlignType = DefaultAlign -- \vbox
                   | TopAlign -- \vtop
    deriving ( Show, Eq )

newtype Kern = Kern { kernDimen :: LenVal }
    deriving ( Show )

newtype SetGlue = SetGlue { glueDimen :: LenVal }
    deriving ( Show )

instance Readable SetGlue where
    describe SetGlue{glueDimen} = "[" <> Unit.showSP glueDimen <> "]"

data BoxContents
    = HBoxContents [HBoxElem]
    | VBoxContents [VBoxElem] VBoxAlignType
    deriving ( Show )

data Box = Box { contents :: BoxContents, desiredLength :: DesiredLength }
    deriving ( Show )

instance Dimensioned Box where
    naturalLength dim b = case (dim, b) of
        -- HBox.
        (BoxWidth, Box (HBoxContents _) (To toLen)) -> toLen

        (BoxWidth, Box bc@(HBoxContents _) (Spread spread)) -> spread
            + (naturalLength BoxWidth (Box bc Natural))

        (BoxWidth, Box (HBoxContents cs) Natural) -> sumLength cs

        -- The height and depth of an hbox are the maximum distances by which
        -- the interior boxes reach above and below the baseline, respectively.
        -- An \hbox never has negative height or depth, but the width can be
        -- negative.
        (BoxHeight, Box (HBoxContents cs) _) -> maxLength cs

        (BoxDepth, Box (HBoxContents cs) _) -> maxLength cs

        -- VBox.
        -- The width of a \vbox is the maximum distance by which an
        -- enclosed box extends to the right of the reference point, taking
        -- possible shifting into account. This width is always nonnegative.
        (BoxWidth, Box (VBoxContents cs _) _) -> maxLength cs

        (BoxHeight, Box (VBoxContents _ _) (To toLen)) -> toLen

        (BoxHeight, Box bc@(VBoxContents _ _) (Spread spread)) -> spread
            + (naturalLength BoxHeight (Box bc Natural))

        (BoxHeight, Box (VBoxContents cs DefaultAlign) Natural) ->
            -- h + d for all but last elements, plus the last element's height.
            let allButLast = case initMay cs of
                    Nothing     -> 0
                    Just _inits -> sum $ hPlusD <$> _inits
                lastElem = case lastMay cs of
                    Nothing  -> 0
                    Just lst -> naturalLength BoxHeight lst
            in
                allButLast + lastElem

        -- When wrapping a vertical list via \vbox, to compute its depth,
        -- - If the list contains no boxes, the depth is zero.
        -- - If there's at least one box, consider the final box.
        --     - if the box is followed by kerning or glue, possibly with
        --       intervening penalties or other things, the depth is zero.
        --     - otherwise, the depth is that box's depth.
        -- If the computed depth exceeds \boxmaxdepth,
        --     - the depth is \boxmaxdepth
        --     - add the excess depth to the box's natural height, essentially
        --       moving the reference point down to reduce the depth to the
        --       stated maximum.
        (BoxDepth, Box (VBoxContents cs DefaultAlign) _) -> case lastMay cs of
            Nothing  -> 0
            Just lst -> naturalLength BoxDepth lst
      where
        subLengths :: Dimensioned a => [a] -> [LenVal]
        subLengths cs = naturalLength dim <$> cs

        maxLength cs = max 0 $ maximumDef 0 $ subLengths cs

        sumLength cs = sum $ subLengths cs

        hPlusD e = naturalLength BoxHeight e + naturalLength BoxDepth e

data BaseElem = ElemBox Box
              | ElemRule Rule
              | ElemFontDefinition FontDefinition
              | ElemFontSelection FontSelection
              | ElemKern Kern
    deriving ( Show )

spacerNaturalLength :: Axis -> BoxDim -> Int -> Int
spacerNaturalLength ax dim d = case (ax, dim) of
    (Vertical, BoxHeight) -> d
    (Horizontal, BoxWidth) -> d
    _ -> 0

axisBaseElemNaturalLength :: Axis -> BoxDim -> BaseElem -> Int
axisBaseElemNaturalLength ax dim e = case e of
    ElemFontSelection _ -> 0
    ElemFontDefinition _ -> 0
    ElemBox r -> naturalLength dim r
    ElemRule r -> naturalLength dim r
    ElemKern k -> spacerNaturalLength ax dim $ kernDimen k

data VBoxElem = VBoxBaseElem BaseElem | BoxGlue SetGlue
    deriving ( Show )

axisVBoxElemNaturalLength :: Axis -> BoxDim -> VBoxElem -> Int
axisVBoxElemNaturalLength ax dim e = case e of
    VBoxBaseElem be -> axisBaseElemNaturalLength ax dim be
    BoxGlue g       -> spacerNaturalLength ax dim $ glueDimen g

instance Dimensioned VBoxElem where
    naturalLength = axisVBoxElemNaturalLength Vertical

-- TODO: Ligature, DiscretionaryBreak, Math on/off, V-adust
data HBaseElem = ElemCharacter Character
    deriving ( Show )

instance Dimensioned HBaseElem where
    naturalLength dim (ElemCharacter c) = naturalLength dim c

data HBoxElem = HVBoxElem VBoxElem | HBoxHBaseElem HBaseElem
    deriving ( Show )

instance Dimensioned HBoxElem where
    naturalLength dim (HVBoxElem e) =
        axisVBoxElemNaturalLength Horizontal dim e
    naturalLength dim (HBoxHBaseElem e) = naturalLength dim e

newtype Page = Page [VBoxElem]
    deriving ( Show )
