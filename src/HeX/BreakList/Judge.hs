module HeX.BreakList.Judge where

import           HeXlude

import           HeX.Box                 ( SetGlue(..) )
import           HeX.BreakList.BreakList ( BreakableListElem(..)
                                         , naturalListSpan
                                         , totalGlue
                                         )
import           HeX.BreakList.Glue      ( Glue(..), GlueFlex(..) )
import           HeX.Unit                ( showFrac, showSP, tenK )

-- Here’s the way TeX goes about setting the glue when an hbox is being wrapped
-- up: The natural width, x, of the box contents is determined by adding up the
-- widths of the boxes and kerns inside, together with the natural widths of
-- all the glue inside. Furthermore the total amount of glue stretchability and
-- shrinkability in the box is computed; let’s say that there’s a total of y0 +
-- y1 fil + y2 fill + y3 filll available for stretching and z0 + z1 fil + z2
-- fill + z3 filll available for shrinking. Now the natural width x is compared
-- to the desired width w. If x = w, all glue gets its natural width. Otherwise
-- the glue will be modified, by computing a “glue set ratio” r and a “glue set
-- order” i in the following way: (a) If x < w, TeX attempts to stretch the
-- contents of the box; the glue order is the highest subscript i such that yi
-- is nonzero, and the glue ratio is r = (w−x)/yi. (If y0 = y1 = y2 = y3 = 0,
-- there’s no stretchability; both i and r are set to zero.) (b) If x > w, TeX
-- attempts to shrink the contents of the box in a similar way; the glue order
-- is the highest subscript i such that zi ̸= 0, and the glue ratio is normally
-- r = (x−w)/zi. However, r is set to 1.0 in the case i = 0 and x − w > z0,
-- because the maximum shrinkability must not be exceeded. (c) Finally, every
-- glob of glue in the horizontal list being boxed is modified. Suppose the
-- glue has natural width u, stretchability y, and shrinkability z, where y is
-- a jth order infinity and z is a kth order infinity. Then if x < w
-- (stretching), this glue takes the new width u+ry if j = i; it keeps its
-- natural width u if j ̸= i. If x > w (shrinking), this glue takes the new
-- width u−rz if k = i; it keeps its natural width u if k ̸= i. Notice that
-- stretching or shrinking occurs only when the glue has the highest order of
-- infinity that doesn’t cancel out.

data FixParams = FixParams { ratio :: Rational, setOrder :: Int }
    deriving ( Show )

instance Readable FixParams where
    describe (FixParams r n) = case n of
        0 -> "Finite ratio: " <> showFrac r
        1 -> "Fil ratio: " <> showSP r
        _ -> "Fil order: " <> show n <> ", ratio: " <> showSP r

data LengthJudgment = Bare | Full | Overfull
    deriving ( Show )

data GlueStatus =
    NaturallyGood | UnfixablyBare | FixablyBad LengthJudgment FixParams
    deriving ( Show )

glueStatus :: Int -> Glue -> GlueStatus
glueStatus excessLength
           (Glue _ _stretch _shrink) = case compare excessLength 0 of
    -- The glue ratio is r = [excess length]/[flex]i.
    -- The natural width x is compared to the desired width w.
    -- If x = w, all glue gets its natural width.
    EQ -> NaturallyGood
    -- Otherwise the glue will be modified, by computing a “glue set ratio” r
    -- and a “glue set order” i
    -- LT -> stretchStatus _stretch
    LT -> stretchStatus _stretch
    GT -> shrinkStatus _shrink
  where
    toRatio f = abs $ fromIntegral excessLength / f

    -- No stretchability. Not much we can do in this case.
    stretchStatus GlueFlex{factor = f, order = o}
        | f == 0 = UnfixablyBare
        | otherwise =
            FixablyBad Bare FixParams { ratio = toRatio f, setOrder = o }

    -- r is set to 1 if i = 0 and x − w > z0, because the maximum
    -- shrinkability must not be exceeded.
    shrinkStatus GlueFlex{factor = f, order = o}
        | o == 0 && (fromIntegral excessLength > f) =
            FixablyBad Overfull FixParams { ratio = 1, setOrder = o }
        | otherwise =
            FixablyBad Full FixParams { ratio = toRatio f, setOrder = o }


listGlueStatus :: BreakableListElem a => Int -> [a] -> GlueStatus
listGlueStatus desiredLength cs =
    glueStatus (naturalListSpan cs - desiredLength) (totalGlue cs)

-- TODO: Use types to ensure number is within bounds, such as <= tenK.
data Badness = FiniteBadness Int | InfiniteBadness
    deriving ( Show )

-- The badness of a line is approximately 100 times the cube
-- of the glue set ratio. But if the badness obtained by this method turns out to be
-- more than 10000, the value 10000 is used. If i ̸= 0, there is infinite
-- stretchability or shrinkability, so the badness is zero, otherwise the
-- badness is approximately min(100r3,10000). Overfull boxes are considered to
-- be infinitely bad; they are avoided whenever possible.
badness :: GlueStatus -> Badness
badness gs = case gs of
    NaturallyGood -> FiniteBadness 0
    UnfixablyBare -> FiniteBadness tenK
    FixablyBad Overfull _ -> InfiniteBadness
    -- if i == 0, the badness is approximately min(100r^3, 10000).
    -- Otherwise, there is infinite stretchability or shrinkability, so the
    -- badness is zero.
    FixablyBad _ FixParams{ratio, setOrder}
        | setOrder == 0 ->
            FiniteBadness $ min tenK $ round $ (ratio ^ (3 :: Int)) * 100
        | otherwise -> FiniteBadness 0


-- Suppose the line order is i, and the glue has natural width u, and
-- flexibility f_j, corresponding to its amount and order of stretch or shrink
-- as appropriate.
-- The glue's width is u, plus: r * f_j if j = i; otherwise 0.
setGlue :: GlueStatus -> Glue -> SetGlue
setGlue st g@Glue{dimen = d} = SetGlue $ d + glueDiff st g
  where
    glueDiff :: GlueStatus -> Glue -> Int
    glueDiff s Glue{stretch, shrink} = case s of
        NaturallyGood     -> 0
        -- Note: I made this logic up. Take a 'do your best' approach. This
        -- shouldn't matter anyway, because unfixably bare implies there's no
        -- stretchable glue anyway.
        UnfixablyBare     -> round $ factor stretch
        FixablyBad Bare p -> scaleFactor stretch p
        -- Full or overfull
        FixablyBad _ p    -> -(scaleFactor shrink p)

    scaleFactor GlueFlex{factor = f, order = gO} FixParams{ratio, setOrder}
        | setOrder == gO = round (ratio * f)
        | otherwise = 0
