{-# LANGUAGE DuplicateRecordFields #-}

module TFM.Character where

-- The character info array contains, for each character, six fields packed
-- into four bytes:
-- > 8 bits: width_index
-- > 4 bits: height_index > 16
-- > 4 bits: depth_index
-- > 6 bits: italic_index > 4
-- > 2 bits: tag
-- > 8 bits: remainder
-- The width of a character is 'width[width_index]', in design-size units. The
-- TFM format provides up to 16 heights, 16 depths, and 64 italic corrections.
-- Incidentally, 'width[0] = height[0] = depth[0] = italic[0] = 0' should
-- always hold, so that an index of zero implies a value of zero. The width
-- index should never be zero unless the character is not in the font, since a
-- character is valid if and only if it lies between 'lowest_char_code' and
-- 'upper_char_code' and has a nonzero width index.
-- The tag field has four values indicating how to interpret the remainder:
-- > 0: 'no_tag': a vanilla character; the remainder is unused
-- > 1, 'lig_tag': the character has a ligature/kerning program starting at
--   'lig_kern[remainder]'
-- > 2, 'list_tag': this character is part of a chain of characters of
--   ascending sizes, and not the largest in the chain. The remainder field
--   gives the character code of the next larger character.
-- > 3, 'ext_tag': this character code represents an extensible character, that
--   is, a character that is built from smaller pieces so that it can be made
--   arbitrarily large. The pieces are specified in 'exten[remainder]'.
import           Data.Bits
import           Data.ByteString
import           Data.HashMap.Lazy

import           TFM.Common

data Character = Character
    { code                 :: Char
    , width, height, depth :: Rational
    , italicCorrection     :: Rational
    , special              :: Maybe CharacterSpecial
    } deriving (Show)

data ExtensibleRecipe = ExtensibleRecipe
    { top, middle, bottom :: Int
    , repeater            :: Int
    } deriving (Show)

data CharacterSpecial
    = ExtensibleRecipeSpecial ExtensibleRecipe
    | LigKernIndex Int
    | NextLargerChar Int
    deriving (Show)

data Tag
    = Plain
    | LigKern
    | Chain
    | Extensible
    deriving (Enum, Ord, Eq, Show)

readCharInfo
    :: Int
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> Int
    -> Character
readCharInfo _smallestCharCode charInfoStr extStr wStr hStr dStr iStr _code =
    let charIdx = _code - _smallestCharCode
        (widthIdx, heightDepthByte, italicTagByte, remainder) =
            runGetAt get4Word8Ints charInfoStr charIdx

        heightIdx = heightDepthByte `shiftR` 4
        depthIdx = heightDepthByte .&. 0xF
        italicIdx = italicTagByte `shiftR` 6

        tag = italicTagByte .&. 0x3

        -- Get a dimension from some dimension table, at some index
        getDim str i
          | i == 0 = 0
          | otherwise = runGetAt getFixWord str i

        _width  = getDim wStr widthIdx
        _height = getDim hStr heightIdx
        _depth  = getDim dStr depthIdx

        _italicCorrection = getDim iStr italicIdx
        -- If the character is special, get its particular extra attributes.
        _special =
            case toEnum tag of
                Plain      -> Nothing
                LigKern    -> Just $ LigKernIndex remainder
                Chain      -> Just $ NextLargerChar remainder
                Extensible ->
                    let (_top, _middle, _bottom, _repeater) =
                          runGetAt get4Word8Ints extStr remainder
                    in Just $ ExtensibleRecipeSpecial ExtensibleRecipe
                        { top = _top
                        , middle = _middle
                        , bottom = _bottom
                        , repeater = _repeater
                        }
    in  Character
            { code             = toEnum _code
            , width            = _width
            , height           = _height
            , depth            = _depth
            , italicCorrection = _italicCorrection
            , special          = _special
            }

readCharInfos
    :: Int
    -> Int
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> ByteString
    -> HashMap Char Character
readCharInfos _smallestCharCode _largestCharCode cInfoStr extStr wStr hStr dStr iStr =
    let charList = fmap
            (readCharInfo _smallestCharCode cInfoStr extStr wStr hStr dStr iStr)
            [_smallestCharCode .. _largestCharCode]
    in  fromList $ (\c@Character {code = i} -> (i, c)) <$> charList
