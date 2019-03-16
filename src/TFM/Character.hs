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
import           Data.Bits ( shiftR, (.&.) )
import qualified Data.Binary.Get as B.G
import           Data.Char ( chr )
import           Data.List.Index ( indexed )
import           Data.HashMap.Lazy

import           TFM.Common
import           TFM.Recipe

data Character = Character
    { width, height, depth :: Rational
    , italicCorrection     :: Rational
    , special              :: Maybe CharacterSpecial
    } deriving (Show)

data CharacterSpecial
    = ExtensibleRecipeSpecial ExtensibleRecipe
    | LigKernIndex Int
    | NextLargerChar Int
    deriving (Show)

character
    :: [ExtensibleRecipe]
    -> [Rational]
    -> [Rational]
    -> [Rational]
    -> [Rational]
    -> CharInfo
    -> Character
character recipes widths heights depths italicCorrs charInfo =
    let
        _remainder = remainder charInfo
        _width  = getDim widths $ widthIdx charInfo
        _height = getDim heights $ heightIdx charInfo
        _depth  = getDim depths $ depthIdx charInfo
        _italicCorrection = getDim italicCorrs $ italicIdx charInfo
        -- If the character is special, get its particular extra attributes.
        _special = case tag charInfo of
            Plain      -> Nothing
            LigKern    -> Just $ LigKernIndex _remainder
            Chain      -> Just $ NextLargerChar _remainder
            Extensible -> Just $ ExtensibleRecipeSpecial $ recipes !! _remainder
    in  Character
        { width            = _width
        , height           = _height
        , depth            = _depth
        , italicCorrection = _italicCorrection
        , special          = _special
        }
  where
    -- Get a dimension from some dimension table, at some index
    getDim dims i
        | i == 0 = 0
        | otherwise = dims !! i

readCharacters
    :: Int
    -> [CharInfo]
    -> [ExtensibleRecipe]
    -> [Rational] -> [Rational] -> [Rational] -> [Rational]
    -> HashMap Char Character
readCharacters _minCode charInfos recipes widths heights depths italicCorrs =
    let charList = character recipes widths heights depths italicCorrs <$> charInfos
    in  fromList $ fmap (\(idx, c) -> (chr $ idx + _minCode, c)) $ indexed charList

data CharInfo = CharInfo
    { widthIdx
    , heightIdx
    , depthIdx
    , italicIdx :: Int
    , tag :: Tag
    , remainder :: Int
    } deriving Show

data Tag
    = Plain
    | LigKern
    | Chain
    | Extensible
    deriving (Enum, Ord, Eq, Show)

getCharInfo :: B.G.Get CharInfo
getCharInfo =
    do
    _widthIdx <- getWord8Int
    heightDepthByte <- getWord8Int
    italicTagByte <- getWord8Int
    _remainder <- getWord8Int
    pure $ CharInfo
        { widthIdx = _widthIdx
        , heightIdx = heightDepthByte `shiftR` 4
        , depthIdx = heightDepthByte .&. 0xF
        , italicIdx = italicTagByte `shiftR` 6
        , tag = toEnum $ italicTagByte .&. 0x3
        , remainder = _remainder }
