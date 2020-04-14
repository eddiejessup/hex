module TFM.Character where

import           Hexlude

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
import qualified Data.Binary.Get    as B.G
import           Data.IntMap.Strict
import           Data.List.Index    (indexed)

import           TFM.Common
import           TFM.Recipe

data Character = Character
    { width, height, depth
    , italicCorrection :: Rational
    , special          :: Maybe CharacterSpecial
    } deriving (Show)

data CharacterSpecial
    = ExtensibleRecipeSpecial ExtensibleRecipe
    | LigKernIndex Int
    | NextLargerChar Int
    deriving (Show)

character
    :: forall m
     . MonadError Text m
    => [ExtensibleRecipe]
    -> [Rational]
    -> [Rational]
    -> [Rational]
    -> [Rational]
    -> CharInfo
    -> m Character
character recipes widths heights depths italicCorrs charInfo =
    do
    width  <- dimAtEith "width" widths $ widthIdx charInfo
    height <- dimAtEith "height" heights $ heightIdx charInfo
    depth  <- dimAtEith "depth" depths $ depthIdx charInfo
    italicCorrection <- dimAtEith "italic correction" italicCorrs $ italicIdx charInfo
    let _remainder = remainder charInfo
    -- If the character is special, get its particular extra attributes.
    special <- case tag charInfo of
            Plain      -> pure Nothing
            LigKern    -> pure $ Just $ LigKernIndex _remainder
            Chain      -> pure $ Just $ NextLargerChar _remainder
            Extensible ->
                do
                recipe <- atEith "recipe" recipes _remainder
                pure $ Just $ ExtensibleRecipeSpecial recipe
    pure Character
        { width
        , height
        , depth
        , italicCorrection
        , special
        }
  where
    -- Get a dimension from some dimension table, at some index
    dimAtEith :: Text -> [Rational] -> Int -> m Rational
    dimAtEith str xs i
        | i == 0 = pure 0
        | otherwise = atEith str xs i

readCharacters
    :: MonadError Text m
    => Int
    -> [CharInfo]
    -> [ExtensibleRecipe]
    -> [Rational] -> [Rational] -> [Rational] -> [Rational]
    -> m (IntMap Character)
readCharacters _minCode charInfos recipes widths heights depths italicCorrs =
    do
    charList <- mapM (character recipes widths heights depths italicCorrs) charInfos
    pure $ fromList $ (\(idx, c) -> (idx + _minCode, c)) <$> indexed charList

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
    pure CharInfo
        { widthIdx = _widthIdx
        , heightIdx = heightDepthByte `shiftR` 4
        , depthIdx = heightDepthByte .&. 0xF
        , italicIdx = italicTagByte `shiftR` 6
        , tag = toEnum $ italicTagByte .&. 0x3
        , remainder = _remainder
        }
