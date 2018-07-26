{-# LANGUAGE DuplicateRecordFields #-}

module TFM.Character where

import           Data.Bits          (shiftR, (.&.))
import qualified Data.ByteString.Lazy    as BLS
import qualified Data.HashMap.Strict as HashMap
import qualified TFM.Parse          as TFMP

-- The character info array contains, for each character, six fields packed
-- into four bytes:

-- * 8 bits: width_index
-- * 4 bits: height_index * 16
-- * 4 bits: depth_index
-- * 6 bits: italic_index * 4
-- * 2 bits: tag
-- * 8 bits: remainder

-- The width of a character is 'width[width_index]', in design-size units. The
-- TFM format provides up to 16 heights, 16 depths, and 64 italic corrections.

-- Incidentally, 'width[0] = height[0] = depth[0] = italic[0] = 0' should
-- always hold, so that an index of zero implies a value of zero. The width
-- index should never be zero unless the character is not in the font, since a
-- character is valid if and only if it lies between 'lowest_char_code' and
-- 'upper_char_code' and has a nonzero width index.

-- The tag field has four values indicating how to interpret the remainder:
-- * 0: 'no_tag': a vanilla character; the remainder is unused
-- * 1, 'lig_tag': the character has a ligature/kerning program starting at
--   'lig_kern[remainder]'
-- * 2, 'list_tag': this character is part of a chain of characters of
--   ascending sizes, and not the largest in the chain. The remainder field
--   gives the character code of the next larger character.
-- * 3, 'ext_tag': this character code represents an extensible character, that
--   is, a character that is built from smaller pieces so that it can be made
--   arbitrarily large. The pieces are specified in 'exten[remainder]'.
-- """

data Character = Character { code             :: Int
                           , width            :: Rational
                           , height           :: Rational
                           , depth            :: Rational
                           , italicCorrection :: Rational
                           , special          :: Maybe CharacterSpecial
                           } deriving (Show)

data CharacterSpecial = ExtensibleRecipe { top      :: Int
                                           , middle :: Int
                                           , bottom :: Int
                                           , repeater :: Int }
                      | LigKernIndex { index :: Int }
                      | NextLargerChar { code :: Int }
                      deriving (Show)

data Tag = Plain | LigKern | Chain | Extensible deriving (Enum, Ord, Eq, Show)

readCharInfo :: TFMP.TFM -> BLS.ByteString -> Int -> Character
readCharInfo tfm contents _code =
    let
      charIdx = _code - TFMP.smallestCharCode tfm
      (widthIdx, heightDepthByte, italicTagByte, remainder) = TFMP.get4Word8IntsAt tfm contents TFMP.CharacterInfo charIdx
      heightIdx = heightDepthByte `shiftR` 4
      depthIdx = heightDepthByte .&. 0xF
      italicIdx = italicTagByte `shiftR` 6
      tag = italicTagByte .&. 0x3
      -- Get a dimension from some dimension table, at some index
      getDim tbl i = if i == 0 then 0 else TFMP.getFixWordAt tfm contents tbl i
      _width = getDim TFMP.Width widthIdx
      _height = getDim TFMP.Height heightIdx
      _depth = getDim TFMP.Depth depthIdx
      _italicCorrection = getDim TFMP.ItalicCorrection italicIdx
    -- If the character is special, get its particular extra attributes.
      _special = case toEnum tag of
        Plain -> Nothing
        LigKern -> Just LigKernIndex { index=remainder }
        Chain -> Just NextLargerChar { code=remainder }
        Extensible ->
            let (_top, _middle, _bottom, _repeater) = TFMP.get4Word8IntsAt tfm contents TFMP.ExtensibleCharacter remainder
            in Just ExtensibleRecipe { top=_top
                                           , middle=_middle
                                           , bottom=_bottom
                                           , repeater=_repeater }
    in Character { code=_code
                     , width=_width
                     , height=_height
                     , depth=_depth
                     , italicCorrection=_italicCorrection
                     , special=_special }

readCharInfos :: TFMP.TFM -> BLS.ByteString -> (HashMap.HashMap Int Character)
readCharInfos tfm contents =
  let charList = fmap (readCharInfo tfm contents) [TFMP.smallestCharCode tfm..TFMP.largestCharCode tfm]
  in HashMap.fromList $ (\c@Character{code=i} -> (i, c)) <$> charList
