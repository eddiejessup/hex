module TFM.Parse
  ( TexFont(..)
  , newTFM
  )
where

import HeXlude

import           Data.Ascii                     ( Ascii )
import qualified Data.Binary.Get               as B.G
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BS.L
import           Data.HashMap.Strict

import           TFM.Character
import           TFM.Common
import qualified TFM.FontParams as F
import qualified TFM.Header as H
import           TFM.LigKern
import           TFM.Recipe
import qualified TFM.Table as T

data TexFont = TexFont
    { checksum              :: Int
    , designFontSize        :: Rational
    , characterCodingScheme
    , family                :: Maybe Ascii
    , slant
    , spacing
    , spaceStretch
    , spaceShrink
    , xHeight
    , quad
    , extraSpace            :: Rational
    , extraParams           :: Maybe F.ExtraFontParams
    , ligKerns              :: [LigKernInstr]
    , characters            :: HashMap Char Character
    }

runGetEith :: Text -> B.G.Get b -> BS.ByteString -> Either Text b
runGetEith ctx f s = case B.G.runGetOrFail f (BS.L.fromStrict s) of
        Left (_, _, err) -> Left $ "In " <> showT ctx <> ": " <> toS err
        Right (_, _, v) -> Right v

newTFM :: BS.ByteString -> Either Text TexFont
newTFM contents =
    do
    tableParams <- runGetEith "tableParams" T.getTableParams contents

    let
        runGetEithTable :: Text -> B.G.Get b -> T.Table -> Either Text b
        runGetEithTable s f tbl = runGetEith s f (T.tableToString tableParams tbl)

    header <- runGetEithTable "header" H.getHeader T.Header
    charInfos <- runGetEithTable "charInfos" (getChunks getCharInfo) T.CharacterInfo
    widths <- runGetEithTable "widths" (getChunks getFixWord) T.Width
    heights <- runGetEithTable "heights" (getChunks getFixWord) T.Height
    depths <- runGetEithTable "depths" (getChunks getFixWord) T.Depth
    italicCorrs <- runGetEithTable "italicCorrs" (getChunks getFixWord) T.ItalicCorrection
    ligKernCommands <- runGetEithTable "ligKernCommands" (getChunks getLigKernCommand) T.LigKern
    kerns <- runGetEithTable "kerns" (getChunks getFixWord) T.Kern
    recipes <- runGetEithTable "recipes" (getChunks getExtensibleRecipe) T.ExtensibleRecipe
    params <- runGetEithTable "params" (F.getFontParams (H.characterCodingScheme header)) T.FontParameter

    ligKernInstrs <- mapM (readLigKern kerns) ligKernCommands
    chars <- readCharacters (T.smallestCharCode tableParams) charInfos recipes widths heights depths italicCorrs

    pure TexFont
        { checksum              = H.checksum header
        , designFontSize        = H.designFontSize header
        , characterCodingScheme = H.characterCodingScheme header
        , family                = H.family header
        , slant                 = F.slant params
        , spacing               = F.spacing params
        , spaceStretch          = F.spaceStretch params
        , spaceShrink           = F.spaceShrink params
        , xHeight               = F.xHeight params
        , quad                  = F.quad params
        , extraSpace            = F.extraSpace params
        , extraParams           = F.extraParams params
        , ligKerns              = ligKernInstrs
        , characters            = chars
        }
