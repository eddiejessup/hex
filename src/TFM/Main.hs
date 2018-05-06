module TFM.Main where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.Binary.Strict.Get as BSG
import qualified Data.Word as W
import qualified Data.Map as M
import Data.List.Split (chunksOf)
import Data.Bits ((.&.), shift)
import Data.Maybe (fromJust)
import qualified Control.Monad as CM

import qualified TFM.Parse as TFMP
import qualified TFM.Character as TFMC

data TexFont = TexFont { meta :: TFMP.TFM
                       , headers :: TFMP.Headers
                       , fontParams :: TFMP.FontParams
                       , ligKerns :: [Either String TFMP.LigKernInstr]
                       , characters :: [Either String TFMC.Character] }
             deriving (Show)

contentsToTFM :: BS.ByteString -> Either String TexFont
contentsToTFM contents = do
    meta <- fst $ BSG.runGet TFMP.newTFM contents
    headers <- fst $ BSG.runGet (TFMP.readHeader meta) contents
    fontParams1 <- fst $ BSG.runGet (TFMP.readFontParams meta headers) contents
    fontParams <- fontParams1
    ligKerns <- TFMP.readLigKerns meta contents
    let
        characters = TFMC.readCharInfos meta contents
    return TexFont { meta=meta
                   , headers=headers
                   , fontParams=fontParams
                   , ligKerns=ligKerns
                   , characters=characters }

readTFM :: String -> IO (Either String TexFont)
readTFM path = do
    contents <- BS.readFile path
    return $ contentsToTFM contents
