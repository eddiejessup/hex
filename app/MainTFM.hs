module Main where

import qualified Data.ByteString.Lazy as BLS
import Data.Binary.Get as BG
import qualified TFM.Parse as TFMP
import qualified TFM.Character as TFMC

main = do
    contents <- BLS.readFile "cmr10.tfm"
    let
      tfm = BG.runGet TFMP.newTFM $ contents
      headers = BG.runGet (TFMP.readHeader tfm) $ contents
      fontParams = BG.runGet (TFMP.readFontParams tfm headers) $ contents
      ligKerns = TFMP.readLigKerns tfm contents
      characters = TFMC.readCharInfos tfm contents
    print headers
    print fontParams
    -- print ligKerns
    -- print characters
    -- print $ TFMP.tableLength tfm TFMP.Kern
    -- print $ TFMP.tablePointerPos tfm TFMP.Kern
    return ()
