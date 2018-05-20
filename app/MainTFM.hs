module Main where

import qualified Data.ByteString as BS
import Data.Binary.Strict.Get as BSG
import qualified TFM.Parse as TFMP
import qualified TFM.Character as TFMC

main = do
    contents <- BS.readFile "cmr10.tfm"
    let foo = BSG.runGet TFMP.newTFM contents
    let (Right tfm, _) = BSG.runGet TFMP.newTFM contents
    let (Right headers, _) = BSG.runGet (TFMP.readHeader tfm) contents
    let (fontParams, _) = BSG.runGet (TFMP.readFontParams tfm headers) contents
    let (Right ligKerns) = TFMP.readLigKerns tfm contents
    let characters = TFMC.readCharInfos tfm contents
    print headers
    print fontParams
    -- print ligKerns
    -- print characters
    -- print $ TFMP.tableLength tfm TFMP.Kern
    -- print $ TFMP.tablePointerPos tfm TFMP.Kern
    return ()
