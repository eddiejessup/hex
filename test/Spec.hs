import Test.Hspec

import qualified Data.ByteString.Lazy as BS
import Data.List.Utils ( replace )
import System.Directory ( listDirectory )

import           HeX.Run                        ( codesToDVIBytes )

cmpFiles :: FilePath -> IO ()
cmpFiles fname =
    do
    inp <- readFile $ "test/input/" ++ fname
    outp <- codesToDVIBytes inp
    outpValid <- BS.readFile $ "test/output/" ++ replace ".tex" ".dvi" fname
    outp `shouldBe` outpValid

main :: IO ()
main = hspec $
    describe "HeX" $
        it "turns TeX into DVI" $
            do
            inpFnames <- (listDirectory "test/input")
            mapM_ cmpFiles inpFnames
