import Test.Hspec

import qualified Data.ByteString.Lazy as BS
import Data.List.Utils ( replace )
import System.Directory ( listDirectory )

import           HeX.Run                        ( codesToDVIBytes )

cmpFile :: FilePath -> IO ()
cmpFile fname =
    do
    inp <- readFile $ "test/input/" ++ fname
    outp <- codesToDVIBytes inp
    outpValid <- BS.readFile $ "test/output/" ++ replace ".tex" ".dvi" fname
    outp `shouldBe` outpValid

testFile :: FilePath -> SpecWith ()
testFile fname =
    it ("turns TeX into DVI: " ++ fname) $ cmpFile fname

main :: IO ()
main = do
    inpFnames <- listDirectory "test/input"
    hspec $
        describe "HeX" $
            mapM_ testFile inpFnames
