-- import           Test.Hspec

-- import qualified Prelude
-- import           Protolude

-- import qualified Data.ByteString  as BS
-- import qualified Data.Text        as Text
-- import           System.Directory (listDirectory)

-- import           Hex.Command.Run  (codesToDVIBytes)

-- cmpFile :: Text -> IO ()
-- cmpFile fname =
--     do
--     inp <- Prelude.readFile $ toS $ "test/input/" <> fname
--     outp <- codesToDVIBytes inp
--     outpValid <- BS.readFile $ toS $ "test/output/" <> Text.replace ".tex" ".dvi" fname
--     outp `shouldBe` outpValid

-- testFile :: Text -> SpecWith ()
-- testFile fname =
--     it (toS $ "turns TeX into DVI: " <> fname) $ cmpFile fname

-- main :: IO ()
-- main = do
--     inpFnames <- listDirectory "test/input"
--     hspec $
--         describe "HeX" $
--             mapM_ testFile $ toS <$> inpFnames
