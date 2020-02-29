{-# LANGUAGE StrictData #-}

module HeX.Categorise where

import           HeXlude

import qualified HeX.Config.Codes     as Code
import qualified Data.ByteString.Lazy as BS.L

data CharCat = CharCat
    { char :: Code.CharCode
    , cat  :: Code.CatCode
    } deriving (Show)

instance Readable CharCat where
  describe (CharCat c ct) = show ct <> " " <> describe c

extractCharCat
  :: (Code.CharCode -> Code.CatCode) -> BS.L.ByteString -> Maybe (CharCat, BS.L.ByteString)
extractCharCat charToCat xs =
    do
    (n1, rest1) <- BS.L.uncons xs
    -- Next two characters must be identical, and have category
    -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
    let cat1 = charToCat (Code.CharCode n1)
        normal = (CharCat (Code.CharCode n1) cat1, rest1)
    pure $ case cat1 of
        Code.CoreCatCode Code.Superscript ->
            case BS.L.uncons rest1 of
                Just (n2, rest2) | n1 == n2 ->
                    case BS.L.uncons rest2 of
                        Just (n3, rest3) ->
                            case charToCat (Code.CharCode n3) of
                                Code.EndOfLine ->
                                    normal
                                _ ->
                                    let n3Triod = Code.CharCode $ if n3 < 64 then n3 + 64 else n3 - 64
                                    in (CharCat n3Triod (charToCat n3Triod), rest3)
                        Nothing ->
                            normal
                _ ->
                    normal
        _ ->
            normal
