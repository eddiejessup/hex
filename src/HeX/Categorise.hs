module HeX.Categorise where

import           HeXlude

import qualified HeX.Config.Codes as Code
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Lazy as BS.L

data CharCat = CharCat
    { char :: Code.CharCode
    , cat  :: Code.CatCode
    } deriving (Show)

instance Readable CharCat where
  describe (CharCat c ct) = show ct <> " " <> describe c

extractCharCatNaive
    :: (Code.CharCode -> Code.CatCode) -> Code.CharCode -> CharCat
extractCharCatNaive charToCat n1 =
    CharCat n1 (charToCat n1)

-- extractCharCat
--   :: (Code.CharCode -> Code.CatCode) -> Seq Code.CharCode -> Maybe (CharCat, Seq Code.CharCode)
-- extractCharCat charToCat = \case
--     Empty ->
--         Nothing
--     n1 :<| n2 :<| n3 :<| rest ->
--         -- Next two characters must be identical, and have category
--         -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
--         let cat1 = charToCat n1
--             n3Triod = toEnum $ fromEnum n3 + if fromEnum n3 < 64 then 64 else (-64)
--         in
--             Just $
--                 if (cat1 == Code.CoreCatCode Code.Superscript) && (n1 == n2) && (charToCat n3 /= Code.EndOfLine)
--                     then (CharCat n3Triod $ charToCat n3Triod, rest)
--                     else (CharCat n1 cat1, n2 :<| n3 :<| rest)
--     n1 :<| rest ->
--         Just (CharCat n1 $ charToCat n1, rest)

extractCharCatBSLike
  :: (Code.CharCode -> Code.CatCode) -> Seq Code.CharCode -> Maybe (CharCat, Seq Code.CharCode)
extractCharCatBSLike charToCat xs =
    case xs of
        Empty -> Nothing
        n1 :<| rest1 ->
            -- Next two characters must be identical, and have category
            -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
            let cat1 = charToCat n1
                normal = (CharCat n1 cat1, rest1)
            in Just $ case cat1 of
                Code.CoreCatCode Code.Superscript ->
                    case rest1 of
                        n2 :<| rest2 | n1 == n2 ->
                            case rest2 of
                                n3 :<| rest3 ->
                                    case charToCat n3 of
                                        Code.EndOfLine ->
                                            normal
                                        _ ->
                                            let n3Triod = if n3 < 64 then n3 + 64 else n3 - 64
                                            in (CharCat n3Triod (charToCat n3Triod), rest3)
                                Empty ->
                                    normal
                        _ ->
                            normal
                _ ->
                    normal

extractCharCatBS
  :: Code.CatCodes -> BS.ByteString -> Maybe (CharCat, BS.ByteString)
extractCharCatBS cats xs =
    do
    (n1, rest1) <- BS.uncons xs
    -- Next two characters must be identical, and have category
    -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
    let cat1 = Code.catLookup cats (Code.CharCode n1)
        normal = (CharCat (Code.CharCode n1) cat1, rest1)
    pure $ case cat1 of
        Code.CoreCatCode Code.Superscript ->
            case BS.uncons rest1 of
                Just (n2, rest2) | n1 == n2 ->
                    case BS.uncons rest2 of
                        Just (n3, rest3) ->
                            case Code.catLookup cats (Code.CharCode n3) of
                                Code.EndOfLine ->
                                    normal
                                _ ->
                                    let n3Triod = Code.CharCode $ if n3 < 64 then n3 + 64 else n3 - 64
                                    in (CharCat n3Triod (Code.catLookup cats n3Triod), rest3)
                        Nothing ->
                            normal
                _ ->
                    normal
        _ ->
            normal

extractCharCatBSL
  :: (Code.CharCode -> Code.CatCode) -> BS.L.ByteString -> Maybe (CharCat, BS.L.ByteString)
extractCharCatBSL charToCat xs =
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
