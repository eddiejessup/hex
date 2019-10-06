module HeX.Categorise where

import           HeXlude

import qualified HeX.Config.Codes as Code

data CharCat = CharCat
    { char :: Code.CharCode
    , cat  :: Code.CatCode
    } deriving (Show)

instance Readable CharCat where
  describe (CharCat c ct) = show ct <> " " <> describe c

extractCharCat
  :: (Code.CharCode -> Code.CatCode) -> Seq Code.CharCode -> Maybe (CharCat, Seq Code.CharCode)
extractCharCat _ Empty =
    Nothing
extractCharCat charToCat (n1 :<| n2 :<| n3 :<| rest) =
    -- Next two characters must be identical, and have category
    -- 'Superscript', and triod character mustn't have category 'EndOfLine'.
    let cat1    = charToCat n1
        n3Triod = toEnum $ fromEnum n3 + if fromEnum n3 < 64 then 64 else (-64)
    in
        Just $
            if (cat1 == Code.CoreCatCode Code.Superscript) && (n1 == n2) && (charToCat n3 /= Code.EndOfLine)
                then (CharCat n3Triod $ charToCat n3Triod, rest)
                else (CharCat n1 cat1, n2 :<| n3 :<| rest)
extractCharCat charToCat (n1 :<| rest) =
    Just (CharCat n1 $ charToCat n1, rest)
