module Hex.Categorise where

import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Combinators (headE)
import qualified Hex.Config.Codes as Code
import Hexlude

data RawCharCat = RawCharCat
  { char :: Code.CharCode,
    cat :: Code.CatCode
  }
  deriving stock (Show, Eq, Generic)

instance Arbitrary RawCharCat where
  arbitrary = genericArbitraryU

instance Describe RawCharCat where
  describe (RawCharCat c ct) =
    [ (0, "RawCharCat")
    ]
      <> describeRel 1 ct
      <> describeRel 1 c

extractCharCat ::
  Monad m => (Code.CharCode -> Code.CatCode) -> ConduitT BS.ByteString RawCharCat m ()
extractCharCat charToCat = go
  where
    -- The first two bytes must be identical, and have category 'Superscript',
    -- and the trio'd character mustn't have category 'EndOfLine'.
    go =
      -- Take the first byte.
      headE >>= \case
        -- If no bytes remain, finish.
        Nothing -> pure ()
        Just n1 -> do
          let code1 = Code.CharCode n1
              cat1 = charToCat code1
              cc1 = RawCharCat code1 cat1
          case cat1 of
            Code.CoreCatCode Code.Superscript ->
              -- If the first byte has category 'Superscript', take another byte.
              headE >>= \case
                -- If there isn't a second byte, emit a char-cat based on the
                -- first byte, and finish.
                Nothing -> do
                  yield cc1
                  pure ()
                Just n2 ->
                  let code2 = Code.CharCode n2
                      cc2 = RawCharCat code2 (charToCat code2)
                   in if n1 == n2
                        then -- If the second byte is equal to the first
                        -- Superscript-category byte, take a third byte.

                          headE >>= \case
                            -- If there isn't a third byte, emit char-cats based on
                            -- the first two bytes as normal, and finish.
                            Nothing -> do
                              yield cc1
                              yield cc2
                              pure ()
                            Just n3 -> do
                              case charToCat (Code.CharCode n3) of
                                Code.EndOfLine -> do
                                  -- If there is a third byte, and it's of category
                                  -- 'EndOfLine', we don't trio this byte, so emit
                                  -- three un-triod char-cats based on the three
                                  -- bytes we've seen, and loop.
                                  let code3 = Code.CharCode n3
                                      cc3 = RawCharCat code3 (charToCat code3)
                                  yield cc1
                                  yield cc2
                                  yield cc3
                                _ -> do
                                  -- If there is a third byte, and its category
                                  -- isn't 'EndOfLine', then emit a single char-cat
                                  -- based on the trioing rules for the third byte,
                                  -- then loop.
                                  let code3Triod = Code.CharCode $ if n3 < 64 then n3 + 64 else n3 - 64
                                  let ccTriod = RawCharCat code3Triod (charToCat code3Triod)
                                  yield ccTriod
                              go
                        else -- If the first and second bytes match, emit two char-cats
                        -- based on those two bytes. Some bytes may remain, so loop.
                        do
                          yield cc1
                          yield cc2
                          go
            _ -> do
              -- If the first byte isn't of category Superscript, emit a char-cat based on that byte, and loop.
              yield cc1
              go

usableExtractCharCat :: Monad m => ConduitT ByteString RawCharCat m ()
usableExtractCharCat = extractCharCat Code.usableCatLookup
