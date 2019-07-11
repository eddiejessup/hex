{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module HeX.Parse.Parser where

import           HeXlude          hiding  (many)

newtype SParser s m a = SParser { runParser :: s -> m (s, a) }

mapSnd :: (t -> b) -> (a, t) -> (a, b)
mapSnd f (s, v) = (s, f v)

instance Functor m => Functor (SParser s m) where
    fmap f (SParser parse) = SParser go
      where
        go stream = parse stream <&> mapSnd f

instance Monad m => Applicative (SParser s m) where
    pure v = SParser $ \s -> pure (s, v)

    (SParser parseFunc) <*> (SParser parseArg) = SParser go
      where
        go stream =
            do
            (postFuncStream, func) <- parseFunc stream
            parseArg postFuncStream <&> mapSnd func

instance Monad m => Monad (SParser s m) where
    -- m a -> (a -> m b) -> m b
    (SParser parseA) >>= next = SParser go
      where
        go stream =
            do
            (postAStream, a) <- parseA stream
            runParser (next a) postAStream

throwParseError :: MonadError e m => e -> SParser s m a
throwParseError e = SParser $ const $ throwError e
