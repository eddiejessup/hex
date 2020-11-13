{-# LANGUAGE UndecidableInstances #-}

module Hex.Parse.TokenParser.ParseT where

import Hex.Parse.TokenParser.Class
import Hexlude hiding (many)
import Control.Monad.Trans (MonadTrans)
import Hex.Resolve.Resolve (ResolutionMode(..))
import Hex.Evaluate (ConditionBodyState)
import Data.Path (MonadInput(..))

data TeXParseState s = TeXParseState { texStream :: s, resolutionMode :: ResolutionMode, skipState :: [ConditionBodyState]}
  deriving stock (Generic)

newtype TeXParseT s m a = TeXParseT {unTeXParseT :: TeXParseState s -> m ((TeXParseState s), Either ParseError a) }

instance Functor m => Functor (TeXParseT s m) where
  fmap f (TeXParseT parse) = TeXParseT $ \s -> parse s <&> \(s', errOrA) ->
    case errOrA of
      Left e -> (s', Left e)
      Right a -> (s', Right (f a))

instance Monad m => Applicative (TeXParseT s m) where
  pure a = TeXParseT $ \s -> pure (s, Right a)

  (<*>) :: TeXParseT s m (a -> b) -> TeXParseT s m a -> TeXParseT s m b
  (TeXParseT parseAToB) <*> (TeXParseT parseA) = TeXParseT $ \s -> do
    (s', errOrAToB) <- parseAToB s
    case errOrAToB of
      Left err ->
        -- s, not s' or s'': Backtracking.
        pure (s, Left err)
      Right aToB -> do
        (s'', errOrA) <- parseA s'
        case errOrA of
          Left err ->
            -- s, not s' or s'': Backtracking.
            pure (s, Left err)
          Right a ->
            pure (s'', Right $ aToB a)

instance Monad m => Monad (TeXParseT s m) where
  return = pure

  (>>=) :: TeXParseT s m a -> (a -> TeXParseT s m b) -> TeXParseT s m b
  (TeXParseT parseA) >>= aToTParseB = TeXParseT $ \s -> do
    (s', errOrA) <- parseA s
    case errOrA of
      Left err ->
        -- s, not s': Backtracking.
        pure (s, Left err)
      Right a ->
        let (TeXParseT parseB) = aToTParseB a
        in parseB s'

instance Monad m => Alternative (TeXParseT s m) where
  empty = mzero

  (<|>) = mplus

instance Monad m => MonadPlus (TeXParseT s m) where
  mzero = TeXParseT $ \s ->
    pure (s, Left ExplicitFailure)

  -- m a -> m a -> m a
  mplus (TeXParseT parseA1) (TeXParseT parseA2) = TeXParseT $ \s -> do
    (s1, errOrA1) <- parseA1 s
    case errOrA1 of
      Left err1 -> do
        -- s, not s1: Backtracking.
        (s2, errOrA2) <- parseA2 s
        case errOrA2 of
          Left _ ->
            -- err1, not err2: Report first failure error
            pure (s1, Left err1)
          Right a2 ->
            pure (s2, Right a2)
      Right a1 ->
        pure (s1, Right a1)

instance MonadTrans (TeXParseT s) where
  lift m = TeXParseT $ \s -> do
    a <- m
    pure (s, Right a)

instance (MonadError e m) => MonadError e (TeXParseT s m) where
  throwError = lift . throwError

  catchError (TeXParseT parseA) errToHandle = TeXParseT $ \s ->
    catchError (parseA s) $ \e -> do
      let (TeXParseT parseRecover) = errToHandle e
      parseRecover s

instance MonadState st m => MonadState st (TeXParseT s m) where
  get = lift get
  put = lift . put

instance MonadIO m => MonadIO (TeXParseT s m) where
  liftIO = lift . liftIO

instance MonadInput m => MonadInput (TeXParseT s m) where
  findPath a b c = lift $ findPath a b c

  readPathBytes a = lift $ readPathBytes a

-- Inhibition.
pWithInhibition :: Monad m => TeXParseT s m a -> TeXParseT s m a
pWithInhibition (TeXParseT parseNow) = TeXParseT $ \st -> do
  let stI = st & field @"resolutionMode" .~ NotResolving
  (st', a) <- parseNow stI
  let st'U = st' & field @"resolutionMode" .~ Resolving
  pure (st'U, a)

runTeXParseT
  :: Monad m
  => TeXParseT s m a
  -> s
  -> m (s, Either ParseError a)
runTeXParseT (TeXParseT f) s = do
  (TeXParseState {texStream}, errOrA) <- f (TeXParseState {texStream = s, resolutionMode = Resolving, skipState = []})
  pure (texStream, errOrA)

runTeXParseTEmbedded
  :: ( MonadError e m
     , AsType ParseError e
     )
  => TeXParseT s m a
  -> s
  -> m (s, a)
runTeXParseTEmbedded p s = do
  (s', errOrV) <- runTeXParseT p s
  case errOrV of
    Left err ->
      throwError $ injectTyped err
    Right v ->
      pure (s', v)
