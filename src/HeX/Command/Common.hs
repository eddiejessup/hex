{-# LANGUAGE RankNTypes #-}

module Hex.Command.Common where

import           Hexlude

import           Hex.Config
import           Hex.Evaluate
import           Hex.Quantity
import qualified Hex.Parse                 as HP

readOnState
    :: ( MonadState r m
       )
     => ReaderT r m b
     -> m b
readOnState f = get >>= runReaderT f

readOnConfState
    :: ( MonadState st m
       , HP.HasTgtType st
       , HP.TeXStream (HP.Tgt st)
       )
    => ReaderT Config (StateT Config m) a
    -> m a
readOnConfState f = HP.runConfState $ readOnState f

modConfState
    :: ( MonadState st m
       , HP.HasTgtType st
       , HP.TeXStream (HP.Tgt st)
       )
    => (Config -> Config)
    -> m ()
modConfState x = HP.runConfState $ modify x

evalOnConfState
    :: ( TeXEvaluable v

       , MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e

       , MonadState st m
       , HP.HasTgtType st
       , HP.TeXStream (HP.Tgt st)
       )
    => v
    -> m (EvalTarget v)
evalOnConfState v = readOnConfState $ texEvaluate v

data BoxModeIntent
    = IntentToAddBox
    | IntentToSetBoxRegister EightBitInt HP.GlobalFlag
    deriving stock (Show)


data RecursionResult a b
    = LoopAgain a
    | EndLoop b

addMaybeElem :: Seq a -> Maybe a -> Seq a
addMaybeElem a mayE = case mayE of
    Nothing -> a
    Just e -> a :|> e

runLoop :: Monad m => (a -> m (RecursionResult a b)) -> a -> m b
runLoop f = go
  where
    go state_ =
        f state_ >>= \case
            LoopAgain newState ->
                go newState
            EndLoop result ->
                pure result

runCommandLoop
    :: ( HP.TeXParseable (HP.Tgt st) e m
       , MonadState st m
       , HP.HasTgtType st
       )
    => (a -> HP.Command -> HP.Tgt st -> m (RecursionResult a r))
    -> a
    -> m r
runCommandLoop f = runLoop g
  where
    g elemList =
        do
        oldStream <- gets $ view HP.tgtLens
        (newStream, command) <- HP.runSimpleRunParserT' HP.parseCommand oldStream
        modify $ HP.tgtLens .~ newStream
        -- liftIO $ putText $ describe command
        f elemList command oldStream
