module HeX.Command.Common where

import           HeXlude

import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , throwError
                                                , withExceptT
                                                )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT
                                                , get
                                                , modify
                                                )

import qualified HeX.Box                       as B
import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Parse                     as HP

data BuildError s
  = ParseError s
  | ConfigError Text

newtype MonadBuild s a = MonadBuild { unMonadBuild :: StateT s IO a }
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO)

type BaseExceptMonadBuild e s a = ExceptT e (MonadBuild s) a

type ExceptMonadBuild s a = ExceptT (BuildError (HP.ParseErrorBundle s)) (MonadBuild s) a

liftConfigError :: BaseExceptMonadBuild Text s a -> ExceptMonadBuild s a
liftConfigError = withExceptT ConfigError

liftConfState
    :: HP.InhibitableStream s
    => StateT Config (ExceptT Text (MonadBuild s)) a
    -> ExceptMonadBuild s a
liftConfState x = liftConfigError $ HP.runConfState x

liftReadOnConfState
    :: HP.InhibitableStream s
    => ReaderT Config (StateT Config (ExceptT Text (MonadBuild s))) a
    -> ExceptMonadBuild s a
liftReadOnConfState x = liftConfigError $ readOnConfState x

throwConfigError :: MonadError (BuildError s) m => Text -> m a
throwConfigError s = throwError $ ConfigError s

liftMaybeConfigError :: MonadError (BuildError s) m => Text -> Maybe a -> m a
liftMaybeConfigError s = liftMaybe (ConfigError s)

readOnState :: MonadState r m => ReaderT r m b -> m b
readOnState f = get >>= runReaderT f

readOnConfState
    :: (HP.InhibitableStream s, MonadState s m)
    => ReaderT Config (StateT Config m) a
    -> m a
readOnConfState f = HP.runConfState $ readOnState f

modConfState
    :: (MonadState s m, HP.InhibitableStream s) => (Config -> Config) -> m ()
modConfState x = HP.runConfState $ modify $ x

liftEvalOnConfState
    :: (HP.InhibitableStream s, TeXEvaluable v)
    => v -> ExceptMonadBuild s (EvalTarget v)
liftEvalOnConfState v = liftReadOnConfState $ texEvaluate v

--

data VModeContents = VModeContents VList (Maybe ParaOrBox)
    deriving ( Show )

vModeWithAddedElems :: VList -> VModeContents -> VModeContents
vModeWithAddedElems elems (VModeContents vList maybeChild) =
    VModeContents (elems <> vList) maybeChild

vModeWithSetChild :: ParaOrBox -> VModeContents -> VModeContents
vModeWithSetChild child (VModeContents vList _) =
    VModeContents vList (Just child)

vModeWithoutChild :: VModeContents -> VModeContents
vModeWithoutChild (VModeContents vList _) =
    VModeContents vList Nothing

-- A VList's child is either an HList (for a para or box), or a VList for a vbox.
data ParaOrBox
    = ParaOrBoxPara HModeContents
    | ParaOrBoxBox HOrVBox
    deriving ( Show )

-- Regardless of whether an HList is for a paragraph or an hbox, its child can be either
-- an HList for an hbox, or a VList for a vbox.
data HModeContents = HModeContents HList (Maybe HOrVBox)
    deriving ( Show )

hModeWithAddedElems :: HList -> HModeContents -> HModeContents
hModeWithAddedElems elems (HModeContents hList maybeChild) =
    HModeContents (elems <> hList) maybeChild

hModeWithSetChild :: HOrVBox -> HModeContents -> HModeContents
hModeWithSetChild child (HModeContents hList _) =
    HModeContents hList (Just child)

hModeWithoutChild :: HModeContents -> HModeContents
hModeWithoutChild (HModeContents hList _) =
    HModeContents hList Nothing

data HOrVBox = HOrVBox B.DesiredLength BoxModeIntent HOrVModeContents
    deriving ( Show )

data BoxModeIntent
    = IntentToAddBox
    | IntentToSetBoxRegister EightBitInt HP.GlobalFlag
    deriving ( Show )

data HOrVModeContents
    = HOrVModeContentsH HModeContents
    | HOrVModeContentsV B.VBoxAlignType VModeContents
    deriving ( Show )

emptyHModeContents :: HModeContents
emptyHModeContents = HModeContents [] Nothing

emptyVModeContents :: VModeContents
emptyVModeContents = VModeContents [] Nothing

boxTypeToFreshVChild :: B.DesiredLength -> HP.ExplicitBox -> BoxModeIntent -> ParaOrBox
boxTypeToFreshVChild desiredLength boxType boxIntent =
    ParaOrBoxBox $ boxTypeToFreshHChild desiredLength boxType boxIntent

boxTypeToFreshHChild :: B.DesiredLength -> HP.ExplicitBox -> BoxModeIntent -> HOrVBox
boxTypeToFreshHChild desiredLength boxType boxIntent =
    (HOrVBox desiredLength boxIntent) $ case boxType of
        HP.ExplicitHBox ->
            HOrVModeContentsH emptyHModeContents
        HP.ExplicitVBox alignType ->
            HOrVModeContentsV alignType emptyVModeContents
