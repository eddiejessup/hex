{-# LANGUAGE UndecidableInstances #-}

module Hex.Build.ListBuilderT where

import           Hexlude

import           Control.Monad.Trans.Class

import qualified Data.Sequence               as Seq
import qualified Data.Path

import           TFM                         (TFMError)

import qualified Hex.Box                     as B
import           Hex.BreakList               (HList(..), VList(..))
import qualified Hex.BreakList               as BL
import           Hex.Build.Class
import           Hex.Build.Command
import           Hex.Build.Helpers
import           Hex.Config
import qualified Hex.Lex                     as Lex
import qualified Hex.Parse                   as HP

newtype ListBuilderT s l m a
  = ListBuilderT {unListBuilderT :: s -> s -> l -> m (s, s, l, a)}

instance Functor m => Functor (ListBuilderT s l m) where
    fmap g (ListBuilderT f) = ListBuilderT $ \s0 s1 l ->
        f s0 s1 l <&> (\(s0_, s1_, l_, a_) -> (s0_, s1_, l_, g a_))

instance Monad m => Applicative (ListBuilderT s l m) where
  pure a = ListBuilderT $ \s0 s1 l -> pure (s0, s1, l, a)

  -- (<*>) :: m (a -> b) -> m a -> m b
  (ListBuilderT fAToB) <*> (ListBuilderT fA) = ListBuilderT $ \s0 s1 l -> do
    (s0_, s1_, l_, aToB) <- fAToB s0 s1 l
    (s0__, s1__, l__, a) <- fA s0_ s1_ l_
    pure (s0__, s1__, l__, aToB a)

instance Monad m => Monad (ListBuilderT s l m) where
  return = pure

  -- m a -> (a -> m b) -> m b
  (ListBuilderT fA) >>= aToTFB = ListBuilderT $ \s0 s1 l -> do
    (s0_, s1_, l_, a) <- fA s0 s1 l
    let (ListBuilderT fB) = aToTFB a
    fB s0_ s1_ l_

instance MonadTrans (ListBuilderT s l) where
  lift m = ListBuilderT $ \s0 s1 l -> do
    a <- m
    pure (s0, s1, l, a)

instance (MonadError e m) => MonadError e (ListBuilderT s l m) where
  throwError = lift . throwError

  catchError (ListBuilderT f) errToHandle = ListBuilderT $ \s0 s1 l ->
    catchError (f s0 s1 l) $ \e -> do
      let (ListBuilderT fRecover) = errToHandle e
      fRecover s0 s1 l

instance MonadState st m => MonadState st (ListBuilderT s l m) where
  get = lift get
  put = lift . put

instance MonadIO m => MonadIO (ListBuilderT s l m) where
  liftIO = lift . liftIO

instance MonadSlog m => MonadSlog (ListBuilderT s l m) where
  sLog = lift . sLog

  sTime = lift sTime

instance
  ( MonadError e m
  , AsType BuildError e
  , AsType TFMError e
  , AsType Data.Path.PathError e

  , TeXBuildCtx st e m

  , HP.MonadTeXParse (HP.TeXParseT s m)
  , HP.TeXStream s

  , MonadIO m
  , MonadSlog m
  ) => MonadModeIndependentBuild (ListBuilderT s HList m) where
    addVElem e = addHElem (BL.HVListElem e)

    insertLexToken = insertLexTokenImpl

    extractExplicitBox = extractExplicitBoxImpl

    revertStream = revertStreamImpl

instance
  ( MonadError e m
  , TeXBuildCtx st e m
  , MonadIO m
  ) => MonadHModeBuild (ListBuilderT s HList m) where
    addHElem e =
      ListBuilderT $ \s0 s1 (HList hElemSeq) ->
        let newLst = HList $ hElemSeq :|> e
        in pure (s0, s1, newLst, ())

instance
  ( MonadError e m
  , AsType BuildError e
  , AsType TFMError e
  , AsType Data.Path.PathError e

  , TeXBuildCtx st e m

  , HP.MonadTeXParse (HP.TeXParseT s m)
  , HP.TeXStream s

  , MonadIO m
  , MonadSlog m
  ) => MonadModeIndependentBuild (ListBuilderT s VList m) where
    addVElem e =
      ListBuilderT $ \s0 s1 list_ -> do
        list__ <- addVListElem list_ e
        pure (s0, s1, list__, ())

    insertLexToken = insertLexTokenImpl

    extractExplicitBox = extractExplicitBoxImpl

    revertStream = revertStreamImpl

instance
  ( MonadError e m
  , AsType BuildError e
  , AsType TFMError e
  , AsType Data.Path.PathError e

  , TeXBuildCtx st e m

  , HP.MonadTeXParse (HP.TeXParseT s m)
  , HP.TeXStream s

  , MonadIO m
  , MonadSlog m
  ) => MonadVModeBuild (ListBuilderT s VList m) where
    extractParaList indentFlag =
        ListBuilderT $ \s0 s1 list_ -> do
          (s1_, hList, endReason) <- extractParaListImpl indentFlag s1
          pure (s0, s1_, list_, (hList, endReason))

insertLexTokenImpl
  :: ( Applicative m
     , HP.TeXStream s
     )
  => Lex.Token
  -> ListBuilderT s l m ()
insertLexTokenImpl lt =
  ListBuilderT $ \s0 s1 list_ ->
    pure (s0, HP.insertLexToken s1 lt, list_, ())

extractExplicitBoxImpl
  :: ( MonadError e m
     , AsType BuildError e
     , AsType TFMError e
     , AsType Data.Path.PathError e

     , TeXBuildCtx st e m

     , HP.MonadTeXParse (HP.TeXParseT s m)
     , HP.TeXStream s

     , MonadIO m
     , MonadSlog m
     )
  => B.DesiredLength
  -> HP.ExplicitBox
  -> ListBuilderT s l m (B.Box B.BoxContents)
extractExplicitBoxImpl desiredLength boxType =
  ListBuilderT $ \s0 s1 list_ -> do
    (s1_, box) <- extractExplicitBoxContentsImpl s1 desiredLength boxType
    pure (s0, s1_, list_, box)

extractExplicitBoxContentsImpl
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
       )
    => s
    -> B.DesiredLength
    -> HP.ExplicitBox
    -> m (s, B.Box B.BoxContents)
extractExplicitBoxContentsImpl s desiredLength = \case
    HP.ExplicitHBox ->
        do
        let handleCommand oldS newS hList cmd = do
              (_, doneS, doneList, mayEndUnit) <- unListBuilderT (handleCommandInHBoxMode cmd) oldS newS hList
              pure (doneS, doneList, mayEndUnit)
        (doneS, finalHList, ()) <- runCommandLoop handleCommand s mempty
        pure (doneS, B.HBoxContents <$> BL.setHList finalHList (BL.UncomputedTargetLength desiredLength))
    HP.ExplicitVBox vAlignType ->
        do
        let handleCommand oldS newS vList cmd = do
              (_, doneS, doneList, mayEndUnit) <- unListBuilderT (handleCommandInVBoxMode cmd) oldS newS vList
              pure (doneS, doneList, mayEndUnit)
        (doneS, finalVList, ()) <- runCommandLoop handleCommand s mempty
        pure (doneS, B.VBoxContents <$> BL.setVList finalVList desiredLength vAlignType)

revertStreamImpl
  :: (Applicative m)
  => ListBuilderT s l m ()
revertStreamImpl =
  ListBuilderT $ \s0 _s1 list_ ->
    pure (s0, s0, list_, ())

extractParaListImpl
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
       )
    => HP.IndentFlag
    -> s
    -> m (s, HList, EndParaReason)
extractParaListImpl indentFlag s = do
    initList <- case indentFlag of
        HP.Indent -> do
            indentBox <- gets (view $ typed @Config % to parIndentBox)
            pure $ Seq.singleton indentBox
        HP.DoNotIndent ->
            pure mempty
    let handleCommand oldS newS hList cmd = do
          (_, doneS, doneList, mayEndParaReason) <- unListBuilderT (handleCommandInParaMode cmd) oldS newS hList
          pure (doneS, doneList, mayEndParaReason)

    runCommandLoop handleCommand s (HList initList)

extractBreakAndSetMainVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
       )
    => s
    -> m (s, Seq B.Page, IntParamVal 'HP.Mag)
extractBreakAndSetMainVList s = do
    (finalS, finalMainVList) <- extractMainVList s
    sLog "In extractBreakAndSetMainVList, done extractMainVList"
    case finalS ^. HP.conditionBodyStateLens of
        [] -> pure ()
        condStates -> throwError $ injectTyped $ BuildError $ "Cannot end: in condition block: " <> show condStates
    join $ gets $ view $ typed @Config % to finaliseConfig
    desiredH <- gets $ view $ typed @Config % to (LenParamVal . lookupLengthParameter HP.VSize)
    let pages = BL.runPageBuilder desiredH BL.newCurrentPage finalMainVList
    mag <- gets $ view $ typed @Config % to (IntParamVal . lookupTeXIntParameter HP.Mag)
    pure (finalS, pages, mag)

extractMainVList
    :: ( MonadError e m
       , AsType BuildError e
       , AsType TFMError e
       , AsType Data.Path.PathError e

       , TeXBuildCtx st e m

       , HP.MonadTeXParse (HP.TeXParseT s m)
       , HP.TeXStream s

       , MonadIO m
       , MonadSlog m
       )
    => s
    -> m (s, VList)
extractMainVList s = do
    let handleCommand oldS newS hList cmd = do
          (_, doneS, doneList, mayEndUnit) <- unListBuilderT (handleCommandInMainVMode cmd) oldS newS hList
          pure (doneS, doneList, mayEndUnit)
    (doneS, vList, _) <- runCommandLoop handleCommand s mempty
    sLog "In extractMainVList, done runCommandLoop handleCommandInMainVMode"
    pure (doneS, vList)
