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
import           Hex.Evaluate
import qualified Hex.Lex                     as Lex
import qualified Hex.Parse.TokenParser.Class as P
import qualified Hex.Parse.Stream.Class as S
import qualified Hex.Resolve.Token as Tok

newtype ListBuilderT l m a
  = ListBuilderT {unListBuilderT :: StateT l m a}
  deriving newtype (Functor, Applicative, Monad)

runListBuilderT = runStateT . unListBuilderT

-- instance (MonadError e m) => MonadError e (ListBuilderT s l m) where
--   throwError = lift . throwError

--   catchError (ListBuilderT f) errToHandle = ListBuilderT $ \s0 s1 l ->
--     catchError (f s0 s1 l) $ \e -> do
--       let (ListBuilderT fRecover) = errToHandle e
--       fRecover s0 s1 l

-- instance MonadState st m => MonadState st (ListBuilderT s l m) where
--   get = lift get
--   put = lift . put

-- instance MonadIO m => MonadIO (ListBuilderT s l m) where
--   liftIO = lift . liftIO

-- instance MonadSlog m => MonadSlog (ListBuilderT s l m) where
--   sLog = lift . sLog

--   sTime = lift sTime

-- instance
--   ( MonadError e m
--   , AsType BuildError e
--   , AsType TFMError e
--   , AsType Data.Path.PathError e

--   , TeXBuildCtx st e m

--   , MonadEvaluateCtx e st m

--   , P.MonadTokenParse m

--   , MonadIO m
--   , MonadSlog m
--   ) => MonadModeIndependentBuild (ListBuilderT HList m) where
--     addVElem e = addHElem (BL.HVListElem e)

--     insertLexToken = insertLexTokenImpl

--     extractExplicitBox = extractExplicitBoxImpl

--     revertStream = revertStreamImpl

-- instance
--   ( MonadError e m
--   , TeXBuildCtx st e m
--   , MonadIO m
--   ) => MonadHModeBuild (ListBuilderT HList m) where
--     addHElem e =
--       ListBuilderT $ do
--         modify (\(HList hElemSeq) -> HList $ hElemSeq :|> e)

-- instance
--   ( MonadError e m
--   , AsType BuildError e
--   , AsType TFMError e
--   , AsType Data.Path.PathError e

--   , TeXBuildCtx st e m

--   , MonadEvaluateCtx e st m

--   , P.MonadTokenParse m
--   , S.TeXStream s

--   , MonadIO m
--   , MonadSlog m
--   ) => MonadModeIndependentBuild (ListBuilderT VList m) where
--     addVElem e =
--       ListBuilderT $ do
--         list_ <- get
--         list__ lift $ addVListElem list_ e
--         put list__

--     insertLexToken = insertLexTokenImpl

--     extractExplicitBox = extractExplicitBoxImpl

--     revertStream = revertStreamImpl

-- instance
--   ( MonadError e m
--   , AsType BuildError e
--   , AsType TFMError e
--   , AsType Data.Path.PathError e

--   , TeXBuildCtx st e m

--   , MonadEvaluateCtx e st m


--   , P.MonadTokenParse m
--   , S.TeXStream s

--   , MonadIO m
--   , MonadSlog m
--   ) => MonadVModeBuild (ListBuilderT s VList m) where
--     extractParaList indentFlag =
--         ListBuilderT $ \s0 s1 list_ -> do
--           (s1_, hList, endReason) <- extractParaListImpl indentFlag s1
--           pure (s0, s1_, list_, (hList, endReason))

-- insertLexTokenImpl
--   :: ( P.MonadTokenParse m
--      )
--   => Lex.Token
--   -> ListBuilderT s l m ()
-- insertLexTokenImpl lt =
--   ListBuilderT $ do
--     lift $ P.insertLexTokens (singleton lt)

-- extractExplicitBoxImpl
--   :: ( MonadError e m
--      , AsType BuildError e
--      , AsType TFMError e
--      , AsType Data.Path.PathError e

--      , MonadEvaluateCtx e st m

--      , TeXBuildCtx st e m

--      , P.MonadTokenParse m
--      , S.TeXStream s

--      , MonadIO m
--      , MonadSlog m
--      )
--   => B.DesiredLength
--   -> Tok.ExplicitBox
--   -> ListBuilderT s l m (B.Box B.BoxContents)
-- extractExplicitBoxImpl desiredLength boxType =
--   ListBuilderT $ \s0 s1 list_ -> do
--     (s1_, box) <- extractExplicitBoxContentsImpl s1 desiredLength boxType
--     pure (s0, s1_, list_, box)

-- extractExplicitBoxContentsImpl
--     :: ( MonadError e m
--        , AsType BuildError e
--        , AsType TFMError e
--        , AsType Data.Path.PathError e

--        , MonadEvaluateCtx e st m

--        , TeXBuildCtx st e m

--        , P.MonadTokenParse m
--        , S.TeXStream s

--        , MonadIO m
--        , MonadSlog m
--        )
--     => s
--     -> B.DesiredLength
--     -> Tok.ExplicitBox
--     -> m (s, B.Box B.BoxContents)
-- extractExplicitBoxContentsImpl s desiredLength = \case
--     Tok.ExplicitHBox ->
--         do
--         let handleCommand oldS newS hList cmd = do
--               (_, doneS, doneList, mayEndUnit) <- handleCommandInHBoxMode cmd hList
--               pure (doneS, doneList, mayEndUnit)
--         (doneS, finalHList, ()) <- runCommandLoop handleCommand s mempty
--         pure (doneS, B.HBoxContents <$> BL.setHList finalHList (BL.UncomputedTargetLength desiredLength))
--     Tok.ExplicitVBox vAlignType ->
--         do
--         let handleCommand oldS newS vList cmd = do
--               (_, doneS, doneList, mayEndUnit) <- unListBuilderT (handleCommandInVBoxMode cmd) oldS newS vList
--               pure (doneS, doneList, mayEndUnit)
--         (doneS, finalVList, ()) <- runCommandLoop handleCommand s mempty
--         pure (doneS, B.VBoxContents <$> BL.setVList finalVList desiredLength vAlignType)

-- revertStreamImpl
--   :: P.MonadTokenParse m
--   => ListBuilderT s l m ()
-- revertStreamImpl =
--   ListBuilderT $ undefined

-- extractParaListImpl
--     :: ( MonadError e m
--        , AsType BuildError e
--        , AsType TFMError e
--        , AsType Data.Path.PathError e

--        , MonadEvaluateCtx e st m

--        , TeXBuildCtx st e m

--        , P.MonadTokenParse m

--        , MonadIO m
--        , MonadSlog m
--        )
--     => Tok.IndentFlag
--     -> m (HList, EndParaReason)
-- extractParaListImpl indentFlag = do
--     initList <- case indentFlag of
--         Tok.Indent -> do
--             indentBox <- uses (typed @Config) parIndentBox
--             pure $ Seq.singleton indentBox
--         Tok.DoNotIndent ->
--             pure mempty

--     (_, doneS, hList, endReason) <- unListBuilderT (runCommandLoop handleCommandInParaMode) (HList initList) oldS oldS initList
--     pure ()

-- extractBreakAndSetMainVList
--     :: ( MonadError e m
--        , AsType BuildError e
--        , AsType TFMError e
--        , AsType Data.Path.PathError e

--        , TeXBuildCtx st e m

--        , MonadEvaluateCtx e st m

--        , P.MonadTokenParse m
--        , S.TeXStream s

--        , MonadIO m
--        , MonadSlog m
--        )
--     => s
--     -> m (s, Seq B.Page, IntParamVal 'Tok.Mag)
-- extractBreakAndSetMainVList s = do
--     (finalS, finalMainVList) <- extractMainVList s
--     sLog "In extractBreakAndSetMainVList, done extractMainVList"
--     case finalS ^. S.conditionBodyStateLens of
--         [] -> pure ()
--         condStates -> throwError $ injectTyped $ BuildError $ "Cannot end: in condition block: " <> show condStates
--     join $ use $ typed @Config % to finaliseConfig
--     desiredH <- use $ typed @Config % to (LenParamVal . lookupLengthParameter Tok.VSize)
--     let pages = BL.runPageBuilder desiredH BL.newCurrentPage finalMainVList
--     mag <- use $ typed @Config % to (IntParamVal . lookupTeXIntParameter Tok.Mag)
--     pure (finalS, pages, mag)

-- extractMainVList
--     :: ( MonadError e m
--        , AsType BuildError e
--        , AsType TFMError e
--        , AsType Data.Path.PathError e

--        , TeXBuildCtx st e m

--        , MonadEvaluateCtx e st m

--        , P.MonadTokenParse m

--        , MonadIO m
--        , MonadSlog m
--        )
--     => m VList
-- extractMainVList = do
--     let handleCommand oldS hList cmd = do
--           (_, doneS, doneList, mayEndUnit) <- unListBuilderT (handleCommandInMainVMode cmd) oldS hList
--           pure (doneS, doneList, mayEndUnit)
--     (vList, _) <- runCommandLoop handleCommand mempty
--     sLog "In extractMainVList, done runCommandLoop handleCommandInMainVMode"
--     pure vList
