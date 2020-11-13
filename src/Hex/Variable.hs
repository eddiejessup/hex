{-# LANGUAGE TypeApplications #-}

module Hex.Variable where

import qualified Hex.BreakList as BL
import Hex.Config
import Hex.Evaluate
import Hex.Quantity
import Hexlude
import qualified Hex.Resolve as HP
import qualified Hex.Parse.AST as HP

class TeXVariable a where

  setValue
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType ConfigError e
       , MonadEvaluate m HP.EightBitTeXInt
       )
    => a
    -> HP.ScopeFlag
    -> EvalTarget a
    -> m ()

setValueFromAST
  :: ( MonadState st m
     , HasType Config st
     , MonadError e m
     , MonadEvaluate m b
     , MonadEvaluate m HP.EightBitTeXInt
     , AsType ConfigError e
     , TeXVariable a
     , EvalTarget a ~ EvalTarget b
     )
  => a
  -> HP.ScopeFlag
  -> b
  -> m ()
setValueFromAST var scopeFlag astVal =
  astEval astVal >>= setValue var scopeFlag

instance TeXVariable HP.TeXIntVariable where

  setValue v scopeFlag tgt = case v of
    HP.ParamVar p ->
      modifying' (typed @Config) $ setTeXIntParameter p tgt scopeFlag
    HP.RegisterVar iRaw ->
      astEval iRaw >>=
        (\i -> modifying' (typed @Config) $ setTeXIntRegister i tgt scopeFlag)

instance TeXVariable HP.LengthVariable where

  setValue v scopeFlag tgt = case v of
    HP.ParamVar p -> modifying' (typed @Config) $ setLengthParameter p tgt scopeFlag
    HP.RegisterVar iRaw ->
      astEval iRaw >>=
        (\i -> modifying' (typed @Config) $ setLengthRegister i tgt scopeFlag)

instance TeXVariable HP.GlueVariable where

  setValue v scopeFlag tgt = case v of
    HP.ParamVar p -> modifying' (typed @Config) $ setGlueParameter p tgt scopeFlag
    HP.RegisterVar iRaw ->
      astEval iRaw >>=
        (\i -> modifying' (typed @Config) $ setGlueRegister i tgt scopeFlag)

instance TeXVariable HP.MathGlueVariable where

  setValue v scopeFlag tgt = case v of
    HP.ParamVar p -> modifying' (typed @Config) $ setMathGlueParameter p tgt scopeFlag
    HP.RegisterVar iRaw ->
      astEval iRaw >>=
        (\i -> modifying' (typed @Config) $ setMathGlueRegister i tgt scopeFlag)

instance TeXVariable HP.TokenListVariable where

  setValue v scopeFlag tgt = case v of
    HP.ParamVar p -> modifying' (typed @Config) $ setTokenListParameter p tgt scopeFlag
    HP.RegisterVar iRaw ->
      astEval iRaw >>=
        (\i -> modifying' (typed @Config) $ setTokenListRegister i tgt scopeFlag)

instance TeXVariable HP.SpecialTeXInt where

  setValue p _ tgt =
    modifying' (typed @Config) $ setSpecialTeXInt p tgt

instance TeXVariable HP.SpecialLength where

  setValue p _ tgt =
    modifying' (typed @Config) $ setSpecialLength p tgt

-- Numeric variables.
class TeXVariable a => TeXNumericVariable a where

  advanceOp :: Proxy a -> EvalTarget a -> EvalTarget a -> EvalTarget a

  scaleUpOp :: Proxy a -> EvalTarget a -> TeXInt -> EvalTarget a

  scaleDownOp :: Proxy a -> EvalTarget a -> TeXInt -> EvalTarget a

  advanceValueFromAST
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType ConfigError e
       , MonadEvaluate m a
       , MonadEvaluate m b
       , MonadEvaluate m HP.EightBitTeXInt
       , EvalTarget b ~ EvalTarget a
       )
    => a
    -> HP.ScopeFlag
    -> b
    -> m ()
  advanceValueFromAST var scopeFlag astPlusVal =
    (advanceOp (Proxy @a) <$> astEval var <*> astEval astPlusVal) >>=
      setValue var scopeFlag

  scaleValueFromAST
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType ConfigError e
       , MonadEvaluate m a
       , MonadEvaluate m HP.TeXInt
       , MonadEvaluate m HP.EightBitTeXInt
       )
    => a
    -> HP.ScopeFlag
    -> VDirection
    -> HP.TeXInt
    -> m ()
  scaleValueFromAST var scopeFlag vDir scaleVal = do
    let op = case vDir of
          Upward -> scaleUpOp
          Downward -> scaleDownOp
    val <- op (Proxy @a) <$> astEval var <*> astEval scaleVal
    setValue var scopeFlag val

instance TeXNumericVariable HP.TeXIntVariable where

  advanceOp _ = (+)

  scaleUpOp _ = (*)

  -- Division of a positive integer by a positive integer
  -- discards the remainder, and the sign of the result
  -- changes if you change the sign of either operand.
  scaleDownOp _ = quot

instance TeXNumericVariable HP.LengthVariable where

  advanceOp _ = (+)

  scaleUpOp _ = scaleLength

  scaleDownOp _ = shrinkLength

instance TeXNumericVariable HP.GlueVariable where

  advanceOp _ = mappend

  scaleUpOp _ = BL.scaleLengthGlue

  scaleDownOp _ = BL.shrinkLengthGlue

instance TeXNumericVariable HP.MathGlueVariable where

  advanceOp _ = mappend

  scaleUpOp _ = BL.scaleMathGlue

  scaleDownOp _ = BL.shrinkMathGlue
