module Hex.Variable where

import           Hexlude

import qualified Hex.BreakList            as BL
import           Hex.Command.Common
import           Hex.Config
import           Hex.Evaluate
import qualified Hex.Parse                as HP
import           Hex.Quantity

class TeXVariable a where

    setValue
        :: ( MonadState st m
           , HasType Config st
           , MonadError e m
           , AsType EvaluationError e
           , AsType ConfigError e
           )
        => a
        -> HP.GlobalFlag
        -> EvalTarget a
        -> m ()

setValueFromAST
    :: ( MonadState st m
       , HasType Config st
       , MonadError e m
       , AsType EvaluationError e
       , AsType ConfigError e
       , TeXVariable a
       , TeXEvaluable b
       , EvalTarget a ~ EvalTarget b
       )
    => a
    -> HP.GlobalFlag
    -> b
    -> m ()
setValueFromAST var globalFlag astVal =
    readOnState (texEvaluate astVal) >>= setValue var globalFlag

instance TeXVariable HP.TeXIntVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p ->
            modify $ typed @Config %~ setTeXIntParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ typed @Config %~ setTeXIntRegister i tgt globalFlag)

instance TeXVariable HP.LengthVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ typed @Config %~ setLengthParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ typed @Config %~ setLengthRegister i tgt globalFlag)

instance TeXVariable HP.GlueVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ typed @Config %~ setGlueParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ typed @Config %~ setGlueRegister i tgt globalFlag)

instance TeXVariable HP.MathGlueVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ typed @Config %~ setMathGlueParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ typed @Config %~ setMathGlueRegister i tgt globalFlag)

instance TeXVariable HP.TokenListVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ typed @Config %~ setTokenListParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ typed @Config %~ setTokenListRegister i tgt globalFlag)

instance TeXVariable HP.SpecialTeXInt where
    setValue p _ tgt =
        modify $ typed @Config %~ setSpecialTeXInt p tgt

instance TeXVariable HP.SpecialLength where
    setValue p _ tgt =
        modify $ typed @Config %~ setSpecialLength p tgt

-- Numeric variables.

class TeXVariable a => TeXNumericVariable a where
    advanceOp :: Proxy a -> EvalTarget a -> EvalTarget a -> EvalTarget a
    scaleUpOp :: Proxy a -> EvalTarget a -> TeXInt -> EvalTarget a
    scaleDownOp :: Proxy a -> EvalTarget a -> TeXInt -> EvalTarget a

    advanceValueFromAST
        :: ( MonadState st m
           , HasType Config st
           , MonadError e m
           , AsType EvaluationError e
           , AsType ConfigError e
           , TeXEvaluable a
           , TeXEvaluable b
           , EvalTarget b ~ EvalTarget a
           )
        => a
        -> HP.GlobalFlag
        -> b
        -> m ()
    advanceValueFromAST var globalFlag astPlusVal =
        readOnState (advanceOp (Proxy @a) <$> texEvaluate var <*> texEvaluate astPlusVal)
            >>= setValue var globalFlag

    scaleValueFromAST
        :: ( MonadState st m
           , HasType Config st
           , MonadError e m
           , AsType EvaluationError e
           , AsType ConfigError e
           , TeXEvaluable a
           )
        => a
        -> HP.GlobalFlag
        -> VDirection
        -> HP.TeXInt
        -> m ()
    scaleValueFromAST var globalFlag vDir scaleVal =
        do
        let op = case vDir of
                Upward   -> scaleUpOp
                Downward -> scaleDownOp
        readOnState ((op (Proxy @a)) <$> texEvaluate var <*> texEvaluate scaleVal)
            >>= setValue var globalFlag

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
