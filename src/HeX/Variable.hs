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
        :: ( MonadState Config m
           , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
           )
        => a
        -> HP.GlobalFlag
        -> EvalTarget a
        -> m ()

setValueFromAST
    :: ( MonadState Config m
       , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
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
        HP.ParamVar p -> modify $ setTeXIntParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setTeXIntRegister i tgt globalFlag)

instance TeXVariable HP.LengthVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setLengthParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setLengthRegister i tgt globalFlag)

instance TeXVariable HP.GlueVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setGlueParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setGlueRegister i tgt globalFlag)

instance TeXVariable HP.MathGlueVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setMathGlueParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setMathGlueRegister i tgt globalFlag)

instance TeXVariable HP.TokenListVariable where
    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setTokenListParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setTokenListRegister i tgt globalFlag)

instance TeXVariable HP.SpecialTeXInt where
    setValue p _ tgt =
        modify $ setSpecialTeXInt p tgt

instance TeXVariable HP.SpecialLength where
    setValue p _ tgt =
        modify $ setSpecialLength p tgt

-- Numeric variables.

class TeXVariable a => TeXNumericVariable a where
    advanceOp :: Proxy a -> EvalTarget a -> EvalTarget a -> EvalTarget a
    scaleUpOp :: Proxy a -> EvalTarget a -> TeXInt -> EvalTarget a
    scaleDownOp :: Proxy a -> EvalTarget a -> TeXInt -> EvalTarget a

    advanceValueFromAST
        :: ( MonadState Config m
           , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
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
        :: ( MonadState Config m
           , MonadErrorAnyOf e m '[EvaluationError, ConfigError]
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
