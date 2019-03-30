module HeX.Variable where

import           HeXlude

import           Control.Monad.Except           ( MonadError)
import           Control.Monad.State.Lazy       ( MonadState
                                                , modify
                                                )

import qualified HeX.BreakList                 as BL
import           HeX.BuildHelp
import           HeX.Config
import           HeX.Evaluate
import qualified HeX.Parse                     as HP

class ( TeXEvaluable a
      , TeXEvaluable (TeXValueAST a)
      , EvalTarget a ~ EvalTarget (TeXValueAST a)
      ) => TeXVariable a where
    type TeXValueAST a

    setValue
        :: (MonadState Config m, MonadError Text m)
        => a
        -> HP.GlobalFlag
        -> EvalTarget a
        -> m ()

    setValueFromAST
        :: (MonadState Config m, MonadError Text m)
        => a
        -> HP.GlobalFlag
        -> TeXValueAST a
        -> m ()
    setValueFromAST var globalFlag astVal =
        readOnState (texEvaluate astVal) >>= setValue var globalFlag

instance TeXVariable HP.IntegerVariable where
    type TeXValueAST HP.IntegerVariable = HP.Number

    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setIntegerParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setIntegerRegister i tgt globalFlag)

instance TeXVariable HP.LengthVariable where
    type TeXValueAST HP.LengthVariable = HP.Length

    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setLengthParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setLengthRegister i tgt globalFlag)

instance TeXVariable HP.GlueVariable where
    type TeXValueAST HP.GlueVariable = HP.Glue

    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setGlueParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setGlueRegister i tgt globalFlag)

instance TeXVariable HP.MathGlueVariable where
    type TeXValueAST HP.MathGlueVariable = HP.MathGlue

    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setMathGlueParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setMathGlueRegister i tgt globalFlag)

instance TeXVariable HP.TokenListVariable where
    type TeXValueAST HP.TokenListVariable = HP.TokenListAssignmentTarget

    setValue v globalFlag tgt = case v of
        HP.ParamVar p -> modify $ setTokenListParameter p tgt globalFlag
        HP.RegisterVar iRaw ->
            readOnState (texEvaluate iRaw)
                >>= (\i -> modify $ setTokenListRegister i tgt globalFlag)

instance TeXVariable HP.SpecialInteger where
    type TeXValueAST HP.SpecialInteger = HP.Number

    setValue p _ tgt =
        modify $ setSpecialInteger p tgt

instance TeXVariable HP.SpecialLength where
    type TeXValueAST HP.SpecialLength = HP.Length

    setValue p _ tgt =
        modify $ setSpecialLength p tgt

class (TeXVariable a) => TeXNumericVariable a where
    advanceOp :: Proxy a -> EvalTarget a -> EvalTarget a -> EvalTarget a
    scaleUpOp :: Proxy a -> EvalTarget a -> IntVal -> EvalTarget a
    scaleDownOp :: Proxy a -> EvalTarget a -> IntVal -> EvalTarget a

    advanceValueFromAST
        :: (MonadState Config m, MonadError Text m)
        => a
        -> HP.GlobalFlag
        -> TeXValueAST a
        -> m ()
    advanceValueFromAST var globalFlag astPlusVal =
        do
        newVal <- readOnState $ (advanceOp (Proxy @a)) <$> texEvaluate var <*> texEvaluate astPlusVal
        setValue var globalFlag newVal

    scaleValueFromAST
        :: (MonadState Config m, MonadError Text m)
        => a
        -> HP.GlobalFlag
        -> VDirection
        -> HP.Number
        -> m ()
    scaleValueFromAST var globalFlag vDir scaleVal =
        do
        let op = case vDir of
                Upward -> scaleUpOp
                Downward -> scaleDownOp
        newVal <- readOnState $ (op (Proxy @a)) <$> texEvaluate var <*> texEvaluate scaleVal
        setValue var globalFlag newVal

instance TeXNumericVariable HP.IntegerVariable where
    advanceOp _ = (+)
    scaleUpOp _ = (*)
    -- Division of a positive integer by a positive integer
    -- discards the remainder, and the sign of the result
    -- changes if you change the sign of either operand.
    scaleDownOp _ = quot

instance TeXNumericVariable HP.LengthVariable where
    advanceOp _ = (+)
    scaleUpOp _ = (*)
    scaleDownOp _ = quot

instance TeXNumericVariable HP.GlueVariable where
    advanceOp _ = mappend
    scaleUpOp _ = BL.multiplyGlue
    scaleDownOp _ = BL.divGlue

instance TeXNumericVariable HP.MathGlueVariable where
    advanceOp _ = mappend
    scaleUpOp _ = BL.multiplyMathGlue
    scaleDownOp _ = BL.divMathGlue
