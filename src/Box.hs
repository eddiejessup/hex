{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Box where

import qualified DVI.Write as DVIW
import qualified TFM.Main as TFMM
import qualified Paragraph as P

data SetGlue = SetGlue { dimen :: Int } deriving (Show)

data HBoxElement = HVBox [VBoxElement]
              | HHBox [HBoxElement]
              | HRule P.Rule
              | HGlue SetGlue
              | HKern P.Kern
              | HFontDefinition P.FontDefinition
              | HFontSelection P.FontSelection
              | HCharacter P.Character
              deriving (Show)

data VBoxElement = VVBox [VBoxElement]
              | VHBox [HBoxElement]
              | VRule P.Rule
              | VGlue SetGlue
              | VKern P.Kern
              | VFontDefinition P.FontDefinition
              | VFontSelection P.FontSelection
              deriving (Show)

class DVIAble a where
    toDVI :: a -> IO [DVIW.Instruction]

instance DVIAble [VBoxElement] where
    toDVI a = do
        inner <- mapM toDVI a
        return $ concat inner

instance DVIAble [HBoxElement] where
    toDVI a = do
        inner <- mapM toDVI a
        return $ concat inner

-- TODO: Rule.

instance DVIAble SetGlue where
    toDVI SetGlue{} = return []

instance DVIAble P.Kern where
    toDVI P.Kern{} = return []

instance DVIAble P.FontDefinition where
    toDVI P.FontDefinition{fontNr=fNr, fontPath=path, fontName=name, scaleFactorRatio=scale} =
        do
            -- TODO: 'Right'.
            Right fontInfo <- TFMM.readTFM path
            return [DVIW.DefineFont{fontInfo=fontInfo, fontPath=name, fontNr=fNr, scaleFactorRatio=scale}]

instance DVIAble P.FontSelection where
    toDVI P.FontSelection{fontNr=fNr} = return [DVIW.SelectFont fNr]

instance DVIAble P.Character where
    toDVI P.Character{code=c} = return [DVIW.Character{charNr=c, move=True}]

instance DVIAble HBoxElement where
    -- TODO: Move by amount dependent on contents.
    toDVI (HVBox e) = do
        inner <- toDVI e
        return $ [DVIW.PushStack] ++ inner ++ [DVIW.PopStack, DVIW.MoveRight{distance=200000}]
    -- TODO: Move by amount dependent on contents.
    toDVI (HHBox e) = do
        inner <- toDVI e
        return $ [DVIW.PushStack] ++ inner ++ [DVIW.PopStack, DVIW.MoveRight{distance=200000}]
    -- TODO: Rule.
    toDVI (HGlue SetGlue{dimen=d}) = return [DVIW.MoveRight{distance=d}]
    toDVI (HKern P.Kern{dimen=d}) = return [DVIW.MoveRight{distance=d}]
    toDVI (HFontDefinition e@P.FontDefinition{}) = toDVI e
    toDVI (HFontSelection e@P.FontSelection{}) = toDVI e
    toDVI (HCharacter e@P.Character{}) = toDVI e

instance DVIAble VBoxElement where
    -- TODO: Move by amount dependent on contents.
    toDVI (VVBox e) = do
        inner <- toDVI e
        return $ [DVIW.BeginNewPage, DVIW.PushStack] ++ inner ++ [DVIW.PopStack, DVIW.MoveDown{distance=200000}]
    -- TODO: Move by amount dependent on contents.
    toDVI (VHBox e) = do
        inner <- toDVI e
        return $ [DVIW.PushStack] ++ inner ++ [DVIW.PopStack, DVIW.MoveDown{distance=200000}]
    -- TODO: Rule.
    toDVI (VGlue SetGlue{dimen=d}) = return [DVIW.MoveDown{distance=d}]
    toDVI (VKern P.Kern{dimen=d}) = return [DVIW.MoveDown{distance=d}]
    toDVI (VFontDefinition e) = toDVI e
    toDVI (VFontSelection e) = toDVI e

setHList :: P.HListElement -> [HBoxElement]
setHList (P.HVList P.VList{contents=cs}) = [HVBox $ concatMap setVList cs]
setHList (P.HHList P.HList{contents=cs}) = [HHBox $ concatMap setHList cs]
setHList (P.HGlue P.Glue{dimen=d}) = [HGlue $ SetGlue d]
setHList (P.HPenalty _) = []
setHList (P.HRule a) = [HRule a]
setHList (P.HKern a) = [HKern a]
setHList (P.HFontDefinition a) = [HFontDefinition a]
setHList (P.HFontSelection a) = [HFontSelection a]
setHList (P.HCharacter a) = [HCharacter a]

setVList :: P.VListElement -> [VBoxElement]
setVList (P.VVList P.VList{contents=cs}) = [VVBox $ concatMap setVList cs]
setVList (P.VHList P.HList{contents=cs}) = [VHBox $ concatMap setHList cs]
setVList (P.VGlue P.Glue{dimen=d}) = [VGlue $ SetGlue d]
setVList (P.VPenalty _) = []
setVList (P.VRule a) = [VRule a]
setVList (P.VKern a) = [VKern a]
setVList (P.VFontDefinition a) = [VFontDefinition a]
setVList (P.VFontSelection a) = [VFontSelection a]
