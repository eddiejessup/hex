{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Parse where

import qualified Data.IntMap.Strict as IMAP
import Data.List (intersperse)

import qualified TFM.Main as TFMM
import qualified TFM.Character as TFMC
import qualified Box as B
import qualified Setting as S
import qualified Command as C

import qualified Debug.Trace as T

newtype State = State { currentFontInfo :: Maybe TFMM.TexFont } deriving Show

characterBox :: State -> Int -> Maybe B.Character
characterBox state code = do
  font <- currentFontInfo state
  let toSP = TFMM.toScaledPoint font
  TFMC.Character{width=w, height=h, depth=d} <- IMAP.lookup code $ TFMM.characters font
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

spaceGlue :: State -> Maybe S.Glue
spaceGlue state = do
  font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr} <- currentFontInfo state
  let
    toSP = TFMM.toScaledPoint font
    toFlex = S.finiteGlueFlex . toSP
  return S.Glue{dimen=toSP d, stretch=toFlex str, shrink=toFlex shr}

-- Returns: state, paragraph's-worth of h-list elements, remaining commands.
extractParagraph :: State -> [S.BreakableHListElem] -> [C.Command] -> IO (State, [S.BreakableHListElem], [C.Command])
-- Run out of commands: return the list so far.
extractParagraph state acc [] = return (state, acc, [])
-- Mundane commands.
extractParagraph state acc (C.AddCharacter{code=i}:rest) = do
  charBox <- case characterBox state i of
    Just c -> return $ S.HCharacter c
    Nothing -> fail "Could not get character info"
  extractParagraph state (charBox:acc) rest
extractParagraph state acc (C.AddSpace{}:rest) = do
  glue <- case spaceGlue state of
    Just sg -> return $ S.HGlue sg
    Nothing -> fail "Could not get space glue"
  extractParagraph state (glue:acc) rest
-- See a \par: Return to outer mode with completed list.
extractParagraph state acc (C.StartParagraph{}:rest) = return (state, acc, rest)
-- See something else: Ignore and continue.
extractParagraph state acc (_:rest) = extractParagraph state acc rest

extractBoxedParagraph :: Int -> Int -> Int -> S.Glue -> State -> [C.Command] -> IO (State, [S.BreakableVListElem], [C.Command])
extractBoxedParagraph desiredWidth lineTolerance linePenalty interLineGlue state coms = do
  (stateNext, hList, comsNext) <- extractParagraph state [] coms
  let
    lineBoxes = S.setParagraph desiredWidth lineTolerance linePenalty hList
    paraBoxes = intersperse (S.VGlue interLineGlue) lineBoxes
  return (stateNext, paraBoxes, comsNext)

desiredWidth :: Int
desiredWidth = 28000000

lineTolerance :: Int
lineTolerance = 200

linePenalty :: Int
linePenalty = 10

desiredHeight :: Int
desiredHeight = 60000000

interLineGlue :: S.Glue
interLineGlue = S.Glue{dimen = 400000, stretch=S.noGlueFlex, shrink=S.noGlueFlex}

interParGlue :: S.Glue
interParGlue = S.Glue {dimen = 1000000, stretch = S.noGlueFlex, shrink = S.noGlueFlex}

startsParagraph :: C.Command -> Bool
startsParagraph C.AddCharacter{} = True
startsParagraph C.AddSpace{} = True
startsParagraph _ = False

-- Returns: state, a page of set v-box elements, remaining v-list elements, remaining commands.
extractPage :: State -> [C.Command] -> [S.BreakableVListElem] -> IO (State, B.Page, [S.BreakableVListElem], [C.Command])
extractPage state [] acc = return (state, B.Page $ S.setListElems S.NaturallyGood acc, [], [])
extractPage state (a:comsRest) acc
  | startsParagraph a = do
    (stateNext, paraBoxes, commandsNext) <- extractBoxedParagraph desiredWidth lineTolerance linePenalty interLineGlue state (a:comsRest)
    let
      breakItem = S.GlueBreak interParGlue
      extra = paraBoxes ++ [S.VGlue interParGlue]
      -- TODO: Discard when adding to empty page.
      -- TODO: Keep best rather than taking last.
      pen = S.breakPenalty breakItem
      stat = S.listGlueSetRatio desiredHeight $ acc ++ extra
      bad = S.listStatusBadness stat
      cost = T.traceShow stat $ S.pageCost pen bad 0
      page = B.Page $ S.setListElems stat acc
    if (cost == S.oneMillion) || (pen <= -S.tenK)
      then return (stateNext, page, extra, commandsNext)
      else extractPage stateNext commandsNext (acc ++ extra)
  | C.Assign (C.NonMacroAssign C.SelectFont) <- a = do
    fontInfo <- TFMM.readTFM "cmr10.tfm"
    let fontDef =
          S.VFontDefinition
            B.FontDefinition
            { fontNr = 1
            , fontPath = "cmr10.tfm"
            , fontName = "cmr10"
            , fontInfo = fontInfo
            , scaleFactorRatio = 1.0
            }
        fontSel = S.VFontSelection B.FontSelection {fontNr = 1}
    extractPage state{currentFontInfo=Just fontInfo} comsRest $ acc ++ [fontDef, fontSel]
  | otherwise = extractPage state comsRest acc

-- Returns: state, some pages.
extractPages :: State -> [C.Command] -> [S.BreakableVListElem] -> IO (State, [B.Page])
extractPages s [] [] = return (s, [])
extractPages state commands acc = do
    (stateNext, page, vListRemain, commandsNext) <- extractPage state commands acc
    (stateEnd, pagesRest) <- extractPages stateNext commandsNext vListRemain
    return (stateEnd, page:pagesRest)
