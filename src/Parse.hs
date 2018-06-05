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

character :: State -> Int -> Maybe B.Character
character state code = do
  font <- currentFontInfo state
  let toSP = TFMM.toScaledPoint font
  TFMC.Character{width=w, height=h, depth=d} <- IMAP.lookup code $ TFMM.characters font
  return B.Character {code = code, width=toSP w, height=toSP h, depth=toSP d}

-- Returns: state, paragraph's-worth of h-list elements, remaining commands.
extractParagraph :: State -> [C.Command] -> IO (State, [S.BreakableHListElem], [C.Command])
extractParagraph s [] = return (s, [], [])
extractParagraph s (C.AddCharacter{code=i}:comsRest) = do
  extra <- case character s i of
    Just c -> return [S.HCharacter c]
    Nothing -> fail "Could not get character info"
  (sNext, nextResult, nextRemain) <- extractParagraph s comsRest
  return (sNext, extra ++ nextResult, nextRemain)
extractParagraph s (C.AddSpace{}:comsRest) = do
  extra <- case currentFontInfo s of
    (Just (font@TFMM.TexFont{spacing=d, spaceStretch=str, spaceShrink=shr})) -> do
      let toSP = TFMM.toScaledPoint font
      return [S.HGlue S.Glue {dimen = toSP d, stretch = S.finiteGlueFlex $ toSP str, shrink = S.finiteGlueFlex $ toSP shr}]
    Nothing ->
      fail "No font selected"
  (sNext, nextResult, nextRemain) <- extractParagraph s comsRest
  return (sNext, extra ++ nextResult, nextRemain)
extractParagraph s (C.StartParagraph{}:comsRest) = return (s, [], comsRest)
extractParagraph s (_:comsRest) = extractParagraph s comsRest

desiredWidth :: Int
desiredWidth = 28000000
lineTolerance :: Int
lineTolerance = 200
linePenalty :: Int
linePenalty = 10
desiredHeight :: Int
desiredHeight = 60000000

startsParagraph :: C.Command -> Bool
startsParagraph C.AddCharacter{} = True
startsParagraph C.AddSpace{} = True
startsParagraph _ = False

-- Returns: state, a page of set v-box elements, remaining v-list elements, remaining commands.
extractPage :: State -> [C.Command] -> [S.BreakableVListElem] -> IO (State, B.Page, [S.BreakableVListElem], [C.Command])
extractPage s [] acc = return (s, B.Page $ S.setListElems S.NaturallyGood acc, [], [])
extractPage s (a:comsRest) acc
  | startsParagraph a = do
    (sNext, hConts, commandsNext) <- extractParagraph s (a : comsRest)
    let 
      lineBoxes = S.setParagraph desiredWidth lineTolerance linePenalty hConts
      paraBoxes = intersperse (S.VGlue S.Glue{dimen = 400000, stretch=S.noGlueFlex, shrink=S.noGlueFlex}) lineBoxes
      downGlue = S.Glue {dimen = 1000000, stretch = S.noGlueFlex, shrink = S.noGlueFlex}
      down = S.VGlue downGlue
      breakItem = S.GlueBreak downGlue
      extra = paraBoxes ++ [down]

      -- TODO: Discard when adding to empty page.
      -- TODO: Keep best rather than taking last.
      pen = S.breakPenalty breakItem
      stat = S.listGlueSetRatio desiredHeight $ acc ++ extra
      bad = S.listStatusBadness stat
      cost = T.traceShow stat $ S.pageCost pen bad 0
      page = B.Page $ S.setListElems stat acc
    if (cost == S.oneMillion) || (pen <= -S.tenK)
      then return (sNext, page, extra, commandsNext)
      else extractPage sNext commandsNext (acc ++ extra)
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
        extra = [fontDef, fontSel]
        sNext = s{currentFontInfo=Just fontInfo}
        accNext = acc ++ extra
    extractPage sNext comsRest accNext
  | otherwise = extractPage s comsRest acc

-- Returns: state, some pages.
extractPages :: State -> [C.Command] -> [S.BreakableVListElem] -> IO (State, [B.Page])
extractPages s [] [] = return (s, [])
extractPages state commands acc = do
    (stateNext, page, vListRemain, commandsNext) <- extractPage state commands acc
    (stateEnd, pagesRest) <- extractPages stateNext commandsNext vListRemain
    return (stateEnd, page:pagesRest)
