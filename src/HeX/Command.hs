module HeX.Command where

import           HeXlude

import           Control.Monad.Except           ( MonadError )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( MonadState
                                                , liftIO
                                                , modify
                                                )
import qualified Data.Text                     as Text
import qualified Data.HashMap.Strict           as HMap
import           Data.Path                      ( findFilePath )
import           Path                           ( Path
                                                , Rel
                                                , File
                                                )
import qualified Path

import qualified TFM
import           TFM                            ( TexFont(..) )

import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Categorise                 ( CharCode )
import           HeX.Config
import           HeX.Evaluate
import           HeX.BuildHelp
import qualified HeX.Lex                       as Lex
import qualified HeX.Parse                     as HP

-- Horizontal mode commands.

characterBox
    :: (MonadReader Config m, MonadError Text m) => CharCode -> m B.Character
characterBox char = do
    fontMetrics <- currentFontMetrics
    let toSP = TFM.designScaleSP fontMetrics
    TFM.Character { TFM.width, TFM.height, TFM.depth } <-
        liftMaybe "No such character"
            $ (HMap.lookup char $ characters fontMetrics)
    pure B.Character { B.char       = char
                     , B.charWidth  = toSP width
                     , B.charHeight = toSP height
                     , B.charDepth  = toSP depth
                     }

spaceGlue :: (MonadReader Config m, MonadError Text m) => m BL.Glue
spaceGlue = do
    fontMetrics@TexFont { spacing, spaceStretch, spaceShrink } <-
        currentFontMetrics
    let toSP   = TFM.designScaleSP fontMetrics
        toFlex = toSP >>> fromIntegral >>> BL.finiteFlex
    pure BL.Glue { BL.dimen   = toSP spacing
                 , BL.stretch = toFlex spaceStretch
                 , BL.shrink  = toFlex spaceShrink
                 }

-- Mode independent.

loadFont
    :: (MonadState Config m, MonadIO m, MonadError Text m)
    => Path Rel File
    -> HP.FontSpecification
    -> m B.FontDefinition
loadFont relPath fontSpec = do
    fontInfo_ <- readOnState $ readRelPath relPath
    let designSizeSP = TFM.designSizeSP $ fontMetrics fontInfo_
    scaleRatio <- readOnState $ evaluateFontSpecification designSizeSP fontSpec
    fontName   <- extractFontName relPath
    liftIO
        $  putStrLn
        $  "Loading font: "
        <> showT fontName
        <> ", with design size: "
        <> showT designSizeSP
        <> ", with scale ratio: "
        <> showT scaleRatio
    fNr <- addFont fontInfo_
    pure B.FontDefinition { B.fontNr           = fNr
        -- TODO: Improve mapping of name and path.
                          , B.fontPath         = relPath
                          , B.fontName         = fontName
                          , B.fontInfo         = fontMetrics fontInfo_
                          , B.scaleFactorRatio = scaleRatio
                          }
  where
    readRelPath p =
        findFilePath p
            <$> asks searchDirectories
            >>= liftThrow ("Could not find font: " <> showT p)
            >>= readFontInfo

    stripExtension p =
        liftThrow "Could not strip font extension" $ Path.setFileExtension "" p

    fileName = Path.filename >>> Path.toFilePath >>> toS

    extractFontName p = stripExtension p <&> fileName

selectFont :: MonadState Config m => Int -> HP.GlobalFlag -> m ()
selectFont n globalFlag = modify $ selectFontNr n globalFlag

getFileStream :: HMap.HashMap FourBitInt Handle -> IntVal -> Maybe Handle
getFileStream strms n = do
    fourBitn <- newFourBitInt n
    HMap.lookup fourBitn strms

-- Showing things.

showLexTok :: Lex.Token -> Text
showLexTok = \case
    Lex.CharCatToken Lex.CharCat { Lex.char, Lex.cat = Lex.Letter } ->
        Text.singleton char
    Lex.CharCatToken Lex.CharCat { Lex.char, Lex.cat = Lex.Other } ->
        Text.singleton char
    Lex.CharCatToken Lex.CharCat { Lex.char, Lex.cat = Lex.Space } ->
        Text.singleton char
    Lex.CharCatToken         cc                       -> showT cc
    Lex.ControlSequenceToken (Lex.ControlSequence cs) -> "\\" <> toS cs

showPrimTok :: HP.PrimitiveToken -> Text
showPrimTok = \case
    HP.UnexpandedTok   t   -> showLexTok t
    HP.SubParserError  err -> err
    HP.ResolutionError cs  -> "Unknown control sequence: " <> showT cs
    pt                     -> showT pt

showBalancedText :: HP.BalancedText -> Text
showBalancedText (HP.BalancedText txt) = Text.concat $ showLexTok <$> txt

showExpandedBalancedText :: HP.ExpandedBalancedText -> Text
showExpandedBalancedText (HP.ExpandedBalancedText txt) =
    Text.concat $ showPrimTok <$> txt
