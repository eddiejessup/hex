{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HeX.Config.Config where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.State.Lazy       ( StateT
                                                , liftIO
                                                , modify
                                                , MonadState
                                                , gets )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Control.Monad.Except           ( MonadError
                                                )
import           Data.Bits                      ( (.&.)
                                                , shiftR
                                                , shiftL
                                                )
import           Data.Char                      ( chr
                                                , ord
                                                , toLower
                                                , toUpper
                                                )
import qualified Data.HashMap.Strict           as HMap
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!?) )
import           Path
import           Safe                           ( toEnumMay )
import           System.Directory

import qualified TFM
import           TFM                            ( TexFont )

import           HeXPrelude
import           HeX.Concept
import           HeX.Type
import qualified HeX.Categorise                as Cat
import qualified HeX.Box                       as B
import qualified HeX.BreakList                 as BL
import           HeX.Parse.Token
import           HeX.Config.Parameters

-- The ⟨number⟩ at the end of a ⟨code assignment⟩ must not be negative, except
-- in the case that a \delcode is being assigned.

data FamilyCharRef = FamilyCharRef { family, position :: IntVal }
    deriving (Show)

instance Bounded FamilyCharRef where
    minBound = FamilyCharRef 0x0 0x00
    maxBound = FamilyCharRef 0xF 0xFF

instance Enum FamilyCharRef where
    toEnum n
        | n < 0 = error $ "Negative value: " ++ show n
        | n > 0xFFF = error $ "Value too large: " ++ show n
        | otherwise = FamilyCharRef (n `shiftR` 8) (n .&. 0xFF)

    fromEnum (FamilyCharRef fam pos) =
        (fam `shiftL` 8) + pos

data DelimiterVar
    = PresentDelimiterVar FamilyCharRef
    | NullDelimiterVar
    deriving (Show)

instance Bounded DelimiterVar where
    minBound = NullDelimiterVar
    maxBound = PresentDelimiterVar (maxBound :: FamilyCharRef)

instance Enum DelimiterVar where
    toEnum n
        | n == 0 = NullDelimiterVar
        | otherwise = PresentDelimiterVar $ toEnum n

    fromEnum NullDelimiterVar = 0
    fromEnum (PresentDelimiterVar f) = fromEnum f

data DelimiterSpec = DelimiterSpec { smallVar, largeVar :: DelimiterVar }
    deriving (Show)

data DelimiterCode
    = NotADelimiter IntVal
    | DelimiterSpecCode DelimiterSpec
    deriving (Show)

-- a delcode is either negative, for characters that should not act as
-- delimiters, or less than "1000000.
-- In other words, non-negative delcodes consist of six hexadecimal digits.
-- The first and last sets of three digits specify "small" and "large" variants
-- of the delimiter, respectively.
-- the code,
--     "123456
-- implies a small variant in position "23 of family "1, and a large variant in
-- position "56 of family "4.
-- If the small or large variant is given as "000, however (position 0 of
-- family 0), that variant is ignored.

instance Bounded DelimiterCode where
    minBound = NotADelimiter (minBound :: IntVal)
    maxBound = DelimiterSpecCode $ DelimiterSpec maxBound maxBound

instance Enum DelimiterCode where
    toEnum n
        | n < 0 = NotADelimiter n
        | n > 0xFFFFFF = error $ "Value too large: " ++ show n
        | otherwise =
            let smallVar = toEnum $ n `shiftR` 12
                largeVar = toEnum $ n .&. 0xFFF
            in  DelimiterSpecCode $ DelimiterSpec smallVar largeVar

    fromEnum (NotADelimiter n) = n
    fromEnum (DelimiterSpecCode DelimiterSpec{..}) =
        (fromEnum largeVar `shiftL` 12) + (fromEnum smallVar)

data MathClass
    = Ordinary        -- 0
    | LargeOperator   -- 1
    | BinaryRelation  -- 2
    | Relation        -- 3
    | Opening         -- 4
    | Closing         -- 5
    | Punctuation     -- 6
    | VariableFamily  -- 7
    deriving (Show, Enum, Bounded)

data MathCode
    = NormalMathCode MathClass FamilyCharRef
    | ActiveMathCode
    deriving (Show)

-- A math-code is specified by a number between 0 and 4095.
-- Consider such a number, represented as four hexadecimal digits: "pfcc
-- p: MathClass
-- f: font family
-- cc: character code position
-- A mathcode can also have the special value "8000, which causes the character
-- to behave as if it has catcode 13 (active).

instance Bounded MathCode where
    minBound = NormalMathCode minBound minBound
    maxBound = ActiveMathCode

instance Enum MathCode where
    toEnum n
        | n < 0 = error $ "Negative value: " ++ show n
        | n > 0x8000 = error $ "Value too large: " ++ show n
        | n == 0x8000 = ActiveMathCode
        | otherwise =
            let cls = toEnum $ n `shiftR` 12
                fam = toEnum $ n .&. 0xFFF
            in NormalMathCode cls fam

    fromEnum ActiveMathCode = 0x8000
    fromEnum (NormalMathCode cls famRef) =
        fromEnum famRef + (fromEnum cls `shiftL` 12)

data CaseChangeCode
    = NoCaseChange
    | ChangeToCode Cat.CharCode
    deriving (Show)

instance Bounded CaseChangeCode where
    minBound = NoCaseChange
    maxBound = ChangeToCode maxBound

-- Conversion to uppercase means that a character is replaced by its \uccode
-- value, unless the \uccode value is zero, when no change is made. Conversion
-- to lowercase is similar, using the \lccode.
instance Enum CaseChangeCode where
    toEnum n
        | n < 0 = error $ "Negative value: " ++ show n
        | n > 255 = error $ "Value too large: " ++ show n
        | n == 0 = NoCaseChange
        | otherwise = ChangeToCode $ toEnum n

    fromEnum NoCaseChange = 0
    fromEnum (ChangeToCode c) = fromEnum c

newtype SpaceFactorCode = SpaceFactorCode IntVal
    deriving (Show)

instance Bounded SpaceFactorCode where
    minBound = SpaceFactorCode 0
    maxBound = SpaceFactorCode 0x7FFF

instance Enum SpaceFactorCode where
    toEnum n
        | n < 0 = error $ "Negative value: " ++ show n
        | n >= 0x8000 = error $ "Value too large: " ++ show n
        | otherwise = SpaceFactorCode n

    fromEnum (SpaceFactorCode n) = n

 -- Furthermore, that ⟨number⟩
-- should be at most 15 for \catcode, 32768 for \mathcode, 255 for \lccode or
-- \uccode, 32767 for \sfcode, and 2^24 − 1 for \delcode.

data Config = Config
    { currentFontNr     :: Maybe Int
    , fontInfos         :: V.Vector FontInfo
    , searchDirectories :: [Path Abs Dir]
    , params            :: ParamConfig
    -- Char-code attribute maps.
    , catCodeMap        :: Cat.CharCatMap
    , mathCodeMap       :: Cat.CharCodeMap MathCode
    , lowercaseMap
    , uppercaseMap      :: Cat.CharCodeMap CaseChangeCode
    , spaceFactorMap    :: Cat.CharCodeMap SpaceFactorCode
    , delimiterCodeMap  :: Cat.CharCodeMap DelimiterCode
    } deriving (Show)

initialiseCharCodeMap :: (Cat.CharCode -> v) -> Cat.CharCodeMap v
initialiseCharCodeMap val = HMap.fromList $ ((\c -> (c, val c)) . chr) <$> [0 .. 127]

updateCharCodeMap
    :: (MonadError String m, MonadState Config m)
    => CodeType
    -> Cat.CharCode
    -> IntVal
    -> m ()
updateCharCodeMap t c n =
    modify =<< case t of
        CategoryCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure (\cnf -> cnf{catCodeMap=insert v $ catCodeMap cnf})
        MathCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure (\cnf -> cnf{mathCodeMap=insert v $ mathCodeMap cnf})
        ChangeCaseCodeType dir ->
            do
            v <- liftMay $ toEnumMay n
            pure $ case dir of
                Upward -> (\cnf -> cnf{uppercaseMap=insert v $ uppercaseMap cnf})
                Downward -> (\cnf -> cnf{lowercaseMap=insert v $ lowercaseMap cnf})
        SpaceFactorCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure (\cnf -> cnf{spaceFactorMap=insert v $ spaceFactorMap cnf})
        DelimiterCodeType ->
            do
            v <- liftMay $ toEnumMay n
            pure (\cnf -> cnf{delimiterCodeMap=insert v $ delimiterCodeMap cnf})
  where
    insert = HMap.insert c
    liftMay f = liftMaybe ("Invalid target for code type " ++ show t ++ ": " ++ show n) f

digits :: [Char]
digits = ['1'..'9']

lowerLetters :: [Char]
lowerLetters = ['a'..'z']

upperLetters :: [Char]
upperLetters = ['A'..'Z']

letters :: [Char]
letters = lowerLetters ++ upperLetters

-- The ten digits have \mathcode x = x + "7000.
-- The 52 letters have \mathcode x = x + "7100.
-- Otherwise,          \mathcode x = x
-- Put otherwise: letters are class 7, family 1; digits are class 7, family 0.
newMathCodeMap :: Cat.CharCodeMap MathCode
newMathCodeMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` digits  = NormalMathCode VariableFamily (FamilyCharRef 0 $ ord c)
        | c `elem` letters = NormalMathCode VariableFamily (FamilyCharRef 1 $ ord c)
        | otherwise        = NormalMathCode Ordinary       (FamilyCharRef 0 $ ord c)

-- By default, all \uccode and \lccode values are zero except that the
-- letters a to z and A to Z have \uccode values A to Z and \lccode values a to
-- z.
newLowercaseMap :: Cat.CharCodeMap CaseChangeCode
newLowercaseMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` letters = ChangeToCode $ toLower c
        | otherwise        = NoCaseChange

newUppercaseMap :: Cat.CharCodeMap CaseChangeCode
newUppercaseMap = initialiseCharCodeMap f
 where
    f c
        | c `elem` letters = ChangeToCode $ toUpper c
        | otherwise        = NoCaseChange

-- By default, all characters have a space factor code of 1000, except that the
-- uppercase letters ‘A’ through ‘Z’ have code 999.
newSpaceFactorMap :: Cat.CharCodeMap SpaceFactorCode
newSpaceFactorMap = initialiseCharCodeMap $ SpaceFactorCode . f
  where
    f c
        | c `elem` ['A' .. 'Z'] = 999
        | otherwise             = 1000

-- All delcodes are −1 until they are changed by a \delcode command.
newDelimiterCodeMap :: Cat.CharCodeMap DelimiterCode
newDelimiterCodeMap = initialiseCharCodeMap $ const $ NotADelimiter (-1)

newConfig :: IO Config
newConfig =
    do
    cwdRaw <- getCurrentDirectory
    cwd <- parseAbsDir cwdRaw
    pure Config
        { currentFontNr     = Nothing
        , fontInfos         = V.empty
        , searchDirectories = [cwd]
        , params            = usableParamConfig
        , catCodeMap        = Cat.usableCharCatMap
        , mathCodeMap       = newMathCodeMap
        , lowercaseMap      = newLowercaseMap
        , uppercaseMap      = newUppercaseMap
        , spaceFactorMap    = newSpaceFactorMap
        , delimiterCodeMap  = newDelimiterCodeMap
        }

type ConfStateT = StateT Config
type ConfReaderT = ReaderT Config

-- Fonts.

data FontInfo = FontInfo
    { fontMetrics :: TexFont
    , hyphenChar
    , skewChar    :: IntVal
    } deriving (Show)

readFontInfo :: (MonadState Config m, MonadIO m) => Path Abs File -> m FontInfo
readFontInfo fontPath =
    do
    fontMetrics <- liftIO $ TFM.readTFMFancy fontPath
    hyphenChar <- unIntParam <$> gets (defaultHyphenChar . params)
    skewChar <- unIntParam <$> gets (defaultSkewChar . params)
    pure FontInfo{..}

lookupFontInfo :: (MonadState Config m, MonadError String m) => Int -> m FontInfo
lookupFontInfo fNr =
    do
    infos <- gets fontInfos
    liftMaybe "No such font number" $ infos !? fNr

currentFontInfo :: (MonadState Config m, MonadError String m) => m FontInfo
currentFontInfo = gets currentFontNr >>= liftMaybe "Font number isn't set" >>= lookupFontInfo

currentFontMetrics :: (MonadState Config m, MonadError String m) => m TexFont
currentFontMetrics = fontMetrics <$> currentFontInfo

addFont :: MonadState Config m => FontInfo -> m Int
addFont newInfo =
    do
    infos <- gets fontInfos
    let newInfos = V.snoc infos newInfo
    modify (\conf -> conf{fontInfos = newInfos})
    pure $ V.length newInfos - 1

-- Parameters.

modifyParams :: MonadState Config m => (ParamConfig -> ParamConfig) -> m ()
modifyParams f =
    do
    pConf <- gets params
    let modConf _params conf = conf{params=_params}
    modify $ modConf $ f pConf

liftSetParam
    :: MonadState Config m
    => (p -> v -> ParamConfig -> ParamConfig)
    -> p
    -> v
    -> m ()
liftSetParam g p v = modifyParams $ g p v

setConfIntParam :: MonadState Config m => IntegerParameter -> IntVal -> m ()
setConfIntParam = liftSetParam setIntParam

setConfLenParam :: MonadState Config m => LengthParameter -> LenVal -> m ()
setConfLenParam = liftSetParam setLenParam

setConfGlueParam :: MonadState Config m => GlueParameter -> BL.Glue -> m ()
setConfGlueParam = liftSetParam setGlueParam

-- setConfMathGlueParam :: MonadState Config m => MathGlueParameter -> BL.MathGlue -> m ()
-- setConfMathGlueParam = liftSetParam setMathGlueParam

-- setConfTokenListParam :: MonadState Config m => TokenListParameter -> [Token] -> m ()
-- setConfTokenListParam = liftSetParam setTokenListParam

setConfSpecialInt :: MonadState Config m => SpecialInteger -> IntVal -> m ()
setConfSpecialInt = liftSetParam setSpecialInt

setConfSpecialLen :: MonadState Config m => SpecialLength -> LenVal -> m ()
setConfSpecialLen = liftSetParam setSpecialLen

parIndentBox :: Config -> BL.BreakableHListElem
parIndentBox conf =
    BL.HVListElem $ BL.VListBaseElem $ B.ElemBox $ B.Box
        { contents      = B.HBoxContents []
        , desiredLength = B.To . unLenParam . parIndent . params $ conf
        }
