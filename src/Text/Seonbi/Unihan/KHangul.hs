{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.Seonbi.Unihan.KHangul
    ( CharacterSet (..)
    , HanjaReadings
    , HanjaReadingCitation (..)
    , KHangulData
    , Purpose (..)
    , kHangulData
    , kHangulData'
    ) where

import Data.Either

import Data.Aeson
import Data.Attoparsec.Text
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed
import Data.Map.Strict
import Data.Set hiding (empty)
import System.FilePath (takeDirectory, (</>))

-- $setup
-- >>> import qualified Text.Show.Unicode
-- >>> :set -interactive-print=Text.Show.Unicode.uprint

-- | Maps all Hanja characters to their possible readings.
type KHangulData = Map Char HanjaReadings

-- | All readings of a Hanja character.
type HanjaReadings = Map Char HanjaReadingCitation

-- | Represents what standard a reading of character belongs to and a purpose
-- of the reading.
data HanjaReadingCitation =
    HanjaReadingCitation CharacterSet (Set Purpose) deriving (Eq, Ord, Show)

-- | Represents character set standards for Korean writing system.
data CharacterSet
    -- | KS X 1001 (정보 교환용 부호계).
    = KS_X_1001
    -- | KS X 1002 (정보 교환용 부호 확장 세트).
    | KS_X_1002
    -- | Represents that a Hanja character is not included in any Korean
    -- character set standards.
    | NonStandard
    deriving (Eq, Ord, Show)

-- | Represents purposes of Hanja characters.
data Purpose
    -- | Basic Hanja for educational use (漢文敎育用基礎漢字), a subset of
    -- Hanja defined in 1972 by a South Korean standard for educational use.
    = Education
    -- | Hanja for personal names (人名用漢字).
    | PersonalName
    deriving (Eq, Ord, Show)

citationParser :: Parser HanjaReadingCitation
citationParser = do
    charset' <- option NonStandard charset
    purposes <- many' purpose
    return $ HanjaReadingCitation charset' $ Data.Set.fromList purposes
  where
    charset :: Parser CharacterSet
    charset = do
        d <- digit
        case d of
            '0' -> return KS_X_1001
            '1' -> return KS_X_1002
            c -> fail ("Invalid kHangul character set code: " ++ show c)
    purpose :: Parser Purpose
    purpose = do
        l <- letter
        case l of
            'E' -> return Education
            'N' -> return PersonalName
            c -> fail ("Invalid kHangul purpose code: " ++ show c)

instance FromJSON HanjaReadingCitation where
    parseJSON =
        withText "kHangul value (e.g., 0E, 1N, 0EN)" $ \ t ->
            case parseOnly (citationParser <* endOfInput) t of
                Right cite -> return cite
                Left msg -> fail msg

kHangulData' :: Either String KHangulData
kHangulData' = eitherDecode $
    fromStrict $(embedFile $ takeDirectory __FILE__ </> "kHangul.json")

-- | Data that map Hanja characters to their corresponding kHangul entries
-- (i.e., Hanja readings and citations).
--
-- >>> import Data.Map.Strict as M
-- >>> let Just entries = M.lookup '天' kHangulData
-- >>> entries
-- fromList [('천',HanjaReadingCitation KS_X_1001 (fromList [Education]))]
kHangulData :: KHangulData
kHangulData = fromRight empty kHangulData'

{- HLINT ignore "Unused LANGUAGE pragma" -}
