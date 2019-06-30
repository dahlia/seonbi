module Text.Seonbi.Hangul
    ( JamoTriple
    , fromJamoTriple
    , isHangulSyllable
    , toJamoTriple
    ) where

-- $setup
-- >>> import qualified Text.Show.Unicode
-- >>> :set -interactive-print=Text.Show.Unicode.uprint

-- | A triple of an initial consonant, a vowel, and an optional final consonant.
type JamoTriple = (Char, Char, Maybe Char)

-- | Checks if a character is a hangul letter and a complete syllable.
--
-- >>> isHangulSyllable '가'
-- True
-- >>> isHangulSyllable 'ㄱ'
-- False
isHangulSyllable :: Char -> Bool
isHangulSyllable c =
    c >= '\xac00' && c <= '\xd7a3';

syllableBase :: Int
syllableBase = 0xac00

initialBase :: Int
initialBase = 0x1100

vowelBase :: Int
vowelBase = 0x1161

finalBase :: Int
finalBase = 0x11a7

vowelCount :: Int
vowelCount = 21;

finalCount :: Int
finalCount = 28;

-- | Takes a complete hangul syllable apart into consonants and a vowel.
-- Returns 'Nothing' for non-hangul letters.
--
-- >>> toJamoTriple '가'
-- Just ('ᄀ','ᅡ',Nothing)
-- >>> toJamoTriple '글'
-- Just ('ᄀ','ᅳ',Just 'ᆯ')
-- >>> toJamoTriple 'A'
-- Nothing
toJamoTriple :: Char -> Maybe JamoTriple
toJamoTriple c
  | isHangulSyllable c = Just
      ( toEnum $ initialBase + ((syllable `div` finalCount) `div` vowelCount)
      , toEnum $ vowelBase + ((syllable `div` finalCount) `mod` vowelCount)
      , case syllable `mod` finalCount of
          0 -> Nothing
          f -> Just $ toEnum (finalBase + f)
      )
  | otherwise = Nothing
  where
    syllable :: Int
    syllable = fromEnum c - syllableBase

-- | Composes hangul jamo triple into a hangul syllable.
--
-- >>> fromJamoTriple ('ᄀ', 'ᅡ', Nothing)
-- Just '가'
-- >>> fromJamoTriple ('ᄀ', 'ᅳ', Just 'ᆯ')
-- Just '글'
fromJamoTriple :: JamoTriple -> Maybe Char
fromJamoTriple (initial, vowel, final)
  | initialIndex < 0 = Nothing
  | initialIndex > 18 = Nothing
  | vowelIndex < 0 = Nothing
  | vowelIndex > 20 = Nothing
  | finalIndex < 0 = Nothing
  | finalIndex > 27 = Nothing
  | otherwise = Just $ toEnum $ syllableBase +
      (initialIndex * vowelCount + vowelIndex) * finalCount + finalIndex
  where
    initialIndex :: Int
    initialIndex = fromEnum initial - initialBase
    vowelIndex :: Int
    vowelIndex = fromEnum vowel - vowelBase
    finalIndex :: Int
    finalIndex = maybe 0 (\ f -> fromEnum f - finalBase) final
