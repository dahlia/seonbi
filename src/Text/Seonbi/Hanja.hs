{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Text.Seonbi.Hanja
    ( HanjaPhoneticization (..)
    , HanjaWordRenderer
    , convertInitialSoundLaw
    , def
    , hangulOnly
    , hanjaInParentheses
    , hanjaInRuby
    , initialSoundLawTable
    , initialSoundLawTable'
    , phoneticizeHanja
    , phoneticizeHanjaChar
    , phoneticizeHanjaWord
    , phoneticizeHanjaWordWithInitialSoundLaw
    , revertInitialSoundLaw
    ) where

import Prelude hiding (lookup)

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List hiding (lookup)
import Data.Maybe
import Data.Ord (comparing)

import Data.Attoparsec.Text
import Data.Default
import Data.Map.Strict
import Data.Set
import Data.Text hiding (concatMap)

import Text.Seonbi.Hangul
import Text.Seonbi.Html
import Text.Seonbi.Html.TagStack (push)
import Text.Seonbi.Unihan.KHangul

data HanjaPhoneticization = HanjaPhoneticization
    { -- | A function to phoneticize a hanja word.
      -- Use 'phoneticizeHanjaWordWithInitialSoundLaw' for South Korean
      -- orthography, or 'phoneticizeHanjaWord' for North Korean orthography.
      phoneticizer :: Text -> Text
      -- | A function to render a hanja word.  See also 'HanjaWordRenderer'.
    , wordRenderer :: HanjaWordRenderer
      -- | Whether to insert some HTML comments that contain useful information
      -- for debugging into the result.  This does not affect the rendering
      -- of the result HTML, but only the HTML code.
    , debugComment :: Bool
    }

-- | A function to render a hanja word.
-- Choose one in 'hangulOnly', 'hanjaInParentheses', and 'hanjaInRuby'.
type HanjaWordRenderer
  = HtmlTagStack
  -> Text
  -> Text
  -> [HtmlEntity]

-- | Renders a word in hangul-only, no hanja at all (e.g., @안녕히@).
hangulOnly :: HanjaWordRenderer
hangulOnly stack _ hangul = [HtmlCdata stack hangul]

-- | Renders a word in hangul followed by hanja in parentheses
-- (e.g., @안녕(安寧)히@).
hanjaInParentheses :: HanjaWordRenderer
hanjaInParentheses stack hanja hangul =
    [HtmlCdata stack $ Data.Text.concat [hangul, "(", hanja, ")"]]

-- | Renders a word in @<ruby>@ tag (e.g.,
-- @\<ruby\>安寧\<rp\>(\<\/rp\>\<rt\>안녕\<\/rt\>\<rp\>)\<\/rp\>\<\/ruby\>히@).
--
-- Please read [Use Cases & Exploratory Approaches for Ruby
-- Markup](https://www.w3.org/TR/ruby-use-cases/) as well for more information.
hanjaInRuby :: HanjaWordRenderer
hanjaInRuby stack hanja hangul =
    [ HtmlStartTag stack Ruby ""
    , HtmlCdata rubyStack hanja
    , HtmlStartTag rubyStack RP ""
    , HtmlText (push RP rubyStack) "("
    , HtmlEndTag rubyStack RP
    , HtmlStartTag rubyStack RT ""
    , HtmlCdata (push RT rubyStack) hangul
    , HtmlEndTag rubyStack RT
    , HtmlStartTag rubyStack RP ""
    , HtmlText (push RP rubyStack) ")"
    , HtmlEndTag rubyStack RP
    , HtmlEndTag stack Ruby
    ]
  where
    rubyStack :: HtmlTagStack
    rubyStack = push Ruby stack

instance Default HanjaPhoneticization where
    def = HanjaPhoneticization
        { phoneticizer = phoneticizeHanjaWordWithInitialSoundLaw
        , wordRenderer = hangulOnly
        , debugComment = False
        }

-- | Transforms hanja words in the given HTML entities into corresponding
-- hangul words.
phoneticizeHanja
    :: HanjaPhoneticization
    -- ^ Configures the phoneticization details.
    -> [HtmlEntity]
    -- ^ HTML entities (that may contain some hanja words) to phoneticize
    -- all hanja words into corresponding hangul-only words.
    -> [HtmlEntity]
    -- ^ HTML entities that have no hanja words but hangul-only words instead.
phoneticizeHanja HanjaPhoneticization { phoneticizer
                                      , wordRenderer
                                      , debugComment
                                      } =
    concatMap transform . normalizeText
  where
    transform :: HtmlEntity -> [HtmlEntity]
    transform entity@HtmlText { tagStack = tagStack', rawText = rawText' } =
        case analyzeHanjaText rawText' of
            Nothing -> [entity { rawText = rawText' }]
            Just pairs -> concatMap (transformHanjaText tagStack') pairs
    transform entity = [entity]
    transformHanjaText :: HtmlTagStack -> (Text, Text) -> [HtmlEntity]
    transformHanjaText tagStack' (hanja, text')
      | debugComment =
            HtmlComment tagStack' (" Hanja: " `append` hanja)
                : hangulEntities ++
                (HtmlComment tagStack' " /Hanja " : textEntities)
      | otherwise =
            hangulEntities ++ textEntities
      where
        htmlText :: Text -> HtmlEntity
        htmlText = HtmlText tagStack'
        texts :: [Text]
        texts = splitOn "]]>" text'
        hangul :: Text
        hangul = phoneticizer hanja
        hangulEntities :: [HtmlEntity]
        hangulEntities = mapHanjaWords
            (wordRenderer tagStack')
            ((: []) . HtmlCdata tagStack')
            hanja
            hangul
        textEntities :: [HtmlEntity]
        textEntities = (htmlText <$> Prelude.take 1 texts) ++ Prelude.concat
            [ [htmlText "]]&gt;", HtmlCdata tagStack' t]
            | t <- Prelude.drop 1 texts
            ]
        mapHanjaWords :: (Text -> Text -> [HtmlEntity])
                      -> (Text -> [HtmlEntity])
                      -> Text
                      -> Text
                      -> [HtmlEntity]
        mapHanjaWords renderHanja renderHangul hanja' hangul'
          | Data.Text.null hanja' =
                []
          | len /= Data.Text.length hangul' =
                renderHanja hanja' hangul'
          | otherwise =
                renderedHanja ++
                    let restHanja = drop' hanja'
                        restHangul = drop' hangul'
                        (prefix', nextHanja, nextHangul) =
                            case commonPrefixes restHanja restHangul of
                                Nothing -> ([], restHanja, restHangul)
                                Just ("", restHanja', restHangul') ->
                                    ([], restHanja', restHangul')
                                Just (prefix, restHanja', restHangul') ->
                                    ( renderHangul prefix
                                    , restHanja'
                                    , restHangul'
                                    )
                    in
                        prefix' ++ mapHanjaWords
                            renderHanja
                            renderHangul
                            nextHanja
                            nextHangul
          where
            len :: Int
            len = Data.Text.length hanja'
            wordLen :: Int
            wordLen = fromMaybe len $
                Data.List.find
                    (\ i -> index hanja' i == index hangul' i)
                    ([0..(len - 1)] :: [Int])
            take' :: Text -> Text
            take' = Data.Text.take wordLen
            drop' :: Text -> Text
            drop' = Data.Text.drop wordLen
            renderedHanja :: [HtmlEntity]
            renderedHanja
              | wordLen > 0 = renderHanja (take' hanja') (take' hangul')
              | otherwise = []

analyzeHanjaText :: Text -> Maybe [(Text, Text)]
analyzeHanjaText text' =
    case parseOnly (textParser <* endOfInput) text' of
        Left _ -> Nothing
        Right [("", _)] -> Nothing
        Right pairs -> Just pairs

-- | Reads a hanja word and returns a corresponding hangul word.
--
-- >>> :set -XOverloadedStrings
-- >>> phoneticizeHanjaWord "漢字"
-- "\54620\51088"
--
-- Note that it does not apply Initial Sound Law (頭音法則):
--
-- >>> phoneticizeHanjaWord  "來日"
-- "\47000\51068"
phoneticizeHanjaWord :: Text -> Text
phoneticizeHanjaWord =
    Data.Text.map phoneticizeHanjaChar

-- | It is like 'phoneticizeHanjaWord', but it also applies
-- Initial Sound Law (頭音法則).
--
-- >>> :set -XOverloadedStrings
-- >>> phoneticizeHanjaWordWithInitialSoundLaw  "來日"
-- "\45236\51068"
-- >>> phoneticizeHanjaWordWithInitialSoundLaw  "未來"
-- "\48120\47000"
phoneticizeHanjaWordWithInitialSoundLaw :: Text -> Text
phoneticizeHanjaWordWithInitialSoundLaw word =
    case parseOnly (parser <* endOfInput) word of
        Left _ -> word
        Right "" -> word
        Right hangulWord -> hangulWord
  where
    parser :: Parser Text
    parser = do
        chars <- many'
            ( try yeolYul
            <|> try prefixedNumber
            <|> try hanNumber
            <|> try (Data.Text.singleton . phoneticize <$> anyChar)
            )
        let hangulWord = Data.Text.concat chars
        return $ Data.Text.concat
            [ Data.Text.map convertInitialSoundLaw $ Data.Text.take 1 hangulWord
            , Data.Text.drop 1 hangulWord
            ]
    yeolYul :: Parser Text
    yeolYul = do
        former <- satisfy $ \ c ->
            c `hasBatchim` Just '\x11ab' || c `hasBatchim` Nothing
        later <- phone '렬' <|> phone '률'
        return $ pack
            [ phoneticize former
            , convert later
            ]
    prefixedNumber :: Parser Text
    prefixedNumber = do
        prefix <- char '第'
        digits <- takeWhile1 isHanDigit
        return $ Data.Text.cons
            (phoneticize prefix)
            (Data.Text.map convertDigit digits)
    hanNumber :: Parser Text
    hanNumber = do
        first <- hanDigit
        rest <- takeWhile1 isHanDigit
        return $ Data.Text.map convertDigit $ Data.Text.cons first rest
    hanDigit :: Parser Char
    hanDigit = satisfy isHanDigit
    phone :: Char -> Parser Char
    phone hangul = satisfy ((== hangul) . phoneticize)
    convertDigit :: Char -> Char
    convertDigit = convertInitialSoundLaw . phoneticizeDigit
    convert :: Char -> Char
    convert = convertInitialSoundLaw . phoneticize
    phoneticizeDigit :: Char -> Char
    phoneticizeDigit '參' = '삼'
    phoneticizeDigit '叁' = '삼'
    phoneticizeDigit '参' = '삼'
    phoneticizeDigit '叄' = '삼'
    phoneticizeDigit '拾' = '십'
    phoneticizeDigit c = phoneticize c
    phoneticize :: Char -> Char
    phoneticize = phoneticizeHanjaChar
    hasBatchim :: Char -> Maybe Char -> Bool
    hasBatchim c batchim =
        case toJamoTriple (phoneticize c) of
            Just (_, _, final) -> final == batchim
            _ -> False
    isHanDigit :: Char -> Bool
    isHanDigit = inClass $
        "零一壹壱弌夁二貳贰弐弍貮三參叁参弎叄四肆䦉五伍六陸陆陸七柒漆八捌" ++
        "九玖十拾百佰陌千仟阡萬万億兆京垓秭穰溝澗"

-- | Reads a hanja character as a hangul character.
--
-- >>> phoneticizeHanjaChar '漢'
-- '\54620'
--
-- Note that it does not follow Initial Sound Law (頭音法則):
--
-- >>> phoneticizeHanjaChar '六'
-- '\47449'
phoneticizeHanjaChar :: Char -> Char
phoneticizeHanjaChar c = fromMaybe c $ do
    readings <- lookup c kHangulData
    let readings' = Data.Map.Strict.toList readings
    let (sound, _) = minimumBy (comparing snd) readings'
    let initialLawReverted = Data.Set.filter
            (`Data.Map.Strict.member` readings)
            (revertInitialSoundLaw sound)
    return $ case Data.Set.toList initialLawReverted of
        [] -> sound
        reverted : _ -> reverted

withoutBatchim :: Char -> Maybe (Char, Maybe Char)
withoutBatchim hangul = do
    (initial, vowel, final) <- toJamoTriple hangul
    noBatchim <- fromJamoTriple (initial, vowel, Nothing)
    return (noBatchim, final)

withBatchim :: Char -> Maybe Char -> Maybe Char
withBatchim hangul final = do
    (initial, vowel, _) <- toJamoTriple hangul
    fromJamoTriple (initial, vowel, final)

-- | Converts a hangul character according to Initial Sound Law (頭音法則).
--
-- >>> convertInitialSoundLaw '념'
-- '\50684'
--
-- If an input is not a hangul syllable or a syllable is not applicable to
-- the law it returns the given input without change:
--
-- >>> convertInitialSoundLaw 'A'
-- 'A'
-- >>> convertInitialSoundLaw '가'
-- '\44032'
convertInitialSoundLaw :: Char -> Char
convertInitialSoundLaw sound = fromMaybe sound $ do
    (pattern', final) <- withoutBatchim sound
    let converted = findWithDefault pattern' pattern' initialSoundLawTable
    withBatchim converted final

-- | It's a kind of inverse function of 'convertInitialSoundLaw',
-- except it returns a set of candidates instead of a single canonical answer
-- because Initial Sound Law (頭音法則) is not a bijective function.
--
-- >>> revertInitialSoundLaw '예'
-- fromList "\47168"
-- >>> revertInitialSoundLaw '염'
-- fromList "\45392\47156"
--
-- It returns an empty set if an input is not applicable to the law:
--
-- >>> revertInitialSoundLaw '가'
-- fromList ""
revertInitialSoundLaw :: Char -> Set Char
revertInitialSoundLaw sound = fromMaybe Data.Set.empty $ do
    (pattern', final) <- withoutBatchim sound
    let candidates = Data.Set.toList $
            findWithDefault Data.Set.empty pattern' initialSoundLawTable'
    Just $ Data.Set.fromList $ catMaybes $ candidates <&> (`withBatchim` final)
  where
    (<&>) :: Functor f => f a -> (a -> b) -> f b
    (<&>) = flip fmap

textParser :: Parser [(Text, Text)]
textParser = many' $ do
    hanjas <- many' $ try $ do
        c <- unnamedCharRef
        unless (isHanjaOrDigit c) (fail "not a hanja")
        return c
    hanjas' <- Data.Attoparsec.Text.takeWhile isHanjaOrDigit
    chars <- many' unnamedCharRef
    chars' <- takeTill isHanjaOrDigit
    let hanjaText = pack hanjas `append` hanjas'
    let text' = pack chars `append` chars'
    when (Data.Text.null $ hanjaText `append` text') (fail "parsed nothing")
    return (hanjaText, text')
  where
    isHanjaOrDigit :: Char -> Bool
    isHanjaOrDigit c =
        isDigit c || isHanja c
    isHanja :: Char -> Bool
    isHanja c =
        -- Ideographic Description Character
        '\x2f00'  <= c && c <= '\x2fff' ||
        -- U+3007 IDEOGRAPHIC NUMBER ZERO (〇)
        '\x3007'  == c ||
        -- CJK Unified Ideographs Extension A
        '\x3400'  <= c && c <= '\x4dbf' ||
        -- CJK Unified Ideographs
        '\x4e00'  <= c && c <= '\x9fcc' ||
        -- CJK Compatibility Ideographs
        '\xf900'  <= c && c <= '\xfaff' ||
        -- CJK Unified Ideographs Extension B
        '\x20000' <= c && c <= '\x2a6d6' ||
        -- CJK Unified Ideographs Extension C
        '\x2a700' <= c && c <= '\x2b734' ||
        -- CJK Unified Ideographs Extension D
        '\x2b740' <= c && c <= '\x2b81d' ||
        -- CJK Unified Ideographs Extension E
        '\x2b820' <= c && c <= '\x2cea1' ||
        -- CJK Unified Ideographs Extension F
        '\x2ceb0' <= c && c <= '\x2ebe0' ||
        -- CJK Compatibility Ideographs Supplement
        '\x2f800' <= c && c <= '\x2fa1f'

    unnamedCharRef :: Parser Char
    unnamedCharRef = do
        _ <- char '&'
        _ <- char '#'
        hex <- option False ((char 'x' <|> char 'X') >> return True)
        codepoint <- if hex then hexadecimal else decimal
        _ <- char ';'
        return $ chr codepoint

-- | The Initial Sound Law (頭音法則) table according to South Korean
-- /Hangul Orthography/ (한글 맞춤법) Clause 5, Section 52, Chapter 6
-- (第6章52項5節).  Keys are an original Sino-Korean sound and values
-- are a converted sound according to the law.
initialSoundLawTable :: Map Char Char
initialSoundLawTable =
    [ ('녀', '여')
    , ('뇨', '요')
    , ('뉴', '유')
    , ('니', '이')
    , ('랴', '야')
    , ('려', '여')
    , ('례', '예')
    , ('료', '요')
    , ('류', '유')
    , ('리', '이')
    , ('라', '나')
    , ('래', '내')
    , ('로', '노')
    , ('뢰', '뇌')
    , ('루', '누')
    , ('르', '느')
    ]

-- | Contains the same contents to 'initialSoundLawTable' except that
-- keys and values are crossed: keys are a converted sound and values are
-- possible original sounds.
initialSoundLawTable' :: Map Char (Set Char)
initialSoundLawTable' =
    foldrWithKey f Data.Map.Strict.empty initialSoundLawTable
  where
    f :: Char -> Char -> Map Char (Set Char) -> Map Char (Set Char)
    f original converted =
        insertWith Data.Set.union converted (Data.Set.singleton original)
