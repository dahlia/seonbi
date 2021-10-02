{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
#ifdef EMBED_DICTIONARY
{-# LANGUAGE TemplateHaskell #-}
#endif
-- | Provides higher-level APIs.  Read 'transformHtmlText' function first,
-- and then see also 'Configuration' type.
module Text.Seonbi.Facade
    ( -- * HTML transformation
      transformHtmlText
    , transformHtmlLazyText
      -- * Configuration and presets
    , Configuration (..)
    , ko_KP
    , ko_KR
    , presets
      -- * Dictionaries
    , HanjaDictionary
    , readDictionaryFile
    , southKoreanDictionary
      -- * Options
    , ArrowOption (..)
    , CiteOption (..)
    , HanjaOption (..)
    , HanjaReadingOption (..)
    , HanjaRenderingOption (..)
    , QuoteOption (..)
    , StopOption (..)
    ) where

#if MIN_VERSION_base(4,13,0)
import Prelude hiding (MonadFail)
#endif

import Control.Monad.Fail (MonadFail)
import Data.Char
import Data.Maybe
import Data.String (IsString)
import GHC.Exts (IsList (toList))
import GHC.Generics (Generic)
import System.IO.Error
import System.IO.Unsafe

import Data.ByteString.Lazy
import Data.Csv
#ifdef EMBED_DICTIONARY
import Data.FileEmbed
#endif
import Data.Map.Strict
import Data.Set
import Data.Text
import qualified Data.Text.Lazy as LT
import System.FilePath
    ( (</>)
#ifdef EMBED_DICTIONARY
    , takeDirectory
#endif
    )

#ifndef EMBED_DICTIONARY
import Paths_seonbi (getDataDir)
#endif
import Text.Seonbi.Hanja
import Text.Seonbi.Html
import Text.Seonbi.Punctuation
import Text.Seonbi.Trie as Trie

-- | Transformation settings.  For the most cases, you could use one of
-- presets:
--
-- - 'ko_KR'
-- - 'ko_KP'
data Monad m => Configuration m a = Configuration
    { -- | An optional debugging logger to print its internal AST.
      debugLogger :: Maybe (HtmlEntity -> m a)
      -- | Whether to take and result in XHTML instead of HTML.
    , xhtml :: Bool
      -- | An option to decide how quotation marks are rendered.
      -- If 'Nothing' no quotes are transformed.
    , quote :: Maybe QuoteOption
      -- | An option to transform folk-citing quotes (e.g., @\<\<한겨레\>\>@)
      -- into proper citing quotes (e.g., @《한겨레》@).
    , cite :: Maybe CiteOption
      -- | Settings to transform arrow-looking punctuations into proper arrows.
      -- If 'Nothing' no arrows are transformed.
    , arrow :: Maybe ArrowOption
      -- | Whether to transform triple periods into a proper ellipsis.
    , ellipsis :: Bool
      -- | Whether to transform folk em dashes into proper em dashes.
    , emDash :: Bool
      -- | Settings to normalize stops (periods, commas, and interpuncts).
      -- If 'Nothing' stops are never touched.
    , stop :: Maybe StopOption
      -- | Settings to deal with Sino-Korean words.
    , hanja :: Maybe HanjaOption
    }

instance Monad m => Show (Configuration m a) where
    show c = "Configuration {\n" <>
        "  debugLogger = " <>
            maybe "Nothing" (const "Just ...") (debugLogger c) <> "," <>
        "  xhtml = " <> show (xhtml c) <> "," <>
        "  quote = " <> show (quote c) <> "," <>
        "  arrow = " <> show (cite c) <> "," <>
        "  cite = " <> show (arrow c) <> "," <>
        "  ellipsis = " <> show (ellipsis c) <> "," <>
        "  emDash = " <> show (emDash c) <> "," <>
        "  stop = " <> show (stop c) <> "," <>
        "  hanja = " <> show (hanja c) <>
        "}"

-- | An option to decide how quotation marks are rendered.
data QuoteOption
    -- | English-style curved quotes (@‘@: U+2018, @’@: U+2019, @“@: U+201C,
    -- @”@: U+201D), which are used by South Korean orthography.
    = CurvedQuotes
    -- | Vertical corner brackets (@﹁@: U+FE41, @﹂@: U+FE42, @﹃@: U+FE43,
    -- @﹄@: U+FE44), which are used by East Asian orthography.
    | VerticalCornerBrackets
    -- | Traditional horizontal corner brackets (@「@: U+300C, @」@: U+300D,
    -- @『@: U+300E, @』@: U+300F), which are used by East Asian orthography.
    | HorizontalCornerBrackets
    -- | East Asian guillemets (@〈@: U+3008, @〉@: U+3009, @《@: U+300A, @》@:
    -- U+300B), which are used by North Korean orthography.
    | Guillemets
    -- | Use English-style curved quotes (@‘@: U+2018, @’@: U+2019) for single
    -- quotes, and HTML @\<q\>@ tags for double quotes.
    | CurvedSingleQuotesWithQ
    -- | Use vertical corner brackets (@﹁@: U+FE41, @﹂@: U+FE42)
    -- for single quotes, and HTML @\<q\>@ tags for double quotes.
    | VerticalCornerBracketsWithQ
    -- | Use horizontal corner brackets (@「@: U+300C, @」@: U+300D)
    -- for single quotes, and HTML @\<q\>@ tags for double quotes.
    | HorizontalCornerBracketsWithQ
    deriving (Enum, Eq, Generic, Read, Show)

-- | An option to transform folk-citing quotes (e.g., @\<\<한겨레\>\>@) into
-- proper citing quotes (e.g., @《한겨레》@).
data CiteOption
    -- | Cite a title using angle quotes, used by South Korean orthography in
    -- horizontal writing (橫書), e.g., 《나비와 엉겅퀴》 or 〈枾崎의 바다〉.
    = AngleQuotes
    -- | Cite a title using corner brackets, used by South Korean orthography in
    -- vertical writing (縱書) and Japanese orthography,
    -- e.g., 『나비와 엉겅퀴』 or 「枾崎의 바다」.
    | CornerBrackets
    -- | Same as 'AngleQuotes' except it wraps the title with a @\<cite\>@ tag.
    | AngleQuotesWithCite
    -- | Same as 'CornerBrackets' except it wraps the title with
    -- a @\<cite\>@ tag.
    | CornerBracketsWithCite
    deriving (Enum, Eq, Generic, Read, Show)

-- | Settings to transform arrow-looking punctuations into proper arrows.
data ArrowOption = ArrowOption
    { -- | Whether to transform bi-directional arrows as well as
      -- left/rightwards arrows.
      bidirArrow :: Bool
      -- | Whether to transform double arrows as well as single arrows.
    , doubleArrow :: Bool
    } deriving (Eq, Generic, Show)

-- | Settings to normalize stops (periods, commas, and interpuncts) in docs.
data StopOption
    -- | Stop sentences in the modern Korean style which follows Western stops.
    -- E.g.:
    --
    -- > 봄·여름·가을·겨울. 어제, 오늘.
    = Horizontal
    -- | Similar to 'horizontalStops' except slashes are used instead of
    -- interpuncts. E.g.:
    --
    -- > 봄/여름/가을/겨울. 어제, 오늘.
    | HorizontalWithSlashes
    -- | Stop sentences in the pre-modern Korean style which follows Chinese
    -- stops.  E.g.:
    --
    -- > 봄·여름·가을·겨울。어제、오늘。
    | Vertical
    deriving (Enum, Eq, Generic, Read, Show)

-- | Settings to deal with Sino-Korean words.
data HanjaOption = HanjaOption
    { -- | How to render Sino-Korean words.
      rendering :: HanjaRenderingOption
      -- | How to rewrite Sino-Korean words in hangul.
    , reading :: HanjaReadingOption
    } deriving (Show)

-- | Available options to render Sino-Korean words.
data HanjaRenderingOption
    -- | Renders a word in hangul-only, no hanja at all (e.g., @안녕히@).
    = HangulOnly
    -- | Renders a word in hangul followed by hanja in parentheses
    -- (e.g., @안녕(安寧)히@).
    | HanjaInParentheses
    -- | Renders words in hangul-only for the most part, and if there are
    -- homophones in a document put their hanja notation in parentheses
    -- (e.g., @안녕히@ or @소수(小數)와 소수(素數)@).
    | DisambiguatingHanjaInParentheses
    -- | Renders a word in @<ruby>@ tag (e.g.,
    -- @\<ruby\>安寧\<rp\>(\<\/rp\>\<rt\>안녕\<\/rt\>\<rp\>)\<\/rp\>\<\/ruby\>히@).
    --
    -- Please read [Use Cases & Exploratory Approaches for Ruby
    -- Markup](https://www.w3.org/TR/ruby-use-cases/) as well for more
    -- information.
    | HanjaInRuby
    deriving (Enum, Eq, Generic, Read, Show)

-- | Settings to read Sino-Korean words.
data HanjaReadingOption = HanjaReadingOption
    { -- | Whether to apply Initial Sound Law (頭音法則) or not.
      initialSoundLaw :: Bool
      -- | A dictionary which has hanja readings.  Keys are
      -- hanja words and values are their corresponding hangul readings,
      -- e.g.:
      --
      -- > [("敗北", "패배"), ("北極", "북극")] :: HanjaDictionary
    , dictionary :: HanjaDictionary
    }

instance Show HanjaReadingOption where
    show HanjaReadingOption { dictionary, initialSoundLaw } =
        "HanjaReadingOption {" <>
        " dictionary = [" <>
        show (Trie.size dictionary) <>
        " words]," <>
        " initialSoundLaw = " <>
        show initialSoundLaw <>
        " }"

-- | Transforms a given HTML text.  'Nothing' if it fails to parse the given
-- HTML text.
transformHtmlText :: forall (m :: * -> *) a. (Monad m, MonadFail m)
                  => Configuration m a -> Text -> m Text
transformHtmlText config =
    fmap LT.toStrict . transformHtmlLazyText config . LT.fromStrict

-- | A lazy version of 'transformHtmlText' function.
transformHtmlLazyText :: forall (m :: * -> *) a. (Monad m, MonadFail m)
                      => Configuration m a -> LT.Text -> m LT.Text
transformHtmlLazyText config@Configuration { xhtml, debugLogger } htmlText =
    case scanHtml htmlText of
        Done "" input -> do
            case debugLogger of
                Just logger -> mapM_ logger input
                Nothing -> return ()
            return $ printHtml' $ toTransformer config input
        _ ->
            fail "failed to parse input"
  where
    printHtml' :: [HtmlEntity] -> LT.Text
    printHtml'
      | xhtml = printXhtml
      | otherwise = printHtml

toTransformers :: Monad m => Configuration m a -> [[HtmlEntity] -> [HtmlEntity]]
toTransformers Configuration { quote
                             , cite
                             , arrow
                             , ellipsis
                             , emDash
                             , stop
                             , hanja
                             } =
    [ case quote of
        Nothing -> id
        Just quoteOption -> transformQuote $
            case quoteOption of
                CurvedQuotes -> curvedQuotes
                Guillemets -> guillemets
                VerticalCornerBrackets -> verticalCornerBrackets
                HorizontalCornerBrackets -> horizontalCornerBrackets
                CurvedSingleQuotesWithQ -> curvedSingleQuotesWithQ
                VerticalCornerBracketsWithQ -> verticalCornerBracketsWithQ
                HorizontalCornerBracketsWithQ -> horizontalCornerBracketsWithQ
    , case cite of
        Nothing -> id
        Just citeOption -> quoteCitation $
            case citeOption of
                AngleQuotes -> angleQuotes { htmlElement = Nothing }
                CornerBrackets -> cornerBrackets { htmlElement = Nothing }
                AngleQuotesWithCite -> angleQuotes
                CornerBracketsWithCite -> cornerBrackets
    , case arrow of
        Nothing -> id
        Just ArrowOption { bidirArrow, doubleArrow } -> transformArrow $
            Data.Set.fromList $ catMaybes
                [ if bidirArrow then Just LeftRight else Nothing
                , if doubleArrow then Just DoubleArrow else Nothing
                ]
    , case stop of
        Nothing -> id
        Just stopOption -> normalizeStops $
            case stopOption of
                Horizontal -> horizontalStops
                HorizontalWithSlashes -> horizontalStopsWithSlashes
                Vertical -> verticalStops
    , if ellipsis then transformEllipsis else id
    , if emDash then transformEmDash else id
    , case hanja of
        Nothing ->
            id
        Just HanjaOption
                { rendering
                , reading = HanjaReadingOption { initialSoundLaw, dictionary }
                } ->
            phoneticizeHanja $ def
                { phoneticizer =
                    let withDict = if Trie.null dictionary
                            then id
                            else withDictionary dictionary
                        phoneticize = if initialSoundLaw
                            then phoneticizeHanjaWordWithInitialSoundLaw
                            else phoneticizeHanjaWord
                    in
                        withDict phoneticize
                , wordRenderer = case rendering of
                    HangulOnly -> hangulOnly
                    HanjaInParentheses -> hanjaInParentheses
                    DisambiguatingHanjaInParentheses -> hangulOnly
                    HanjaInRuby -> hanjaInRuby
                , homophoneRenderer = case rendering of
                    HangulOnly -> hangulOnly
                    HanjaInParentheses -> hanjaInParentheses
                    DisambiguatingHanjaInParentheses -> hanjaInParentheses
                    HanjaInRuby -> hanjaInRuby
                }
    ]

toTransformer :: Monad m => Configuration m a -> [HtmlEntity] -> [HtmlEntity]
toTransformer =
    Prelude.foldl (.) id . toTransformers

-- | Preset 'Configuration' for South Korean orthography.
ko_KR :: Monad m => Configuration m a
ko_KR = Configuration
    { debugLogger = Nothing
    , quote = Just CurvedQuotes
    , cite = Just AngleQuotes
    , arrow = Just ArrowOption { bidirArrow = True, doubleArrow = True }
    , ellipsis = True
    , emDash = True
    , stop = Just Horizontal
    , hanja = Just HanjaOption
        { rendering = DisambiguatingHanjaInParentheses
        , reading = HanjaReadingOption
            { dictionary = southKoreanDictionaryUnsafe
            , initialSoundLaw = True
            }
        }
    , xhtml = False
    }

-- | Preset 'Configuration' for North Korean orthography.
ko_KP :: Monad m => Configuration m a
ko_KP = ko_KR
    { quote = Just Guillemets
    , hanja = Just HanjaOption
        { rendering = HangulOnly
        , reading = HanjaReadingOption
            { dictionary = []
            , initialSoundLaw = False
            }
        }
    }

-- | A mapping of locale code strings (e.g., @"ko-kr"@) to the corresponding
-- 'Configuration' presets (e.g., 'ko_KR').
presets :: (Ord k, IsString k, Monad m) => Map k (Configuration m a)
presets =
    [ ("ko-kp", ko_KP)
    , ("ko-kr", ko_KR)
    ]

-- | Loads a dictionary file.  The file consists of two-column TSV
-- (tab-separated values); the first column is hanja and the second column is
-- hangul.
readDictionaryFile :: FilePath -> IO HanjaDictionary
readDictionaryFile path = do
    byteString <- Data.ByteString.Lazy.readFile path
    case readDictionaryByteString byteString of
        Right dic -> return dic
        Left err -> fail err

-- | Reads a dictionary from TSV bytes.
readDictionaryByteString :: Data.ByteString.Lazy.ByteString
                         -> Either String HanjaDictionary
readDictionaryByteString byteString =
    case decodeWith tsvDecodeOptions NoHeader byteString of
        Right vector -> Right $ Prelude.foldl
            (\ d (DictionaryPair k v) -> Trie.insert k v d)
            Trie.empty
            (GHC.Exts.toList vector)
        Left err -> Left err
  where
    tsvDecodeOptions :: DecodeOptions
    tsvDecodeOptions = defaultDecodeOptions
        { decDelimiter = fromIntegral (ord '\t')
        }

{-# NOINLINE southKoreanDictionaryUnsafe #-}
southKoreanDictionaryUnsafe :: HanjaDictionary
southKoreanDictionaryUnsafe =
    unsafePerformIO $ ignoreError southKoreanDictionary
  where
    ignoreError :: IO HanjaDictionary -> IO HanjaDictionary
    ignoreError action =
        catchIOError action $ const $ return Trie.empty

-- | Loads [Standard Korean Language Dictionary](https://stdict.korean.go.kr/)
-- (標準國語大辭典) data.
southKoreanDictionary :: IO HanjaDictionary
#ifdef EMBED_DICTIONARY
southKoreanDictionary =
    case readDictionaryByteString bytes of
        Right dic -> return dic
        Left err -> fail err
  where
    bytes :: Data.ByteString.Lazy.ByteString
    bytes = Data.ByteString.Lazy.fromStrict $ $(embedFile $
        takeDirectory __FILE__ </> ".." </> ".." </> ".." </> "data" </>
        "ko-kr-stdict.tsv")

#else
southKoreanDictionary = do
    dataDir <- getDataDir
    readDictionaryFile (dataDir </> "ko-kr-stdict.tsv")
#endif

data DictionaryPair = DictionaryPair !Text !Text deriving (Generic, Show)

instance FromRecord DictionaryPair

{- HLINT ignore "Use camelCase" -}
