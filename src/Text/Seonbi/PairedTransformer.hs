{-# LANGUAGE ScopedTypeVariables #-}
module Text.Seonbi.PairedTransformer
    ( PairedTransformer (..)
    , transformPairs
    ) where

import Data.Text hiding (break, reverse)

import Text.Seonbi.Html

-- | Settings for 'transformPairs'.
data PairedTransformer match = PairedTransformer
    { ignoresTagStack :: HtmlTagStack -> Bool
    , matchStart :: [match] -> Text -> Maybe (match, Text, Text, Text)
    , matchEnd :: Text -> Maybe (match, Text, Text, Text)
    , areMatchesPaired :: match -> match -> Bool
    , transformPair :: match -> match -> [HtmlEntity] -> [HtmlEntity]
    }

-- | Some transformations should be done only if a start and an end are paired
-- like parentheses.  These even usually can be nested.  Even if there is
-- a start and an end they should not be paired unless they are sibling in
-- an HTML tree.
--
-- These kinds of scanning are easily turned highly stateful and imperative,
-- hence hard to debug.  This base class provides the common logic between
-- these kinds of paired transformations so that an implementation class fill
-- several abstract methods triggered by the state machine.
transformPairs :: forall m . PairedTransformer m -> [HtmlEntity] -> [HtmlEntity]
transformPairs (PairedTransformer ignores start end arePaired transform) =
    iter [] . normalizeText
  where
    iter :: [Unclosed m] -> [HtmlEntity] -> [HtmlEntity]
    iter [] [] = []
    iter stack [] = unstack stack
    iter stack (x@HtmlText { tagStack = ts, rawText = txt } : xs) =
        case (startMatch, endMatch) of
            (Just captured, Nothing) ->
                roll stack captured ts xs
            (Nothing, Just captured@(m, _, _, _))
              | Prelude.any ((`arePaired` m) . match) stack ->
                    unroll stack captured ts xs
            (Just captured@(_, pre, _, _), Just captured'@(m', pre', _, _)) ->
                if Data.Text.length pre >= Data.Text.length pre' &&
                    Prelude.any ((`arePaired` m') . match) stack
                then unroll stack captured' ts xs
                else roll stack captured ts xs
            (Nothing, _) ->
                case stack of
                    [] -> x : iter stack xs
                    s : ss -> iter (s { buffer = x : buffer s } : ss) xs
      where
        startMatch :: Maybe (m, Text, Text, Text)
        startMatch = start (reverse $ fmap match stack) txt
        endMatch :: Maybe (m, Text, Text, Text)
        endMatch = end txt
    iter (s@Unclosed {} : ss) (x : xs) =
        iter (s { buffer = x : buffer s } : ss) xs
    iter [] (x : xs) = x : iter [] xs
    roll :: [Unclosed m]
         -> (m, Text, Text, Text)
         -> HtmlTagStack
         -> [HtmlEntity]
         -> [HtmlEntity]
    roll [] (startMatch, pre, t, post) tagStack_ entities =
        prependText tagStack_ pre $ iter
            [Unclosed startMatch [HtmlText tagStack_ t]]
            (normalizeText (prependText tagStack_ post entities))
    roll (s : ss) (startMatch, pre, t, post) tagStack_ entities = iter
        ( Unclosed startMatch [HtmlText tagStack_ t]
        : s { buffer = prependText tagStack_ pre $ buffer s }
        : ss
        )
        (normalizeText (prependText tagStack_ post entities))
    unroll :: [Unclosed m]
           -> (m, Text, Text, Text)
           -> HtmlTagStack
           -> [HtmlEntity]
           -> [HtmlEntity]
    unroll stack (endMatch, pre, t, post) tagStack_ es =
        case remainStack of
            [] -> unrolled ++ iter [] remainEntities
            s : ss -> iter
                (s { buffer = reverse unrolled ++ buffer s } : ss)
                remainEntities
      where
        prependText' :: Text -> [HtmlEntity] -> [HtmlEntity]
        prependText' = prependText tagStack_
        unrolled :: [HtmlEntity]
        remainStack :: [Unclosed m]
        (unrolled, remainStack) = case findPair endMatch stack of
            (_, []) ->
                ([HtmlText tagStack_ (pre `append` t)], [])
            (stack', s@Unclosed { match = startMatch } : ss) ->
                let
                    buf = prependText' pre (unstack' stack' ++ buffer s)
                    buf' = prependText' t buf
                    buf'' = reverse buf'
                    transformed = if Prelude.any (ignores . tagStack) buf''
                       then buf''
                       else transform startMatch endMatch buf''
                in
                    (transformed, ss)
        remainEntities :: [HtmlEntity]
        remainEntities = prependText' post es
    findPair :: m -> [Unclosed m] -> ([Unclosed m], [Unclosed m])
    findPair m = break (arePaired m . match)
    unstack :: [Unclosed m] -> [HtmlEntity]
    unstack = reverse . unstack'
    unstack' :: [Unclosed m] -> [HtmlEntity]
    unstack' [] = []
    unstack' (Unclosed { buffer = b } : ss) = b ++ unstack' ss
    prependText :: HtmlTagStack -> Text -> [HtmlEntity] -> [HtmlEntity]
    prependText tagStack_ txt
      | Data.Text.null txt = id
      | otherwise = (HtmlText tagStack_ txt :)

data Unclosed match = Unclosed
    { match :: match
    , buffer :: [HtmlEntity] -- in reverse order
    }
