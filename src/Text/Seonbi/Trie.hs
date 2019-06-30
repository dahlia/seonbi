{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
-- | This re-exports 'Data.Trie.Text'.
module Text.Seonbi.Trie
    ( module Data.Trie.Text
    ) where

import GHC.Exts

import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Trie.Text

instance IsList (Trie a) where
    type Item (Trie a) = (Text, a)
    fromList = Data.Trie.Text.fromList
    toList trie = [(toStrict k, v) | (k, v) <- Data.Trie.Text.toList trie]
