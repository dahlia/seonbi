{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | A trie from 'Text' keys to values.
module Text.Seonbi.Trie
    ( Trie
    , elems
    , empty
    , fromList
    , insert
    , keys
    , lookup
    , member
    , mergeBy
    , null
    , singleton
    , size
    , toList
    , unionL
    , unionR
    ) where

import Prelude hiding (lookup, null)

import Control.Monad (ap)
import qualified GHC.Exts

import Data.ByteString (ByteString)
import Data.Text hiding (empty, null, singleton)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Trie as BTrie

-- | A trie from 'Text' keys to 'a' values.
newtype Trie a
  = Trie (BTrie.Trie a)
  deriving (Eq, Show)

encodeKey :: Text -> ByteString
encodeKey = encodeUtf8

decodeKey :: ByteString -> Text
decodeKey = decodeUtf8

-- | The empty trie.
empty :: Trie a
empty = Trie BTrie.empty

-- | Checks if the trie is empty.
null :: Trie a -> Bool
null (Trie btrie) = BTrie.null btrie

-- | Constructs a singleton trie.
singleton :: Text -> a -> Trie a
singleton key value = Trie $ BTrie.singleton (encodeKey key) value

-- | Gets the number of elements in the trie.
size :: Trie a -> Int
size (Trie btrie) = BTrie.size btrie

fromList' :: [(Text, a)] -> Trie a
fromList' list = Trie $ BTrie.fromList [(encodeKey k, v) | (k, v) <- list]

toList' :: Trie a -> [(Text, a)]
toList' (Trie btrie) = [(decodeKey k, v) | (k, v) <- BTrie.toList btrie]

-- | Converts a list of associated pairs into a trie.  For duplicate keys,
-- values earlier in the list shadow later ones.
fromList :: [(Text, a)] -> Trie a
fromList = fromList'

-- | Converts a trie into a list of associated pairs.  Keys will be ordered.
toList :: Trie a -> [(Text, a)]
toList = toList'

-- | Lists all keys in the trie.  Keys will be ordered.
keys :: Trie a -> [Text]
keys (Trie btrie) = Prelude.map decodeKey $ BTrie.keys btrie

-- | Lists all values in the trie.  Values are ordered by their associated keys.
elems :: Trie a -> [a]
elems (Trie btrie) = BTrie.elems btrie

-- | Gets the value associated with a key if it exists.
lookup :: Text -> Trie a -> Maybe a
lookup key (Trie btrie) = BTrie.lookup (encodeKey key) btrie

-- | Checks if a key has a value in the trie.
member :: Text -> Trie a -> Bool
member key (Trie btrie) = BTrie.member (encodeKey key) btrie

-- | Inserts a new key into the trie.
insert
    :: Text
    -- ^ A new key to insert.  If there is already the same key in the trie,
    -- the existing value is overwritten by the new value.
    -> a
    -- ^ A value associated to the key.
    -> Trie a
    -- ^ An existing trie.
    -> Trie a
    -- ^ The new trie with the inserted key.
insert key value (Trie btrie) = Trie $ BTrie.insert (encodeKey key) value btrie

-- | Combines two tries, using a function to resolve collisions.  This can only
-- define the space of functions between union and symmetric difference but,
-- with those two, all set operations can be defined (albeit inefficiently).
mergeBy :: (a -> a -> Maybe a) -> Trie a -> Trie a -> Trie a
mergeBy f (Trie a) (Trie b) = Trie $ BTrie.mergeBy f a b

-- | Combines two tries, resolving conflicts by choosing the value from the
-- left (former) trie.
unionL :: Trie a -> Trie a -> Trie a
unionL (Trie left) (Trie right) = Trie $ BTrie.unionL left right

-- | Combines two tries, resolving conflicts by choosing the value from the
-- right (latter) trie.
unionR :: Trie a -> Trie a -> Trie a
unionR (Trie left) (Trie right) = Trie $ BTrie.unionR left right

instance Functor Trie where
    fmap f (Trie btrie) = Trie $ fmap f btrie

instance Foldable Trie where
    foldMap f (Trie btrie) = foldMap f btrie

instance Traversable Trie where
    traverse f (Trie btrie) = Trie <$> traverse f btrie

instance Applicative Trie where
    pure = singleton ""
    (<*>) = ap

instance Monad Trie where
    Trie btrie >>= f = Trie $ btrie >>= (\ v -> case f v of { Trie b -> b })

instance (Semigroup a) => Semigroup (Trie a) where
    (Trie a) <> (Trie b) = Trie (a <> b)

instance (Monoid a) => Monoid (Trie a) where
    mempty = Trie mempty

instance GHC.Exts.IsList (Trie a) where
    type Item (Trie a) = (Text, a)
    fromList = fromList'
    toList = toList'
