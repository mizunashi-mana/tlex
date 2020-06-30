module Language.Lexer.Tlex.Data.EnumSet (
    EnumSet,
    empty,
    singleton,
    fromList,
    toList,
) where

import Prelude

import qualified Data.Hashable as Hashable
import qualified Data.IntSet as IntSet


newtype EnumSet a = EnumSet IntSet.IntSet
    deriving (Eq, Show)

instance Hashable.Hashable (EnumSet a) where
    hashWithSalt s (EnumSet x) = Hashable.hashWithSalt s do IntSet.toAscList x

empty :: EnumSet a
empty = EnumSet IntSet.empty

singleton :: Enum a => a -> EnumSet a
singleton x = EnumSet do IntSet.singleton do fromEnum x

fromList :: Enum a => [a] -> EnumSet a
fromList xs = EnumSet do IntSet.fromList [ fromEnum x | x <- xs ]

toList :: Enum a => EnumSet a -> [a]
toList (EnumSet xs) = [ toEnum x | x <- IntSet.toList xs ]
