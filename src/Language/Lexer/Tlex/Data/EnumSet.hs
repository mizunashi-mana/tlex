module Language.Lexer.Tlex.Data.EnumSet (
    EnumSet,
    empty,
    singleton,
    union,
    intersection,
    difference,
    fromList,
    toList,
    toIntSet,
) where

import           Prelude

import qualified Data.Hashable as Hashable
import qualified Data.IntSet   as IntSet


newtype EnumSet a = EnumSet IntSet.IntSet
    deriving (Eq, Show)

instance Hashable.Hashable (EnumSet a) where
    hashWithSalt s (EnumSet x) = Hashable.hashWithSalt s do IntSet.toAscList x

empty :: Enum a => EnumSet a
empty = EnumSet IntSet.empty

singleton :: Enum a => a -> EnumSet a
singleton x = EnumSet do IntSet.singleton do fromEnum x

union :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
union (EnumSet s1) (EnumSet s2) = EnumSet do IntSet.union s1 s2

intersection :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
intersection (EnumSet s1) (EnumSet s2) = EnumSet do IntSet.intersection s1 s2

difference :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
difference (EnumSet s1) (EnumSet s2) = EnumSet do IntSet.difference s1 s2

fromList :: Enum a => [a] -> EnumSet a
fromList xs = EnumSet do IntSet.fromList [ fromEnum x | x <- xs ]

toList :: Enum a => EnumSet a -> [a]
toList (EnumSet xs) = [ toEnum x | x <- IntSet.toList xs ]

toIntSet :: Enum a => EnumSet a -> IntSet.IntSet
toIntSet (EnumSet m) = m
