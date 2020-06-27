module Language.Lexer.Tlex.Data.CharMap (
    CharMap,
    empty,
    insert,
) where

import qualified Data.Char as Char
import qualified Data.IntMap as IntMap


type Key = Char.Char

newtype CharMap = CharMap IntMap.IntMap

empty :: CharMap a
empty = CharMap IntMap.empty

insert :: Key -> a -> CharMap a -> CharMap a
insert c x (CharMap m) = CharMap do IntMap.insert (Char.ord c) x m

traverseWithKey :: Applicative t => (Key -> a -> t b) -> CharMap a -> t (CharMap b)
traverseWithKey f (CharMap m) = fmap coerce do IntMap.traverseWithKey (\i x -> f (Char.chr i) x) m

assocs :: CharMap a -> [(Key, a)]
assocs (CharMap m) = [ (Char.chr i, x) | (i, x) <- IntMap.assocs m ]
