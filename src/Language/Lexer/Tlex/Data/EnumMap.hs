module Language.Lexer.Tlex.Data.EnumMap (
    EnumMap,
    empty,
    insert,
    assocs,
    toDescList,
    lookup,
    insertOrUpdate,
    fromList,
    foldlWithKey',
    update,
    delete,
    singleton,
    unionWith,
) where

import           Prelude            hiding (lookup)

import qualified Data.IntMap.Strict as IntMap


newtype EnumMap k a = EnumMap (IntMap.IntMap a)
    deriving (Eq, Show)

empty :: Enum k => EnumMap k a
empty = EnumMap IntMap.empty

singleton :: Enum k => k -> a -> EnumMap k a
singleton k x = EnumMap do IntMap.singleton (fromEnum k) x

insert :: Enum k => k -> a -> EnumMap k a -> EnumMap k a
insert k x (EnumMap m) = EnumMap do IntMap.insert (fromEnum k) x m

assocs :: Enum k => EnumMap k a -> [(k, a)]
assocs (EnumMap m) = [ (toEnum i, x) | (i, x) <- IntMap.assocs m ]

toDescList :: Enum k => EnumMap k a -> [(k, a)]
toDescList (EnumMap m) = [ (toEnum i, x) | (i, x) <- IntMap.toDescList m ]

lookup :: Enum k => k -> EnumMap k a -> Maybe a
lookup k (EnumMap m) = IntMap.lookup (fromEnum k) m

insertOrUpdate :: Enum k => k -> a -> (a -> a) -> EnumMap k a -> EnumMap k a
insertOrUpdate k ~dx ~uf (EnumMap m) =
    let ik = fromEnum k
    in EnumMap case IntMap.lookup ik m of
        Nothing -> IntMap.insert ik dx m
        Just x  -> IntMap.insert ik (uf x) m

fromList :: Enum k => [(k, a)] -> EnumMap k a
fromList xs = EnumMap do IntMap.fromList [ (fromEnum i, x) | (i, x) <- xs ]

delete :: Enum k => k -> EnumMap k a -> EnumMap k a
delete k (EnumMap m) = EnumMap do IntMap.delete (fromEnum k) m

foldlWithKey' :: Enum k => (b -> k -> a -> b) -> b -> EnumMap k a -> b
foldlWithKey' f acc0 (EnumMap m) = IntMap.foldlWithKey' (\acc i x -> f acc (toEnum i) x) acc0 m

update :: Enum k => (a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
update f k (EnumMap m) = EnumMap do IntMap.update f (fromEnum k) m

unionWith :: Enum k => (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith f (EnumMap m1) (EnumMap m2) = EnumMap do IntMap.unionWith f m1 m2
