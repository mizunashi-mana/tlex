module Language.Lexer.Tlex.Machine.State (
    StateNum,
    initialStateNum,

    StateSet,
    emptyStateSet,

    StateMap,
    emptyStateMap,
    insertOrUpdateMap,

    StateArray,
    totalStateMapToArray,
    mapArrayWithIx,

    StateGraph,
    stateArrayToGraph,
    liftGraphOp,
    indexGraph,
) where

import Language.Lexer.Tlex.Prelude

import qualified Data.Hashable as Hashable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Array as Array
import qualified Data.Graph as Graph


newtype StateNum = StateNum Int
    deriving (Eq, Ord, Show, Enum, Ix, Hashable.Hashable)

initialStateNum :: StateNum
initialStateNum = StateNum 0


newtype StateSet = StateSet IntSet.IntSet
    deriving (Eq, Show)

emptyStateSet :: StateSet
emptyStateSet = StateSet IntSet.empty

instance Hashable.Hashable StateSet where
    hashWithSalt s (StateSet x) = Hashable.hashWithSalt s do IntSet.toAscList x


newtype StateMap a = StateMap (IntMap.IntMap a)
    deriving (Eq, Show, Functor)

emptyStateMap :: StateMap a
emptyStateMap = StateMap IntMap.empty

insertOrUpdateMap :: StateNum -> a -> (a -> a) -> StateMap a -> StateMap a
insertOrUpdateMap (StateNum k) ~dx ~uf (StateMap m) = StateMap case IntMap.lookup k m of
    Nothing -> IntMap.insert k dx m
    Just x  -> IntMap.insert k (uf x) m


newtype StateArray a = StateArray (Array.Array Int a)
    deriving (Eq, Show, Functor)

totalStateMapToArray :: StateNum -> StateMap a -> StateArray a
totalStateMapToArray (StateNum boundState) (StateMap m) = StateArray
    do Array.array (0, pred boundState) do IntMap.toAscList m

mapArrayWithIx :: (StateNum -> a -> a) -> StateArray a -> StateArray a
mapArrayWithIx f (StateArray arr) = StateArray $ Array.listArray
    do Array.bounds arr
    do [ f (StateNum i) x | (i, x) <- Array.assocs arr ]


newtype StateGraph = StateGraph Graph.Graph

stateArrayToGraph :: StateArray [StateNum] -> StateGraph
stateArrayToGraph (StateArray m) = StateGraph do coerce m

liftGraphOp :: (Graph.Graph -> Graph.Graph) -> StateGraph -> StateGraph
liftGraphOp f x = coerce f x

indexGraph :: StateGraph -> StateNum -> [StateNum]
indexGraph (StateGraph x) (StateNum i) = coerce do x Array.! i
