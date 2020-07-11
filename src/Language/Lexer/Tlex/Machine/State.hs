module Language.Lexer.Tlex.Machine.State (
    StateNum,
    initialStateNum,

    StateSet,
    emptySet,
    singletonSet,
    listToSet,
    setToList,
    nullSet,
    insertSet,
    intersectSet,
    diffSet,
    unionSet,
    lengthSet,
    memberSet,

    StateMap,
    emptyMap,
    insertOrUpdateMap,
    insertMap,
    lookupMap,
    assocsMap,

    StateArray,
    totalStateMapToArray,
    mapArrayWithIx,
    indexArray,
    arrayAssocs,

    StateGraph,
    stateArrayToGraph,
    liftGraphOp,
    indexGraph,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.Array                  as Array
import qualified Data.Graph                  as Graph
import qualified Data.Hashable               as Hashable
import qualified Data.IntMap.Strict          as IntMap
import qualified Data.IntSet                 as IntSet


newtype StateNum = StateNum Int
    deriving (Eq, Ord, Show, Ix)
    deriving (Hashable.Hashable, Enum) via Int

initialStateNum :: StateNum
initialStateNum = StateNum 0


newtype StateSet = StateSet IntSet.IntSet
    deriving (Eq, Show)

instance Hashable.Hashable StateSet where
    hashWithSalt s (StateSet x) = Hashable.hashWithSalt s do IntSet.toAscList x

emptySet :: StateSet
emptySet = StateSet IntSet.empty

singletonSet :: StateNum -> StateSet
singletonSet (StateNum s) = StateSet do IntSet.singleton s

insertSet :: StateNum -> StateSet -> StateSet
insertSet (StateNum s) (StateSet ss) = StateSet do IntSet.insert s ss

listToSet :: [StateNum] -> StateSet
listToSet ss = StateSet do IntSet.fromList do coerce ss

setToList :: StateSet -> [StateNum]
setToList (StateSet ss) = coerce do IntSet.toList ss

nullSet :: StateSet -> Bool
nullSet (StateSet ss) = IntSet.null ss

intersectSet :: StateSet -> StateSet -> StateSet
intersectSet (StateSet ss1) (StateSet ss2) = StateSet do IntSet.intersection ss1 ss2

diffSet :: StateSet -> StateSet -> StateSet
diffSet (StateSet ss1) (StateSet ss2) = StateSet do IntSet.difference ss1 ss2

unionSet :: StateSet -> StateSet -> StateSet
unionSet (StateSet ss1) (StateSet ss2) = StateSet do IntSet.union ss1 ss2

lengthSet :: StateSet -> Int
lengthSet (StateSet ss) = IntSet.size ss

memberSet :: StateNum -> StateSet -> Bool
memberSet (StateNum s) (StateSet ss) = IntSet.member s ss


newtype StateMap a = StateMap (IntMap.IntMap a)
    deriving (Eq, Show, Functor)

emptyMap :: StateMap a
emptyMap = StateMap IntMap.empty

insertMap :: StateNum -> a -> StateMap a -> StateMap a
insertMap (StateNum k) x (StateMap m) = StateMap do IntMap.insert k x m

insertOrUpdateMap :: StateNum -> a -> (a -> a) -> StateMap a -> StateMap a
insertOrUpdateMap (StateNum k) ~dx ~uf (StateMap m) = StateMap case IntMap.lookup k m of
    Nothing -> IntMap.insert k dx m
    Just x  -> IntMap.insert k (uf x) m

lookupMap :: StateNum -> StateMap a -> Maybe a
lookupMap (StateNum sn) (StateMap m) = IntMap.lookup sn m

assocsMap :: StateMap a -> [(StateNum, a)]
assocsMap (StateMap m) = coerce do IntMap.assocs m


newtype StateArray a = StateArray (Array.Array Int a)
    deriving (Eq, Show, Functor)

totalStateMapToArray :: StateNum -> StateMap a -> StateArray a
totalStateMapToArray (StateNum boundState) (StateMap m) = StateArray
    do Array.array (0, pred boundState) do IntMap.toAscList m

mapArrayWithIx :: (StateNum -> a -> a) -> StateArray a -> StateArray a
mapArrayWithIx f (StateArray arr) = StateArray
    do Array.listArray
        do Array.bounds arr
        do [ f (StateNum i) x | (i, x) <- Array.assocs arr ]

indexArray :: StateArray a -> StateNum -> a
indexArray (StateArray arr) (StateNum i) = arr Array.! i

arrayAssocs :: StateArray a -> [(StateNum, a)]
arrayAssocs (StateArray arr) = coerce do Array.assocs arr


newtype StateGraph = StateGraph Graph.Graph

stateArrayToGraph :: StateArray [StateNum] -> StateGraph
stateArrayToGraph (StateArray m) = StateGraph do coerce m

liftGraphOp :: (Graph.Graph -> Graph.Graph) -> StateGraph -> StateGraph
liftGraphOp f x = coerce f x

indexGraph :: StateGraph -> StateNum -> [StateNum]
indexGraph (StateGraph x) (StateNum i) = coerce do x Array.! i
