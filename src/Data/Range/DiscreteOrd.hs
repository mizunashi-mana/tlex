module Data.Range.DiscreteOrd
    (
        DiscreteOrd,
        NeighboredOrdering (..),
        neighboredCompare,
        flipNeighboredOrdering,
    ) where

import Prelude


type DiscreteOrd a = (Ord a, Enum a)

data NeighboredOrdering
    = FarLessThan
    | NeighborLessThan
    | NeighborEqual
    | NeighborGreaterThan
    | FarGreaterThan
    deriving (Eq, Ord, Enum, Show)

flipNeighboredOrdering :: NeighboredOrdering -> NeighboredOrdering
flipNeighboredOrdering = \case
    FarLessThan         -> FarGreaterThan
    NeighborLessThan    -> NeighborGreaterThan
    NeighborEqual       -> NeighborEqual
    NeighborGreaterThan -> NeighborLessThan
    FarGreaterThan      -> FarLessThan

-- |
--
-- >>> neighboredCompare 0 0
-- NeighborEqual
-- >>> neighboredCompare 0 1
-- NeighborLessThan
-- >>> neighboredCompare 0 10
-- FarLessThan
--
-- prop> neighboredCompare x y == flipNeighboredOrdering (neighboredCompare y x)
--
neighboredCompare :: DiscreteOrd a => a -> a -> NeighboredOrdering
neighboredCompare x y = case x `compare` y of
    EQ -> NeighborEqual
    LT
        | succ x == y -> NeighborLessThan
        | otherwise   -> FarLessThan
    GT
        | pred x == y -> NeighborGreaterThan
        | otherwise   -> FarGreaterThan
