module Data.Range.DiscreteRangeSet (
    DiscreteRangeSet,
    singleton,
    rangeSet,
    complement,
    insert,
    elem,
    union,
    intersect,
) where

import Prelude hiding (elem)
import GHC.Exts
import Data.Foldable (foldl')

import Data.Range.DiscreteOrd
import qualified Data.Range.DiscreteRange as DiscreteRange
import           Data.Range.DiscreteRange (discreteRange, DiscreteRange)


data DiscreteRangeSet a
    = Straight (StRangeSet a)
    | Complement (StRangeSet a)
    deriving (Eq, Show)

rangeSet :: DiscreteOrd a => [DiscreteRange a] -> DiscreteRangeSet a
rangeSet rs = Straight $ stRangeSet rs

complement :: DiscreteOrd a => DiscreteRangeSet a -> DiscreteRangeSet a
complement = \case
    Straight rs -> Complement rs
    Complement rs -> Straight rs

empty :: DiscreteOrd a => DiscreteRangeSet a
empty = Straight mempty

singleton :: DiscreteOrd a => a -> DiscreteRangeSet a
singleton x = Straight do StRangeSet [discreteRange x x]

union :: DiscreteOrd a => DiscreteRangeSet a -> DiscreteRangeSet a -> DiscreteRangeSet a
union (Straight rs1) (Straight rs2) = Straight do rs1 <> rs2
union (Complement rs1) (Straight rs2) = Complement do rs1 `differenceStRange` rs2
union (Straight rs1) (Complement rs2) = Complement do rs2 `differenceStRange` rs1
union (Complement rs1) (Complement rs2) = Complement do rs1 `intersectStRange` rs2

infixl 6 `union`

intersect :: DiscreteOrd a => DiscreteRangeSet a -> DiscreteRangeSet a -> DiscreteRangeSet a
intersect (Straight rs1) (Straight rs2) = Straight do rs1 `intersectStRange` rs2
intersect (Complement rs1) (Straight rs2) = Straight do rs2 `differenceStRange` rs1
intersect (Straight rs1) (Complement rs2) = Straight do rs1 `differenceStRange` rs2
intersect (Complement rs1) (Complement rs2) = Complement do rs1 <> rs2

infixl 7 `intersect`

insert :: DiscreteOrd a => DiscreteRange a -> DiscreteRangeSet a -> DiscreteRangeSet a
insert r = \case
    Straight rs -> Straight do insertStRange r rs
    Complement rs -> Complement do removeStRange r rs

elem :: DiscreteOrd a => a -> DiscreteRangeSet a -> Bool
elem x = \case
    Straight rs   -> elemStRange x rs
    Complement rs -> not do elemStRange x rs

instance DiscreteOrd a => Semigroup (DiscreteRangeSet a) where
    (<>) = union

instance DiscreteOrd a => Monoid (DiscreteRangeSet a) where
    mempty = empty


newtype StRangeSet a = StRangeSet [DiscreteRange a]
    deriving (Eq, Show)

stRangeSet :: DiscreteOrd a => [DiscreteRange a] -> StRangeSet a
stRangeSet rs = StRangeSet do sortDiscreteRanges rs

insertStRange :: DiscreteOrd a => DiscreteRange a -> StRangeSet a -> StRangeSet a
insertStRange x = \case
    StRangeSet rs -> StRangeSet do go x rs
    where
        go r0 = \case
            []   -> [r0]
            rrs@(r:rs) -> case DiscreteRange.compareAndMerge r r0 of
                DiscreteRange.LessThanCannotMerge ->
                    r:go r0 rs
                DiscreteRange.GreaterThanCannotMerge ->
                    r0:rrs
                DiscreteRange.Merged mr ->
                    go mr rs

removeStRange :: DiscreteOrd a => DiscreteRange a -> StRangeSet a -> StRangeSet a
removeStRange x = \case
    StRangeSet rs -> StRangeSet do go x rs
    where
        go r0 = \case
            []   -> []
            rrs@(r:rs) -> case DiscreteRange.compareAndFindCommon r r0 of
                DiscreteRange.LessThanWithoutCommon ->
                    r:go r0 rs
                DiscreteRange.GreaterThanWithoutCommon ->
                    rrs
                DiscreteRange.WithCommon cr ->
                    let (crl, cru) = DiscreteRange.bounds cr
                        (l0, u0) = DiscreteRange.bounds r0
                        lr = if
                                | l0 < crl  -> (discreteRange l0 (pred crl):)
                                | otherwise -> id
                    in lr if
                        | cru < u0  -> discreteRange (succ cru) u0:go r0 rs
                        | otherwise -> go r0 rs

elemStRange :: DiscreteOrd a => a -> StRangeSet a -> Bool
elemStRange x = \case
    StRangeSet rs -> go rs
    where
        go = \case
            []   -> False
            r:rs -> DiscreteRange.elem x r || go rs

instance DiscreteOrd a => Semigroup (StRangeSet a) where
    StRangeSet s1 <> StRangeSet s2 = StRangeSet do go s1 s2 where
        go [] rs2 = rs2
        go rs1 [] = rs1
        go rs1@(r1:rs1') rs2@(r2:rs2') = case DiscreteRange.compareAndMerge r1 r2 of
            DiscreteRange.LessThanCannotMerge ->
                r1:go rs1' rs2
            DiscreteRange.GreaterThanCannotMerge ->
                r2:go rs1 rs2'
            DiscreteRange.Merged mr ->
                go rs1' do mr:rs2'

instance DiscreteOrd a => Monoid (StRangeSet a) where
    mempty = StRangeSet []

instance DiscreteOrd a => IsList (StRangeSet a) where
    type Item (StRangeSet a) = a
    fromList xs = StRangeSet do reverse do go xs where
        go :: [a] -> [DiscreteRange a]
        go = foldlSorted' [] \z x ->
            let xr = discreteRange x x in
                case z of
                    []   -> [xr]
                    r:rs -> case DiscreteRange.compareAndMerge xr r of
                        DiscreteRange.LessThanCannotMerge ->
                            error "unreachable"
                        DiscreteRange.GreaterThanCannotMerge ->
                            xr:r:rs
                        DiscreteRange.Merged mr ->
                            mr:rs
    toList = \case
        StRangeSet rs -> foldMap (\r -> let (l, u) = DiscreteRange.bounds r in [l..u]) rs

intersectStRange :: DiscreteOrd a => StRangeSet a -> StRangeSet a -> StRangeSet a
intersectStRange (StRangeSet s1) (StRangeSet s2) = StRangeSet do go s1 s2 where
    go [] _ = []
    go _ [] = []
    go rs1@(r1:rs1') rs2@(r2:rs2') = case DiscreteRange.compareAndFindCommon r1 r2 of
        DiscreteRange.LessThanWithoutCommon ->
            go rs1' rs2
        DiscreteRange.GreaterThanWithoutCommon ->
            go rs1 rs2'
        DiscreteRange.WithCommon cr ->
            let (_, u1) = DiscreteRange.bounds r1
                (_, u2) = DiscreteRange.bounds r2
            in if
                | u1 < u2   -> cr:go rs1' rs2
                | otherwise -> cr:go rs1 rs2'

differenceStRange :: DiscreteOrd a => StRangeSet a -> StRangeSet a -> StRangeSet a
differenceStRange (StRangeSet s1) (StRangeSet s2) = StRangeSet $ go s1 s2 where
    go [] _ = []
    go rs1 [] = rs1
    go rs1@(r1:rs1') rs2@(r2:rs2') = case DiscreteRange.compareAndFindCommon r1 r2 of
        DiscreteRange.LessThanWithoutCommon ->
            r1:go rs1' rs2
        DiscreteRange.GreaterThanWithoutCommon ->
            go rs1 rs2'
        DiscreteRange.WithCommon cr ->
            let (crl, cru) = DiscreteRange.bounds cr
                (l1, u1) = DiscreteRange.bounds r1
                lr = if
                        | l1 < crl  -> (discreteRange l1 (pred crl):)
                        | otherwise -> id
            in lr if
                | cru < u1  -> discreteRange (succ cru) u1:go rs1 rs2'
                | otherwise -> go rs1' rs2


data SortedBinTree a
    = Empty
    | Leaf a
    | NodeLeft (SortedBinTree a) a
    | NodeRight a (SortedBinTree a)
    | Node (SortedBinTree a) a (SortedBinTree a)

sortDiscreteRanges :: DiscreteOrd a => [DiscreteRange a] -> [DiscreteRange a]
sortDiscreteRanges = \case
        []   -> []
        r0:rs -> sortedTreeToList [] do foldl' (\t r -> insertRange r t) (Leaf r0) rs
    where
        sortedTreeToList l = \case
            Empty ->
                l
            Leaf r ->
                r:l
            NodeLeft tl r ->
                sortedTreeToList (r:l) tl
            NodeRight r tr ->
                r:sortedTreeToList l tr
            Node tl r tr ->
                sortedTreeToList (r:sortedTreeToList l tr) tl

        insertRange r0 = \case
            Empty       -> Leaf r0
            t@(Leaf r) -> case DiscreteRange.compareAndMerge r0 r of
                DiscreteRange.LessThanCannotMerge ->
                    NodeRight r0 t
                DiscreteRange.GreaterThanCannotMerge ->
                    NodeLeft t r0
                DiscreteRange.Merged mr ->
                    Leaf mr
            NodeLeft tl r -> case DiscreteRange.compareAndMerge r0 r of
                DiscreteRange.LessThanCannotMerge ->
                    NodeLeft (insertRange r0 tl) r
                DiscreteRange.GreaterThanCannotMerge ->
                    Node tl r (Leaf r0)
                DiscreteRange.Merged mr ->
                    insertRange mr tl
            NodeRight r tr -> case DiscreteRange.compareAndMerge r0 r of
                DiscreteRange.LessThanCannotMerge ->
                    Node (Leaf r0) r tr
                DiscreteRange.GreaterThanCannotMerge ->
                    NodeRight r (insertRange r0 tr)
                DiscreteRange.Merged mr ->
                    insertRange mr tr
            Node tl r tr -> case DiscreteRange.compareAndMerge r0 r of
                DiscreteRange.LessThanCannotMerge ->
                    Node (insertRange r0 tl) r tr
                DiscreteRange.GreaterThanCannotMerge ->
                    Node tl r (insertRange r0 tr)
                DiscreteRange.Merged mr ->
                    let (tl', mr1) = normalizeSortedTreeLeft mr tl
                        (mr2, tr') = normalizeSortedTreeRight mr1 tr
                    in Node tl' mr2 tr'

        normalizeSortedTreeLeft r0 = \case
            t@Empty ->
                (t, r0)
            t@(Leaf r) -> case DiscreteRange.compareAndMerge r r0 of
                DiscreteRange.LessThanCannotMerge ->
                    (t, r0)
                DiscreteRange.GreaterThanCannotMerge ->
                    error "unreachable"
                DiscreteRange.Merged mr ->
                    (Empty, mr)
            t@(NodeLeft tl r) -> case DiscreteRange.compareAndMerge r r0 of
                DiscreteRange.LessThanCannotMerge ->
                    (t, r0)
                DiscreteRange.GreaterThanCannotMerge ->
                    error "unreachable"
                DiscreteRange.Merged mr ->
                    normalizeSortedTreeLeft mr tl
            NodeRight r tr ->
                let (tr', r1) = normalizeSortedTreeLeft r0 tr
                in case tr' of
                    Empty -> case DiscreteRange.compareAndMerge r r1 of
                        DiscreteRange.LessThanCannotMerge ->
                            (Leaf r, r1)
                        DiscreteRange.GreaterThanCannotMerge ->
                            error "unreachable"
                        DiscreteRange.Merged mr ->
                            (Empty, mr)
                    _     -> (NodeRight r tr', r1)
            Node tl r tr ->
                let (tr', r1) = normalizeSortedTreeLeft r0 tr
                in case tr' of
                    Empty -> case DiscreteRange.compareAndMerge r r1 of
                        DiscreteRange.LessThanCannotMerge ->
                            (NodeLeft tl r, r1)
                        DiscreteRange.GreaterThanCannotMerge ->
                            error "unreachable"
                        DiscreteRange.Merged mr ->
                            normalizeSortedTreeLeft mr tl
                    _     -> (Node tl r tr', r1)

        normalizeSortedTreeRight r0 = \case
            t@Empty ->
                (r0, t)
            t@(Leaf r) -> case DiscreteRange.compareAndMerge r0 r of
                DiscreteRange.LessThanCannotMerge ->
                    (r0, t)
                DiscreteRange.GreaterThanCannotMerge ->
                    error "unreachable"
                DiscreteRange.Merged mr ->
                    (mr, Empty)
            t@(NodeRight r tr) -> case DiscreteRange.compareAndMerge r0 r of
                DiscreteRange.LessThanCannotMerge ->
                    (r0, t)
                DiscreteRange.GreaterThanCannotMerge ->
                    error "unreachable"
                DiscreteRange.Merged mr ->
                    normalizeSortedTreeRight mr tr
            NodeLeft tl r ->
                let (r1, tl') = normalizeSortedTreeRight r0 tl
                in case tl' of
                    Empty -> case DiscreteRange.compareAndMerge r1 r of
                        DiscreteRange.LessThanCannotMerge ->
                            (r1, Leaf r)
                        DiscreteRange.GreaterThanCannotMerge ->
                            error "unreachable"
                        DiscreteRange.Merged mr ->
                            (mr, Empty)
                    _     -> (r1, NodeLeft tl' r)
            Node tl r tr ->
                let (r1, tl') = normalizeSortedTreeRight r0 tl
                in case tl' of
                    Empty -> case DiscreteRange.compareAndMerge r1 r of
                        DiscreteRange.LessThanCannotMerge ->
                            (r1, NodeRight r tr)
                        DiscreteRange.GreaterThanCannotMerge ->
                            error "unreachable"
                        DiscreteRange.Merged mr ->
                            normalizeSortedTreeRight mr tr
                    _     -> (r1, Node tl' r tr)

insertSortedBinTree :: Ord a => SortedBinTree a -> a -> SortedBinTree a
insertSortedBinTree t0 x0 = go t0
    where
        go = \case
            Empty           -> Leaf x0
            t@(Leaf x)
                | x0 < x    -> NodeRight x0 t
                | otherwise -> NodeLeft t x0
            NodeLeft tl x
                | x0 < x    -> NodeLeft (go tl) x
                | otherwise -> Node tl x (Leaf x0)
            NodeRight x tr
                | x0 < x    -> Node (Leaf x0) x tr
                | otherwise -> NodeRight x (go tr)
            Node tl x tr
                | x0 < x    -> Node (go tl) x tr
                | otherwise -> Node tl x (go tr)

foldlSorted' :: (Ord a, Foldable f) => b -> (b -> a -> b) -> f a -> b
foldlSorted' z0 f t = go z0 do foldl' insertSortedBinTree Empty t
    where
        go z = \case
            Empty          -> z
            Leaf x         -> f z x
            NodeLeft tl x  -> f (go z tl) x
            NodeRight x tr -> go (f z x) tr
            Node tl x tr   -> go (f (go z tl) x) tr
