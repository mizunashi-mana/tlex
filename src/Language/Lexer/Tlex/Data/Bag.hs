module Language.Lexer.Tlex.Data.Bag (
    Bag,
    fromList,
    singleton,
) where

import           Language.Lexer.Tlex.Prelude


data Bag a
    = EmptyBag
    | UnitBag a
    | IncludeBags (Bag a) (Bag a)
    | ListBag [a]
    deriving (Show, Functor)

instance Foldable Bag where
    foldr k z = \case
        EmptyBag          -> z
        UnitBag x         -> k x z
        IncludeBags b1 b2 -> foldr k (foldr k z b2) b1
        ListBag xs        -> foldr k z xs

    foldMap f = \case
        EmptyBag          -> mempty
        UnitBag x         -> f x
        IncludeBags b1 b2 -> foldMap f b1 <> foldMap f b2
        ListBag xs        -> foldMap f xs

instance Eq a => Eq (Bag a) where
    b1 == b2 = toList b1 == toList b2

instance Semigroup (Bag a) where
    EmptyBag <> b2 = b2
    b1 <> EmptyBag = b1
    b1 <> b2       = IncludeBags b1 b2

instance Monoid (Bag a) where
    mempty = EmptyBag

fromList :: [a] -> Bag a
fromList xs = ListBag xs

singleton :: a -> Bag a
singleton x = UnitBag x
