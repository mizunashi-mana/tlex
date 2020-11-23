module Language.Lexer.Tlex.Data.NonEmptyEnumStringSet (
    NonEmptyEnumStringSet (..),
    empty,
    insert,
    insertSingleByte,
    singleton,
    union,
    intersection,
    fromList,
) where

import           Prelude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Language.Lexer.Tlex.Data.EnumSet as EnumSet
import qualified Language.Lexer.Tlex.Data.EnumMap as EnumMap


data NonEmptyEnumStringSet a = NonEmptyEnumStringSet
    { singleEnums :: EnumSet.EnumSet a
    , enumStrings :: EnumMap.EnumMap a (NonEmptyEnumStringSet a)
    }
    deriving (Eq, Show)

empty :: Enum a => NonEmptyEnumStringSet a
empty = NonEmptyEnumStringSet
    { singleEnums = EnumSet.empty
    , enumStrings = EnumMap.empty
    }

singleton :: Enum a => NonEmpty.NonEmpty a -> NonEmptyEnumStringSet a
singleton (x NonEmpty.:| xs) = case xs of
    [] -> NonEmptyEnumStringSet
        { singleEnums = EnumSet.singleton x
        , enumStrings = EnumMap.empty
        }
    y:ys -> NonEmptyEnumStringSet
        { singleEnums = EnumSet.empty
        , enumStrings = EnumMap.singleton x do
            singleton do y NonEmpty.:| ys
        }

insert :: Enum a
    => NonEmpty.NonEmpty a -> NonEmptyEnumStringSet a -> NonEmptyEnumStringSet a
insert (x NonEmpty.:| xs) s = case xs of
    [] -> insertSingleByte x s
    y:ys -> let xs' = y NonEmpty.:| ys in s
        { enumStrings = EnumMap.insertOrUpdate x
            do singleton xs'
            do \xss -> insert xs' xss
            do enumStrings s
        }

insertSingleByte :: Enum a => a -> NonEmptyEnumStringSet a -> NonEmptyEnumStringSet a
insertSingleByte x s = s
    { singleEnums = EnumSet.insert x
        do singleEnums s
    }

union :: Enum a
    => NonEmptyEnumStringSet a -> NonEmptyEnumStringSet a -> NonEmptyEnumStringSet a
union s1 s2 = NonEmptyEnumStringSet
    { singleEnums = singleEnums s1 `EnumSet.union` singleEnums s2
    , enumStrings = EnumMap.unionWith
        do \xs1 xs2 -> xs1 `union` xs2
        do enumStrings s1
        do enumStrings s2
    }

intersection :: Enum a
    => NonEmptyEnumStringSet a -> NonEmptyEnumStringSet a -> NonEmptyEnumStringSet a
intersection s1 s2 = NonEmptyEnumStringSet
    { singleEnums = singleEnums s1 `EnumSet.intersection` singleEnums s2
    , enumStrings = EnumMap.intersectionWith
        do \xs1 xs2 -> xs1 `union` xs2
        do enumStrings s1
        do enumStrings s2
    }

fromList :: Enum a => [NonEmpty.NonEmpty a] -> NonEmptyEnumStringSet a
fromList xs = foldr insert empty xs
