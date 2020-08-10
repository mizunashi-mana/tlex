module Language.Lexer.Tlex.Data.SymEnumSet (
    SymEnumSet,
    empty,
    full,
    complement,
    singleton,
    union,
    intersection,
    difference,
    fromEnumSet,
    toEnumSet,
) where

import           Prelude

import qualified Language.Lexer.Tlex.Data.EnumSet as EnumSet


data SymEnumSet a = SymEnumSet
    { isStraight :: Bool
    , internalEnumSet :: EnumSet.EnumSet a
    }
    deriving (Eq, Show)

empty :: Enum a => SymEnumSet a
empty = SymEnumSet
    { isStraight = True
    , internalEnumSet = EnumSet.empty
    }

full :: Enum a => SymEnumSet a
full = SymEnumSet
    { isStraight = False
    , internalEnumSet = EnumSet.empty
    }

complement :: Enum a => SymEnumSet a -> SymEnumSet a
complement s = s
    { isStraight = not do isStraight s
    }

singleton :: Enum a => a -> SymEnumSet a
singleton x = SymEnumSet
    { isStraight = True
    , internalEnumSet = EnumSet.singleton x
    }

union :: Enum a => SymEnumSet a -> SymEnumSet a -> SymEnumSet a
union s1 s2 = case isStraight s1 of
    True -> case isStraight s2 of
        True -> SymEnumSet
            { isStraight = True
            , internalEnumSet = EnumSet.union
                do internalEnumSet s1
                do internalEnumSet s2
            }
        False -> SymEnumSet
            { isStraight = False
            , internalEnumSet = EnumSet.difference
                do internalEnumSet s2
                do internalEnumSet s1
            }
    False -> case isStraight s2 of
        True -> SymEnumSet
            { isStraight = False
            , internalEnumSet = EnumSet.difference
                do internalEnumSet s1
                do internalEnumSet s2
            }
        False -> SymEnumSet
            { isStraight = False
            , internalEnumSet = EnumSet.intersection
                do internalEnumSet s1
                do internalEnumSet s2
            }

intersection :: Enum a => SymEnumSet a -> SymEnumSet a -> SymEnumSet a
intersection s1 s2 = case isStraight s1 of
    True -> case isStraight s2 of
        True -> SymEnumSet
            { isStraight = True
            , internalEnumSet = EnumSet.intersection
                do internalEnumSet s1
                do internalEnumSet s2
            }
        False -> SymEnumSet
            { isStraight = True
            , internalEnumSet = EnumSet.difference
                do internalEnumSet s1
                do internalEnumSet s2
            }
    False -> case isStraight s2 of
        True -> SymEnumSet
            { isStraight = True
            , internalEnumSet = EnumSet.difference
                do internalEnumSet s2
                do internalEnumSet s1
            }
        False -> SymEnumSet
            { isStraight = False
            , internalEnumSet = EnumSet.union
                do internalEnumSet s1
                do internalEnumSet s2
            }

difference :: Enum a => SymEnumSet a -> SymEnumSet a -> SymEnumSet a
difference s1 s2 = case isStraight s1 of
    True -> case isStraight s2 of
        True -> SymEnumSet
            { isStraight = True
            , internalEnumSet = EnumSet.difference
                do internalEnumSet s1
                do internalEnumSet s2
            }
        False -> SymEnumSet
            { isStraight = True
            , internalEnumSet = EnumSet.intersection
                do internalEnumSet s1
                do internalEnumSet s2
            }
    False -> case isStraight s2 of
        True -> SymEnumSet
            { isStraight = False
            , internalEnumSet = EnumSet.union
                do internalEnumSet s2
                do internalEnumSet s1
            }
        False -> SymEnumSet
            { isStraight = True
            , internalEnumSet = EnumSet.difference
                do internalEnumSet s2
                do internalEnumSet s1
            }

fromEnumSet :: Enum a => Bool -> EnumSet.EnumSet a -> SymEnumSet a
fromEnumSet b s = SymEnumSet
    { isStraight = b
    , internalEnumSet = s
    }

toEnumSet :: Enum a => SymEnumSet a -> (Bool, EnumSet.EnumSet a)
toEnumSet s = (isStraight s, internalEnumSet s)
