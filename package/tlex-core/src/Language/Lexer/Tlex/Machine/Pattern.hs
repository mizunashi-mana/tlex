module Language.Lexer.Tlex.Machine.Pattern (
    Pattern (..),
    enumsP,
    straightEnumSetP,
    anyoneP,
    AcceptPriority (..),
    mostPriority,
    Accept (..),
    compareAcceptsByPriority,
    StartState (..),
    startStateFromEnum,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.Hashable                       as Hashable
import qualified Language.Lexer.Tlex.Data.EnumSet    as EnumSet
import qualified Language.Lexer.Tlex.Data.SymEnumSet as SymEnumSet


newtype StartState = StartState Int
    deriving (Eq, Show)
    deriving Enum via Int

startStateFromEnum :: Enum s => s -> StartState
startStateFromEnum x = StartState do fromEnum x


newtype AcceptPriority = AcceptPriority Int
    deriving (Eq, Show)
    deriving Ord via Down Int
    deriving (Hashable.Hashable, Enum) via Int

mostPriority :: AcceptPriority
mostPriority = AcceptPriority 0

data Accept a = Accept
    { accPriority       :: AcceptPriority
    , accSemanticAction :: a
    }
    deriving (Eq, Show, Functor)

compareAcceptsByPriority :: Accept a -> Accept a -> Ordering
compareAcceptsByPriority Accept{ accPriority = p1 } Accept{ accPriority = p2 } = p1 `compare` p2

data Pattern e
    = Empty
    | Pattern e :^: Pattern e
    | Pattern e :|: Pattern e
    | Many (Pattern e)
    | Range (SymEnumSet.SymEnumSet e)
    deriving (Eq, Show)

instance Enum e => Semigroup (Pattern e) where
    (<>) = (:^:)

instance Enum e => Monoid (Pattern e) where
    mempty = Empty

enumsP :: Enum e => [e] -> Pattern e
enumsP l = straightEnumSetP do EnumSet.fromList l

straightEnumSetP :: Enum e => EnumSet.EnumSet e -> Pattern e
straightEnumSetP s = Range do SymEnumSet.fromEnumSet True s

anyoneP :: Enum e => Pattern e
anyoneP = Range SymEnumSet.full
