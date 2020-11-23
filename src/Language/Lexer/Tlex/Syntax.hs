module Language.Lexer.Tlex.Syntax (
    Scanner (..),
    ScanRule (..),
    ScannerBuilder,
    ScannerBuilderContext,
    buildScanner,
    lexRule,
    Pattern (..),
    enumsP,
    straightEnumSetP,
    anyoneP,
    maybeP,
    someP,
    manyP,
    orP,
    AcceptPriority,
    mostPriority,
    Accept (..),
    compareAcceptsByPriority,
    StartState,
    startStateFromEnum,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Data.Hashable                       as Hashable
import qualified Language.Lexer.Tlex.Data.SymEnumSet as SymEnumSet
import qualified Language.Lexer.Tlex.Data.EnumSet    as EnumSet


newtype Scanner e a = Scanner
    { scannerRules :: [ScanRule e a]
    }
    deriving (Eq, Show, Functor)

data ScanRule e a = ScanRule
    { scanRuleStartStates    :: [StartState]
    , scanRulePattern        :: Pattern e
    , scanRuleSemanticAction :: a
    }
    deriving (Eq, Show, Functor)


buildScanner :: Enum e => ScannerBuilder s e f () -> Scanner e f
buildScanner builder = Scanner
    { scannerRules = unScannerBuilderContext
        do execState builder do ScannerBuilderContext []
    }

newtype ScannerBuilderContext s e f = ScannerBuilderContext
    { unScannerBuilderContext :: [ScanRule e f]
    }
    deriving (Eq, Show, Functor)

type ScannerBuilder s e f = State (ScannerBuilderContext s e f)

lexRule :: Enum s => Enum e => [s] -> Pattern e -> f -> ScannerBuilder s e f ()
lexRule ss p act = modify' \(ScannerBuilderContext rs0) ->
    ScannerBuilderContext do ScanRule [startStateFromEnum s | s <- ss] p act:rs0


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

-- |
--
-- TODO:
-- * support byte range.
-- * support unicode category.
--
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

maybeP :: Enum e => Pattern e -> Pattern e
maybeP x = orP [Empty, x]

someP :: Enum e => Pattern e -> Pattern e
someP x = x <> Many x

manyP :: Enum e => Pattern e -> Pattern e
manyP x = Many x

{-# INLINE orP #-}
orP :: Enum e => [Pattern e] -> Pattern e
orP = \case
  []   -> Empty
  p:ps -> foldr (:|:) p ps
