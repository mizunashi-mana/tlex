module Language.Lexer.Tlex.Syntax (
    Scanner (..),
    ScanRule (..),
    ScannerBuilder,
    ScannerBuilderContext,
    buildScanner,
    lexRule,
    Pattern (..),
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

import qualified Data.Hashable                    as Hashable
import qualified Language.Lexer.Tlex.Data.CharSet as CharSet


newtype Scanner a = Scanner
    { scannerRules :: [ScanRule a]
    }
    deriving (Eq, Show, Functor)

data ScanRule a = ScanRule
    { scanRuleStartStates    :: [StartState]
    , scanRulePattern        :: Pattern
    , scanRuleSemanticAction :: a
    }
    deriving (Eq, Show, Functor)


buildScanner :: ScannerBuilder s f () -> Scanner f
buildScanner builder = Scanner
    { scannerRules = unScannerBuilderContext
        do execState builder do ScannerBuilderContext []
    }

newtype ScannerBuilderContext s f = ScannerBuilderContext
    { unScannerBuilderContext :: [ScanRule f]
    }
    deriving (Eq, Show, Functor)

type ScannerBuilder s f = State (ScannerBuilderContext s f)

lexRule :: Enum s => [s] -> Pattern -> f -> ScannerBuilder s f ()
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
data Pattern
    = Empty
    | Pattern :^: Pattern
    | Pattern :|: Pattern
    | Many Pattern
    | Range CharSet.CharSet
    deriving (Eq, Show)

instance Semigroup Pattern where
    (<>) = (:^:)

instance Monoid Pattern where
    mempty = Empty

anyoneP :: Pattern
anyoneP = Range CharSet.full

maybeP :: Pattern -> Pattern
maybeP x = orP [Empty, x]

someP :: Pattern -> Pattern
someP x = x <> Many x

manyP :: Pattern -> Pattern
manyP x = Many x

{-# INLINE orP #-}
orP :: [Pattern] -> Pattern
orP = \case
  []   -> Empty
  p:ps -> foldr (:|:) p ps
