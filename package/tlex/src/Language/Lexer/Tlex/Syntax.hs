module Language.Lexer.Tlex.Syntax (
    Scanner (..),
    ScanRule (..),
    ScannerBuilder,
    ScannerBuilderContext,
    buildScanner,
    lexRule,
    Pattern.Pattern,
    Pattern.enumsP,
    Pattern.straightEnumSetP,
    anyoneP,
    maybeP,
    someP,
    manyP,
    orP,
    Pattern.StartState,
    Pattern.Accept (..),
    Pattern.AcceptPriority,
) where

import           Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.SymEnumSet as SymEnumSet
import qualified Language.Lexer.Tlex.Machine.Pattern as Pattern


newtype Scanner e a = Scanner
    { scannerRules :: [ScanRule e a]
    }
    deriving (Eq, Show, Functor)

data ScanRule e a = ScanRule
    { scanRuleStartStates    :: [Pattern.StartState]
    , scanRulePattern        :: Pattern.Pattern e
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

lexRule :: Enum s => Enum e
    => [s] -> Pattern.Pattern e -> f -> ScannerBuilder s e f ()
lexRule ss p act = modify' \(ScannerBuilderContext rs0) ->
    ScannerBuilderContext
        do ScanRule [Pattern.startStateFromEnum s | s <- ss] p act:rs0


anyoneP :: Enum e => Pattern.Pattern e
anyoneP = Pattern.Range SymEnumSet.full

maybeP :: Enum e => Pattern.Pattern e -> Pattern.Pattern e
maybeP x = orP [Pattern.Empty, x]

someP :: Enum e => Pattern.Pattern e -> Pattern.Pattern e
someP x = x <> Pattern.Many x

manyP :: Enum e => Pattern.Pattern e -> Pattern.Pattern e
manyP x = Pattern.Many x

{-# INLINE orP #-}
orP :: Enum e => [Pattern.Pattern e] -> Pattern.Pattern e
orP = \case
  []   -> Pattern.Empty
  p:ps -> foldr (Pattern.:|:) p ps
