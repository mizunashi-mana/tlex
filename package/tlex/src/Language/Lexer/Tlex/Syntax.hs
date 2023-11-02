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


newtype Scanner unit action = Scanner
    { scannerRules :: [ScanRule unit action]
    }
    deriving (Eq, Show, Functor)

data ScanRule unit action = ScanRule
    { scanRuleStartStates    :: [Pattern.StartState]
    , scanRulePattern        :: Pattern.Pattern unit
    , scanRuleSemanticAction :: action
    }
    deriving (Eq, Show, Functor)


buildScanner :: Enum unit
    => ScannerBuilder state unit action () -> Scanner unit action
buildScanner builder = Scanner
    { scannerRules = unScannerBuilderContext
        do execState builder do ScannerBuilderContext []
    }

newtype ScannerBuilderContext state unit action = ScannerBuilderContext
    { unScannerBuilderContext :: [ScanRule unit action]
    }
    deriving (Eq, Show, Functor)

type ScannerBuilder state unit action = State (ScannerBuilderContext state unit action)

lexRule :: Enum state => Enum unit
    => [state] -> Pattern.Pattern unit -> action -> ScannerBuilder state unit action ()
lexRule ss p act = modify' \(ScannerBuilderContext rs0) ->
    ScannerBuilderContext
        do ScanRule [Pattern.startStateFromEnum s | s <- ss] p act:rs0


-- | Wildcard pattern which accepts an any unit.
anyoneP :: Enum unit => Pattern.Pattern unit
anyoneP = Pattern.Range SymEnumSet.full

-- | Maybe pattern which accepts given pattern or nothing.
maybeP :: Enum unit => Pattern.Pattern unit -> Pattern.Pattern unit
maybeP x = orP [x, Pattern.Epsilon]

-- | Some pattern which accepts one given pattern or more times.
someP :: Enum unit => Pattern.Pattern unit -> Pattern.Pattern unit
someP x = x <> Pattern.Many x

-- | Many pattern which accepts nothing or given pattern more times.
manyP :: Enum unit => Pattern.Pattern unit -> Pattern.Pattern unit
manyP x = Pattern.Many x

-- | Or pattern which accepts one of given patterns.
orP :: Enum unit => [Pattern.Pattern unit] -> Pattern.Pattern unit
orP = \case
  []   -> Pattern.Epsilon
  p:ps -> foldr (Pattern.:|:) p ps
{-# INLINE orP #-}
