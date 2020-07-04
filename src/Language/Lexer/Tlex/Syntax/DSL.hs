module Language.Lexer.Tlex.Syntax.DSL (
    LexerRuleBuilder,
    lexRule,
    buildScanner,
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Syntax as Tlex


buildScanner :: LexerRuleBuilder s f () -> Tlex.Scanner f
buildScanner (LexerRuleBuilder builder) = Tlex.Scanner
    { scannerName = mempty
    , scannerRules = execState builder []
    }

newtype LexerRuleBuilder s f a = LexerRuleBuilder (State [Tlex.ScanRule f] a)
    deriving (Functor, Applicative, Monad) via State [Tlex.ScanRule f]

lexRule :: Enum s => [s] -> Tlex.Pattern -> f -> LexerRuleBuilder s f ()
lexRule ss p act = LexerRuleBuilder
    do modify' \rs0 ->
        Tlex.ScanRule [Tlex.startStateFromEnum s | s <- ss] p act:rs0
