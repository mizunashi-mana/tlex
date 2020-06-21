module Language.Lexer.Tlex.DSL (
    LexerDeclaration (..),
    LexerRuleBuilder,
    lexRule,
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Syntax as Tlex


data LexerDeclaration m where
    LexerDeclaration :: forall s m. Eq s =>
        { initialState :: s
        , rules :: [Tlex.ScanRule s m]
        }
        -> LexerDeclaration m


newtype LexerRuleBuilder s m a = LexerRuleBuilder
    { unLexerRuleBuilder
        :: [Tlex.ScanRule s m]
        -> ([Tlex.ScanRule s m], a)
    }
    deriving Functor

instance Applicative (LexerRuleBuilder s m) where
    pure x = LexerRuleBuilder \rs0 -> (rs0, x)

    LexerRuleBuilder mf <*> LexerRuleBuilder mx = LexerRuleBuilder \rs0 ->
        let (rs1, f) = mf rs0
            (rs2, x) = mx rs1
        in (rs2, f x)

instance Monad (LexerRuleBuilder s m) where
    LexerRuleBuilder builder >>= k = LexerRuleBuilder \rs0 ->
        let (rs1, x) = builder rs0
        in unLexerRuleBuilder (k x) rs1

lexRule :: [s] -> Tlex.Pattern -> Tlex.SemanticAction s m -> LexerRuleBuilder s m ()
lexRule ss p act = LexerRuleBuilder \rs0 -> (Tlex.ScanRule ss p act:rs0, ())
