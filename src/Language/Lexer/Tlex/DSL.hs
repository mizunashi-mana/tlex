module Language.Lexer.Tlex.DSL (
    LexerDeclaration (..),
    LexerRuleBuilder,
    lexRule,
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Syntax as Tlex


data LexerDeclaration m a where
    LexerDeclaration :: forall s m a. Eq s =>
        { initialState :: s
        , rules :: LexerRuleBuilder s m a
        }
        -> LexerDeclaration m a


newtype LexerRuleBuilder s m a = LexerRuleBuilder
    { unLexerRuleBuilder :: LexerRuleContext s m -> (LexerRuleContext s m, a) }
    deriving Functor

data LexerRuleContext s m = LexerRuleContext

instance Applicative (LexerRuleBuilder s m) where
    pure x = LexerRuleBuilder \ctx -> (ctx, x)

    LexerRuleBuilder mf <*> LexerRuleBuilder mx = LexerRuleBuilder \ctx0 ->
        let (ctx1, f) = mf ctx0
            (ctx2, x) = mx ctx1
        in (ctx2, f x)

instance Monad (LexerRuleBuilder s m) where
    LexerRuleBuilder builder >>= k = LexerRuleBuilder \ctx0 ->
        let (ctx1, x) = builder ctx0
        in unLexerRuleBuilder (k x) ctx1

lexRule :: s -> Tlex.Pattern -> Tlex.SemanticAction s m -> LexerRuleBuilder s m ()
lexRule = undefined
