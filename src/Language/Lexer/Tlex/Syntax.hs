module Language.Lexer.Tlex.Syntax
    (
        StateNum,
        Pattern (..),
        Accept (..),
    ) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.CharSet as Tlex


type StateNum = Int

newtype Accept a = Accept a
    deriving (Eq, Show)


-- |
--
-- TODO:
-- * support byte range.
-- * support unicode category.
--
data Pattern
    = Empty
    | AnyOne
    | Pattern :^: Pattern
    | Pattern :|: Pattern
    | Maybe Pattern
    | Some Pattern
    | Many Pattern
    | Range Tlex.CharSet
    deriving (Eq, Show)

instance Semigroup Pattern where
    (<>) = (:^:)

instance Monoid Pattern where
    mempty = Empty

