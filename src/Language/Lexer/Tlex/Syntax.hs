module Language.Lexer.Tlex.Syntax (
    StateNum,
    Pattern (..),
    anyoneP,
    maybeP,
    someP,
    manyP,
    orP,
    Accept (..),
    SemanticAction (..),
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.CharSet as CharSet


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

data SemanticAction s a = SemanticAction (Text -> a)
