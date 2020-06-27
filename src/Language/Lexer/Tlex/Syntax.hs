module Language.Lexer.Tlex.Syntax (
    Scanner (..),
    ScanRule (..),
    Pattern (..),
    anyoneP,
    maybeP,
    someP,
    manyP,
    orP,
    AcceptPriority,
    mostPriority,
    Accept (..),
    SemanticAction (..),
) where

import Language.Lexer.Tlex.Prelude

import qualified Language.Lexer.Tlex.Data.CharSet as CharSet


data Scanner s a = Scanner
    { scannerName :: Text
    , scannerRules :: [ScanRule s a]
    }

data ScanRule s a = ScanRule
    { scanRuleStartState :: [s]
    , scanRulePattern :: Pattern
    , scanRuleSemanticAction :: SemanticAction s a
    }


newtype AcceptPriority = AcceptPriority Int
    deriving (Eq, Show, Enum)

mostPriority :: AcceptPriority
mostPriority = AcceptPriority 0

data Accept s a = Accept
    { accPriority :: AcceptPriority
    , accSemanticAction :: SemanticAction s a
    }


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
