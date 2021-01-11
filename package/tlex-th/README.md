# Tlex Plugin: Template Haskell

[![Hackage](https://img.shields.io/hackage/v/tlex-th.svg)](https://hackage.haskell.org/package/tlex-th)

See also https://hackage.haskell.org/package/tlex

## Usage

This plugin outputs from [`Scanner`](https://hackage.haskell.org/package/tlex-0.1.0.0/docs/Language-Lexer-Tlex-Syntax.html#t:Scanner) these APIs:

```haskell
-- aliases of arguments of @buildTHScanner@
type TlexStartState :: Type
type TlexSemanticAction :: Type
type TlexCodeUnit :: Type

-- lexing API
tlexScan :: TlexContext s TlexCodeUnit m => TlexStartState -> m (TlexResult s TlexSemanticAction)

-- runner fields
thTlexInitial :: Int -> Int
thTlexTrans :: Int -> Int -> Int
thTlexAccept :: Int -> Maybe TlexSemanticAction
```
