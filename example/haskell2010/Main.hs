{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH
import qualified Lexer as Lexer

main :: IO ()
main = putStrLn "Hello, World"
{-
$(Lexer.buildLexer)

lexString :: String -> Either String [String]
lexString input = go input id where
    go s acc = case TlexTH.runInputString (tlexScan Lexer.Initial) s of
        (TlexTH.TlexEndOfInput, _)      -> Right (acc [])
        (TlexTH.TlexError, ctx)         -> Left (take 10 (TlexTH.inputStringCtxRest ctx) ++ "...")
        (TlexTH.TlexAccepted ctx (), _) ->
            let rest = TlexTH.inputStringCtxRest ctx
                consumed = TlexTH.inputStringCtxPos ctx
            in go rest (\n -> acc (take consumed s:n))
-}