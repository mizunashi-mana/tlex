{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH
import qualified Lexer                         as Lexer

$(Lexer.buildLexer)

main :: IO ()
main = do
    s <- readFile "sample-input.txt"
    print $ lexString s

lexString :: String -> Either String [String]
lexString input = go input id where
    go s acc = case TlexTH.runInputString (tlexScan ()) s of
        (TlexTH.TlexEndOfInput, _)      -> Right (acc [])
        (TlexTH.TlexError, ctx)         -> Left (take 10 (TlexTH.inputStringCtxRest ctx) ++ "...")
        (TlexTH.TlexAccepted ctx (), _) ->
            let rest = TlexTH.inputStringCtxRest ctx
                consumed = TlexTH.inputStringCtxPos ctx
            in go rest (\n -> acc (take consumed s:n))
