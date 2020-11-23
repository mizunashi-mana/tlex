{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH
import qualified Lexer                         as Lexer


$(Lexer.buildLexer)

main :: IO ()
main = do
    s <- readFile "input/example-uni.hs"
    case lexString s of
        Right xs -> print xs
        Left msg -> do
            putStrLn "error: "
            putStrLn msg

lexString :: String -> Either String [String]
lexString input = go input id where
    go s acc = case TlexTH.runInputString (tlexScan Lexer.Initial) s of
        (TlexTH.TlexEndOfInput, _)      -> Right $ acc []
        (TlexTH.TlexError, ctx)         -> Left $ show (ctx, acc [])
        (TlexTH.TlexAccepted ctx (), _) ->
            let rest = TlexTH.inputStringCtxRest ctx
                consumed = TlexTH.inputStringCtxPos ctx
            in go rest $ \n -> acc $ take consumed s : n
