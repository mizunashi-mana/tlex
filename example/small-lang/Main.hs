{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified System.Environment            as System
import qualified System.Exit                   as System
import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH
import qualified Lexer                         as Lexer
import qualified GHC.Word
import qualified Data.ByteString               as ByteString


$(Lexer.buildLexer)

main :: IO ()
main = do
    args <- System.getArgs
    f <- case args of
        [] -> do
            putStrLn "need input path"
            System.exitFailure
        x:_ -> pure x
    s <- ByteString.readFile f
    case lexByteString s of
        Right xs -> print xs
        Left msg -> do
            putStrLn "error: "
            putStrLn msg
            System.exitFailure

lexByteString :: ByteString.ByteString -> Either String [ByteString.ByteString]
lexByteString input = go (ByteString.unpack input) id where
    go s acc = case TlexTH.runInputString (tlexScan ()) s of
        (TlexTH.TlexEndOfInput, _)      -> Right $ acc []
        (TlexTH.TlexError, ctx)         -> Left $ show (ctx, acc [])
        (TlexTH.TlexAccepted ctx (), _) ->
            let rest = TlexTH.inputStringCtxRest ctx
                consumed = TlexTH.inputStringCtxPos ctx
            in go rest (\n -> acc (ByteString.pack (take consumed s):n))
