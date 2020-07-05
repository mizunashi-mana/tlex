{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Lexer as Lexer

main :: IO ()
main = putStrLn "Hello, World"

$(Lexer.buildLexer)
