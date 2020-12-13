{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lexer
import           Test.Hspec

import qualified Data.ByteString as ByteString
import qualified Data.Either     as Either


main :: IO ()
main = hspec $ do
    describe "lexByteString" $ do
        it "be valid programs" $ do
            lexByteString "\\x -> x" `shouldBe` Right
                ["\\","x"," ","->"," ","x"]
            lexByteString "(\\x -> x) 1" `shouldBe` Right
                ["(","\\","x"," ","->"," ","x",")"," ","1"]

        it "be invalid error programs" $ do
            Either.isLeft (lexByteString "'x") `shouldBe` True
            Either.isLeft (lexByteString "\"x") `shouldBe` True

        it "be valid sample-input.txt" $ do
            program <- ByteString.readFile "sample-input.txt"
            lexByteString program `shouldBe` Right
                [ "("
                , "\\","x1"," ","x2"," ","->"," "
                , "\\","z"," ","->"," ","x1"," ","+"," ","z"," ","*"," ","x2"
                , ")","\n    "
                , "(","0"," ","+"," ","1",")","\n"
                ]
