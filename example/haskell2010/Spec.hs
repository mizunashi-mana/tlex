{-# LANGUAGE OverloadedStrings #-}

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
            lexByteString "\\x -> 0x" `shouldBe` Right
                ["\\","x"," ","->"," ","0","x"]
            lexByteString "\\_ -> 0o666" `shouldBe` Right
                ["\\","_"," ","->"," ","0o666"]

        it "be invalid error programs" $ do
            Either.isLeft (lexByteString "'x") `shouldBe` True
            Either.isLeft (lexByteString "\"x") `shouldBe` True

        it "be valid example.hs" $ do
            program <- ByteString.readFile "input/example-uni.hs"
            lexByteString program `shouldBe` Right
                [ "-- Unicode comment \227\129\130\227\129\132\227\129\134\227\129\136\227\129\138\n"
                , "\229\164\137\230\149\176"," ","::"," ","String","\n"
                , "\229\164\137\230\149\176"," ","="," ","\"\229\174\154\230\149\176\"","\n"
                ]
