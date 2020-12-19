{-# LANGUAGE OverloadedStrings #-}

import           Lexer
import           Test.Hspec

import qualified Data.ByteString as ByteString
import qualified Data.Either     as Either
import           Lexer.Rules     (Token (..))


main :: IO ()
main = hspec $ do
    describe "lexByteString" $ do
        it "be valid programs" $ do
            lexByteString "\\x -> x" `shouldBe` Right
                [
                    SpannedToken {token = TokReservedOp "\\", rawByteString = "\\", tokenSpan = (0,1)},
                    SpannedToken {token = TokQualifiedVarId "x", rawByteString = "x", tokenSpan = (1,1)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (2,1)},
                    SpannedToken {token = TokReservedOp "->", rawByteString = "->", tokenSpan = (3,2)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (5,1)},
                    SpannedToken {token = TokQualifiedVarId "x", rawByteString = "x", tokenSpan = (6,1)}
                ]
            lexByteString "\\x -> 0x" `shouldBe` Right
                [
                    SpannedToken {token = TokReservedOp "\\", rawByteString = "\\", tokenSpan = (0,1)},
                    SpannedToken {token = TokQualifiedVarId "x", rawByteString = "x", tokenSpan = (1,1)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (2,1)},
                    SpannedToken {token = TokReservedOp "->", rawByteString = "->", tokenSpan = (3,2)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (5,1)},
                    SpannedToken {token = TokLitInteger "0", rawByteString = "0", tokenSpan = (6,1)},
                    SpannedToken {token = TokQualifiedVarId "x", rawByteString = "x", tokenSpan = (7,1)}
                ]
            lexByteString "\\_ -> 0o666" `shouldBe` Right
                [
                    SpannedToken {token = TokReservedOp "\\", rawByteString = "\\", tokenSpan = (0,1)},
                    SpannedToken {token = TokReservedId "_", rawByteString = "_", tokenSpan = (1,1)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (2,1)},
                    SpannedToken {token = TokReservedOp "->", rawByteString = "->", tokenSpan = (3,2)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (5,1)},
                    SpannedToken {token = TokLitInteger "0o666", rawByteString = "0o666", tokenSpan = (6,5)}
                ]

        it "be invalid error programs" $ do
            Either.isLeft (lexByteString "'x") `shouldBe` True
            Either.isLeft (lexByteString "\"x") `shouldBe` True

        it "be valid example.hs" $ do
            program <- ByteString.readFile "input/example-uni.hs"
            Either.isRight (lexByteString program) `shouldBe` True

        it "be valid example-uni.hs" $ do
            program <- ByteString.readFile "input/example-uni.hs"
            lexByteString program `shouldBe` Right
                [
                    SpannedToken {token = TokLineComment "-- Unicode comment \xE3\x81\x82\xE3\x81\x84\xE3\x81\x86\xE3\x81\x88\xE3\x81\x8A\n", rawByteString = "-- Unicode comment \xE3\x81\x82\xE3\x81\x84\xE3\x81\x86\xE3\x81\x88\xE3\x81\x8A\n", tokenSpan = (0,35)},
                    SpannedToken {token = TokQualifiedVarId "\xE5\xA4\x89\xE6\x95\xB0", rawByteString = "\xE5\xA4\x89\xE6\x95\xB0", tokenSpan = (35,6)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (41,1)},
                    SpannedToken {token = TokReservedOp "::", rawByteString = "::", tokenSpan = (42,2)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (44,1)},
                    SpannedToken {token = TokQualifiedConId "String", rawByteString = "String", tokenSpan = (45,6)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = "\n", tokenSpan = (51,1)},
                    SpannedToken {token = TokQualifiedVarId "\xE5\xA4\x89\xE6\x95\xB0", rawByteString = "\xE5\xA4\x89\xE6\x95\xB0", tokenSpan = (52,6)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (58,1)},
                    SpannedToken {token = TokReservedOp "=", rawByteString = "=", tokenSpan = (59,1)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (60,1)},
                    SpannedToken {token = TokLitString "\"\xE6\x96\x87\xE5\xAD\x97\xE5\x88\x97\"", rawByteString = "\"\xE6\x96\x87\xE5\xAD\x97\xE5\x88\x97\"", tokenSpan = (61,11)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = "\n", tokenSpan = (72,1)}
                ]
