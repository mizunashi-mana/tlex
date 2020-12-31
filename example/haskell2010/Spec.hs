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
            program <- ByteString.readFile "input/example.hs"
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

        it "be valid example-comment.hs" $ do
            program <- ByteString.readFile "input/example-comment.hs"
            lexByteString program `shouldBe` Right
                [
                    SpannedToken {token = TokOpenComment, rawByteString = "{-", tokenSpan = (0,2)},
                    SpannedToken {token = TokEnclosedCommentChar " ", rawByteString = " ", tokenSpan = (2,1)},
                    SpannedToken {token = TokEnclosedCommentChar "a", rawByteString = "a", tokenSpan = (3,1)},
                    SpannedToken {token = TokEnclosedCommentChar " ", rawByteString = " ", tokenSpan = (4,1)},
                    SpannedToken {token = TokEnclosedCommentChar "m", rawByteString = "m", tokenSpan = (5,1)},
                    SpannedToken {token = TokEnclosedCommentChar "u", rawByteString = "u", tokenSpan = (6,1)},
                    SpannedToken {token = TokEnclosedCommentChar "l", rawByteString = "l", tokenSpan = (7,1)},
                    SpannedToken {token = TokEnclosedCommentChar "t", rawByteString = "t", tokenSpan = (8,1)},
                    SpannedToken {token = TokEnclosedCommentChar "i", rawByteString = "i", tokenSpan = (9,1)},
                    SpannedToken {token = TokEnclosedCommentChar "l", rawByteString = "l", tokenSpan = (10,1)},
                    SpannedToken {token = TokEnclosedCommentChar "i", rawByteString = "i", tokenSpan = (11,1)},
                    SpannedToken {token = TokEnclosedCommentChar "n", rawByteString = "n", tokenSpan = (12,1)},
                    SpannedToken {token = TokEnclosedCommentChar "e", rawByteString = "e", tokenSpan = (13,1)},
                    SpannedToken {token = TokEnclosedCommentChar " ", rawByteString = " ", tokenSpan = (14,1)},
                    SpannedToken {token = TokEnclosedCommentChar "c", rawByteString = "c", tokenSpan = (15,1)},
                    SpannedToken {token = TokEnclosedCommentChar "o", rawByteString = "o", tokenSpan = (16,1)},
                    SpannedToken {token = TokEnclosedCommentChar "m", rawByteString = "m", tokenSpan = (17,1)},
                    SpannedToken {token = TokEnclosedCommentChar "m", rawByteString = "m", tokenSpan = (18,1)},
                    SpannedToken {token = TokEnclosedCommentChar "e", rawByteString = "e", tokenSpan = (19,1)},
                    SpannedToken {token = TokEnclosedCommentChar "n", rawByteString = "n", tokenSpan = (20,1)},
                    SpannedToken {token = TokEnclosedCommentChar "t", rawByteString = "t", tokenSpan = (21,1)},
                    SpannedToken {token = TokEnclosedCommentChar "\n", rawByteString = "\n", tokenSpan = (22,1)},
                    SpannedToken {token = TokOpenComment, rawByteString = "{-", tokenSpan = (23,2)},
                    SpannedToken {token = TokEnclosedCommentChar "\n", rawByteString = "\n", tokenSpan = (25,1)},
                    SpannedToken {token = TokEnclosedCommentChar "a", rawByteString = "a", tokenSpan = (26,1)},
                    SpannedToken {token = TokEnclosedCommentChar " ", rawByteString = " ", tokenSpan = (27,1)},
                    SpannedToken {token = TokEnclosedCommentChar "n", rawByteString = "n", tokenSpan = (28,1)},
                    SpannedToken {token = TokEnclosedCommentChar "e", rawByteString = "e", tokenSpan = (29,1)},
                    SpannedToken {token = TokEnclosedCommentChar "s", rawByteString = "s", tokenSpan = (30,1)},
                    SpannedToken {token = TokEnclosedCommentChar "t", rawByteString = "t", tokenSpan = (31,1)},
                    SpannedToken {token = TokEnclosedCommentChar "e", rawByteString = "e", tokenSpan = (32,1)},
                    SpannedToken {token = TokEnclosedCommentChar "d", rawByteString = "d", tokenSpan = (33,1)},
                    SpannedToken {token = TokEnclosedCommentChar " ", rawByteString = " ", tokenSpan = (34,1)},
                    SpannedToken {token = TokEnclosedCommentChar "c", rawByteString = "c", tokenSpan = (35,1)},
                    SpannedToken {token = TokEnclosedCommentChar "o", rawByteString = "o", tokenSpan = (36,1)},
                    SpannedToken {token = TokEnclosedCommentChar "m", rawByteString = "m", tokenSpan = (37,1)},
                    SpannedToken {token = TokEnclosedCommentChar "m", rawByteString = "m", tokenSpan = (38,1)},
                    SpannedToken {token = TokEnclosedCommentChar "e", rawByteString = "e", tokenSpan = (39,1)},
                    SpannedToken {token = TokEnclosedCommentChar "n", rawByteString = "n", tokenSpan = (40,1)},
                    SpannedToken {token = TokEnclosedCommentChar "t", rawByteString = "t", tokenSpan = (41,1)},
                    SpannedToken {token = TokEnclosedCommentChar "\n", rawByteString = "\n", tokenSpan = (42,1)},
                    SpannedToken {token = TokCloseComment, rawByteString = "-}", tokenSpan = (43,2)},
                    SpannedToken {token = TokEnclosedCommentChar "\n", rawByteString = "\n", tokenSpan = (45,1)},
                    SpannedToken {token = TokCloseComment, rawByteString = "-}", tokenSpan = (46,2)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = "\n\n", tokenSpan = (48,2)},
                    SpannedToken {token = TokQualifiedVarId "main", rawByteString = "main", tokenSpan = (50,4)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (54,1)},
                    SpannedToken {token = TokReservedOp "::", rawByteString = "::", tokenSpan = (55,2)},SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (57,1)},
                    SpannedToken {token = TokQualifiedConId "IO", rawByteString = "IO", tokenSpan = (58,2)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (60,1)},
                    SpannedToken {token = TokSpecial "(", rawByteString = "(", tokenSpan = (61,1)},
                    SpannedToken {token = TokSpecial ")", rawByteString = ")", tokenSpan = (62,1)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = "\n", tokenSpan = (63,1)},
                    SpannedToken {token = TokQualifiedVarId "main", rawByteString = "main", tokenSpan = (64,4)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (68,1)},
                    SpannedToken {token = TokReservedOp "=", rawByteString = "=", tokenSpan = (69,1)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (70,1)},
                    SpannedToken {token = TokQualifiedVarId "print", rawByteString = "print", tokenSpan = (71,5)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (76,1)},
                    SpannedToken {token = TokSpecial "(", rawByteString = "(", tokenSpan = (77,1)},
                    SpannedToken {token = TokSpecial ")", rawByteString = ")", tokenSpan = (78,1)},
                    SpannedToken {token = TokWhiteSpace, rawByteString = " ", tokenSpan = (79,1)},
                    SpannedToken {token = TokLineComment "-- line comment\n", rawByteString = "-- line comment\n", tokenSpan = (80,16)}
                ]
