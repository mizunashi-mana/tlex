import           Lexer
import           Test.Hspec

import qualified Data.Either     as Either
import           Lexer.Rules     (LexerAction (..))
import           Lexer.Token     (Token (..))


main :: IO ()
main = hspec $ do
    describe "lexString" $ do
        it "be valid programs" $ do
            lexString "0b0" `shouldBe` Right
                []
            lexString "\\x -> 0x" `shouldBe` Right
                []
            lexString "\\_ -> 0o666" `shouldBe` Right
                []

        it "be invalid error programs" $ do
            Either.isLeft (lexString "'x") `shouldBe` True
            Either.isLeft (lexString "\"x") `shouldBe` True

        it "be valid example.hs" $ do
            program <- readFile "input/example.hs"
            Either.isRight (lexString program) `shouldBe` True

        it "be valid example-uni.hs" $ do
            program <- readFile "input/example-uni.hs"
            lexString program `shouldBe` Right
                []

        it "be valid example-comment.hs" $ do
            program <- readFile "input/example-comment.hs"
            lexString program `shouldBe` Right
                []
