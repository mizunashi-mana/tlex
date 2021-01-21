import           Lexer
import           Test.Hspec

import qualified Data.Either     as Either
import           Lexer.TextId    (TextId (..))
import           Lexer.Rules     (LexerAction (..), IdToken (..))
import           Lexer.Token     (Token (..))
import           Lexer.CodeUnit  (CodeUnit (..))


main :: IO ()
main = hspec $ do
    describe "lexString" $ do
        it "be valid programs" $ do
            lexString "0b0" `shouldBe` Right
                [
                    SpannedAction {lexerAction = LexLitBitInteger, rawCodeUnits = [LcU0030,LcU0062,LcU0030], tokenSpan = (0,3)}
                ]
            lexString "\\x -> 0x" `shouldBe` Right
                [
                    SpannedAction {lexerAction = WithToken SymLambda, rawCodeUnits = [LcU005C], tokenSpan = (0,1)},
                    SpannedAction {lexerAction = WithIdToken (IdToken $ \x -> IdVarId x), rawCodeUnits = [LcU0078], tokenSpan = (1,1)},
                    SpannedAction {lexerAction = WithWhitespace, rawCodeUnits = [LcOtherCatZs], tokenSpan = (2,1)},
                    SpannedAction {lexerAction = WithToken SymArrow, rawCodeUnits = [LcU002D,LcU003E], tokenSpan = (3,2)},
                    SpannedAction {lexerAction = WithWhitespace, rawCodeUnits = [LcOtherCatZs], tokenSpan = (5,1)},
                    SpannedAction {lexerAction = LexLitDecimalInteger, rawCodeUnits = [LcU0030], tokenSpan = (6,1)},
                    SpannedAction {lexerAction = WithIdToken (IdToken $ \x -> IdVarId x), rawCodeUnits = [LcU0078], tokenSpan = (7,1)}
                ]
            lexString "\\_ -> 0o666" `shouldBe` Right
                [
                    SpannedAction {lexerAction = WithToken SymLambda, rawCodeUnits = [LcU005C], tokenSpan = (0,1)},
                    SpannedAction {lexerAction = WithToken KwUnderscore, rawCodeUnits = [LcU005F], tokenSpan = (1,1)},
                    SpannedAction {lexerAction = WithWhitespace, rawCodeUnits = [LcOtherCatZs], tokenSpan = (2,1)},
                    SpannedAction {lexerAction = WithToken SymArrow, rawCodeUnits = [LcU002D,LcU003E], tokenSpan = (3,2)},
                    SpannedAction {lexerAction = WithWhitespace, rawCodeUnits = [LcOtherCatZs], tokenSpan = (5,1)},
                    SpannedAction {lexerAction = LexLitOctitInteger, rawCodeUnits = [LcU0030,LcU006F,LcU0036,LcU0036,LcU0036], tokenSpan = (6,5)}
                ]
