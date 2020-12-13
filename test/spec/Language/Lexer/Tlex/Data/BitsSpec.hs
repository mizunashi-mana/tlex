module Language.Lexer.Tlex.Data.BitsSpec (spec) where

import Test.Hspec

import Language.Lexer.Tlex.Prelude
import Language.Lexer.Tlex.Data.Bits


spec :: Spec
spec = do
    describe "maxBitSize" do
        it "returns valid bit size" do
            maxBitSize @Int 0b10001 `shouldBe` 5
            maxBitSize @Int 0b00000 `shouldBe` 1
            maxBitSize @Int 0b11111 `shouldBe` 5
            maxBitSize @Int 0b10000 `shouldBe` 5

        it "returns valid bit size for Word8 maxBound" do
            maxBitSize @Word8 maxBound `shouldBe` 8
