module Language.Lexer.Tlex.Data.AddrSpec (spec) where

import           Language.Lexer.Tlex.Prelude
import           Test.Hspec
import           Test.QuickCheck

import           Language.Lexer.Tlex.Data.Addr


spec :: Spec
spec = do
    describe "addrCodeUnitsLE" do
        it "returns valid a Word8 unit" do
            property \x -> addrCodeUnitsLE 1 x `shouldBe` [x]

        it "returns valid Word8 units" do
            property \x -> addrCodeUnitsLE 4 x `shouldBe` [x, 0, 0, 0]

        it "returns valid code units" do
            addrCodeUnitsLE @Int 4 0 `shouldBe` [0, 0, 0, 0]
            addrCodeUnitsLE @Int 4 0x123456 `shouldBe` [0x56, 0x34, 0x12, 0]

        it "returns valid -1 units" do
            addrCodeUnitsLE @Int 4 -1 `shouldBe` [0xFF, 0xFF, 0xFF, 0xFF]
