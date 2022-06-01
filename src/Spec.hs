module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El juezSectorFinanciero vota por la leyMedicinalCannabis" $ do
      juezSectorFinanciero leyMedicinalCannabis `shouldBe` True