module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests de Ejemplo" $ do
    it "La leyProfesionalizacionTenisMesa es compatible con la leyTenis" $ do
      esCompatible leyProfesionalizacionTenisMesa leyTenis `shouldBe` True
    it "El juezSectorFinanciero vota por la leyMedicinalCannabis" $ do
      juezSectorFinanciero leyMedicinalCannabis `shouldBe` True
    it "Se cumple la condición mediante la función borocotizar" $ do
      seCumpleCondicion leyEducacionSuperior corteSuprema `shouldBe` True
    it "Se cumple que es constitucional con 3 nuevos Jueces" $ do
      constitucionalesConNuevosJueces [juezAfirmativo,juezInventadoVago,juezPresupuestario] leyesEjemplo corteSuprema `shouldBe` [UnaLey {tema = "Educacion Superior", presupuesto = 30, partidosImpulsores = ["Docentes Universitarios","Partido de Centro Federal"]}]
    