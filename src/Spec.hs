module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)
hamburguesaConPollo = Hamburguesa 0 [Pollo, Curry, Cheddar]
hamburguesaSinBase = Hamburguesa 0 [Curry, Cheddar]


correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do

        describe "agrandar" $ do
            it "si en la hamburguesa hay carne, se agrega carne"  $ do
                agrandar cuartoDeLibra `shouldBe` cuartoDeLibra {ingredientes = ingredientes cuartoDeLibra ++ [Carne]}
            it "si en la hamburguesa hay pollo, se agrega pollo"  $ do
                agrandar hamburguesaConPollo `shouldBe` hamburguesaConPollo {ingredientes = ingredientes hamburguesaConPollo ++ [Pollo]}
            it "si en la hamburguesa hay pati vegano, se le agrega otro pati vegano "  $ do
                agrandar dobleCuartoVegano `shouldBe` dobleCuartoVegano {ingredientes = ingredientes  dobleCuartoVegano ++ [PatiVegano]}
            it "si en la hamburguesa no hay ni carne ni pollo ni pati vegano, se le agrega pollo"  $ do
                agrandar hamburguesaSinBase `shouldBe` hamburguesaSinBase {ingredientes = ingredientes  hamburguesaSinBase ++ [Pollo]}

        describe "agregarIngrediente" $ do
            it "Si le agrego Curry al cuarto de libra, se agrega Curry a la lista de ingredientes" $ do
                agregarIngrediente Curry cuartoDeLibra `shouldBe` cuartoDeLibra {ingredientes = ingredientes cuartoDeLibra ++ [Curry]}

        describe "descuento" $ do
            it "Si le aplico un descuento del 10% al cuarto de libra, su precio base queda en 18" $ do
                descuento 10 cuartoDeLibra `shouldBe` cuartoDeLibra {precioBase = 18}
            it "Si le aplico un descuento del 100% al cuarto de libra, su precio base queda en 0" $ do
                descuento 100 cuartoDeLibra `shouldBe` cuartoDeLibra {precioBase = 0}
            it "Si le aplico un descuento del 0% al cuarto de libra, su precio base se mantiene 20" $ do
                descuento 0 cuartoDeLibra `shouldBe` cuartoDeLibra {precioBase = 20}

        describe "pdepburger" $ do
            it "La pdepBurger debe poseer un valor final de 110" $ do
                precioFinal pdepBurger `shouldBe` 110

        describe "dobleCuarto" $ do
            it "El dobleCuarto debe poseer un valor final de 84" $ do
                precioFinal dobleCuarto `shouldBe` 84

        describe "bigPdep" $ do
            it "La bigPdep debe poseer un valor final de 89" $ do
                precioFinal bigPdep `shouldBe` 89

        describe "delDia" $ do
            it "La bigPdep del dia debe poseer un valor final de 88" $ do
                precioFinal (delDia dobleCuarto) `shouldBe` 88

        describe "hacerVeggie" $ do
            it "Si hago veggie la pdepBurger, su valor final es de 81" $ do
                precioFinal  (hacerVeggie pdepBurger) `shouldBe` 81
            it "Si hago veggie una hamburguesa que ya lo es, el precio no cambia" $ do
                precioFinal  (hacerVeggie dobleCuartoVegano) `shouldBe` 76

        describe "cambiarPanDePati" $ do
            it "Si hago cambio el pan de la bigPdep, su valor final es de 91" $ do
                precioFinal (cambiarPanDePati bigPdep) `shouldBe` 91

        describe "dobleCuartoVegano" $ do
            it "El valor final del doble cuarto vegano es de 76" $ do
                precioFinal dobleCuartoVegano `shouldBe` 76
            
            
