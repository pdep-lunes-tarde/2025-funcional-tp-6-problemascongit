{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | BaconDeTofu | PanIntegral
    deriving (Eq, Show)


precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3
precioIngrediente BaconDeTofu = 1

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

modificarIngredientesHamburguesa :: ([Ingrediente] -> [Ingrediente]) -> Hamburguesa -> Hamburguesa
modificarIngredientesHamburguesa funcion hamburguesa = hamburguesa {ingredientes = funcion . ingredientes $ hamburguesa}

modificarprecioBaseHamburguesa :: (Number -> Number) -> Hamburguesa -> Hamburguesa
modificarprecioBaseHamburguesa funcion hamburguesa = hamburguesa {precioBase = funcion . precioBase $ hamburguesa}

sumarIngredientes :: [Ingrediente] -> Number
sumarIngredientes = sum . map precioIngrediente

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = (+ precioBase hamburguesa) . sumarIngredientes . ingredientes $ hamburguesa

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

tieneCarne :: Hamburguesa -> Bool
tieneCarne = elem Carne . ingredientes

tienePatiVegano :: Hamburguesa -> Bool
tienePatiVegano = elem PatiVegano . ingredientes

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | tieneCarne hamburguesa = agregarIngrediente Carne hamburguesa
    | tienePatiVegano hamburguesa = agregarIngrediente PatiVegano hamburguesa
    | otherwise = agregarIngrediente Pollo hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente = modificarIngredientesHamburguesa (++ [ingrediente])


descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje =  modificarprecioBaseHamburguesa (* (1 - (porcentaje / 100)))

pdepBurger :: Hamburguesa
pdepBurger = agrandar . agrandar . agregarIngrediente Panceta . agregarIngrediente Cheddar . descuento 20 $ cuartoDeLibra

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Carne . agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia = agregarIngrediente Papas . descuento 30

reemplazarIngredientes :: (Ingrediente -> Ingrediente) -> Hamburguesa -> Hamburguesa
reemplazarIngredientes funcion = modificarIngredientesHamburguesa (map funcion)

reemplazarPorVeggie :: Ingrediente -> Ingrediente
reemplazarPorVeggie Carne = PatiVegano
reemplazarPorVeggie Pollo = PatiVegano
reemplazarPorVeggie Cheddar = QuesoDeAlmendras
reemplazarPorVeggie Panceta = BaconDeTofu
reemplazarPorVeggie ingrediente = ingrediente

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = reemplazarIngredientes reemplazarPorVeggie

reemplazarPorPanIntegral :: Ingrediente -> Ingrediente
reemplazarPorPanIntegral Pan = PanIntegral 
reemplazarPorPanIntegral ingrediente = ingrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati = reemplazarIngredientes reemplazarPorPanIntegral

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = hacerVeggie . cambiarPanDePati $ dobleCuarto
