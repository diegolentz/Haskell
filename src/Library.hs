{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where

import PdePreludat (Bool (..), Eq ((==)), Number, Ord (max, (<=)), Show, String, error, even, fromInteger, (*), (+), (-), (.), (>), (>=), (||), reverse, otherwise, elem, and, or, sum, map, filter, flip, (/), not, (++))
import Data.Type.Coercion (trans)

cuadruple :: Number -> Number
cuadruple x = x * 4

largoDeListon :: Number
largoDeListon = 300

maderaCuadro :: Number -> Number
maderaCuadro = cuadruple

meAlcanza :: Number -> Bool
meAlcanza largo = largo <= largoDeListon

cuantoMeSobra :: Number -> Number
cuantoMeSobra cantidad = largoDeListon - cantidad

quePorcentaje :: Number -> Number
quePorcentaje cantidad = (cantidad / largoDeListon) * 100

--func compuestas
puedoHacerCuadroDe :: Number -> Bool
puedoHacerCuadroDe lado = meAlcanza (maderaCuadro lado)

puedoHacerCuadradoDe :: Number -> Bool
puedoHacerCuadradoDe = meAlcanza . maderaCuadro

-- ej 2

esMayorEdad :: Number -> Bool
esMayorEdad edad = edad >= 18

esMenorEdad :: Number -> Bool
esMenorEdad edad = not (esMayorEdad edad)

nombreFormateado :: String -> String -> String
nombreFormateado nombre apellido = apellido ++ " " ++ nombre
