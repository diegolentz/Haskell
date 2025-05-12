module Library where

import PdePreludat ( Eq((==)), Bool(..), fromInteger, (+), Number, even, (.), String, (>), (>=) )
import GHC.Num (Num)

siguiente :: Number -> Number
siguiente numero = numero + 1

siguientePar :: Number -> Bool
siguientePar = even . siguiente

type Persona = (String, Number)

clara :: Persona
clara = ("Clara", 10)

-- calcular edad de una persona
edad :: Persona -> Number
edad (_, edad) = edad

--saber si es mayor de edad
mayorEdad :: Number -> Bool
mayorEdad edad = edad >= 18

-- saber si es mayor de edad
esMayorDeEdad :: Persona -> Bool
esMayorDeEdad = mayorEdad . edad