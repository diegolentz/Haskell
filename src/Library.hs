module Library where

import GHC.Num (Num)
import PdePreludat (Bool (..), Eq ((==)), Number, Show, String, even, fromInteger, (+), (.), (>), (>=))

-- data Persona = Persona String Number

-- nombre :: Persona -> String
-- nombre (Persona _nombre _) = _nombre

-- edad :: Persona -> Number
-- edad (Persona _ _edad) = _edad

-- mayorEdad :: Number -> Bool
-- mayorEdad edad = edad >= 18

-- personaMayor :: Persona -> Bool
-- personaMayor = mayorEdad . edad

-- data estructura
data Persona = Persona
  { nombre :: String,
    edad :: Number,
    domicilio :: String,
    telefono :: String,
    fechaNacimiento :: (Number, Number, Number),
    buenaPersona :: Bool,
    plata :: Number
  }
  deriving (Show)

diego :: Persona
diego =
  Persona
    { nombre = "Diego",
      edad = 30,
      domicilio = "Av. Siempre Viva 123",
      telefono = "123456789",
      fechaNacimiento = (1, 1, 1990),
      buenaPersona = True,
      plata = 1000
    }

lucas :: Persona
lucas = Persona "lucas" 25 "Av. Siempre Viva 456" "987654321" (2, 2, 1995) True 500

-- persona cumple anio

cumplirAnio :: Persona -> Persona
cumplirAnio persona =
  persona
    { edad = edad persona + 1
    }
