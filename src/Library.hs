{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where

import PdePreludat (Bool (..), Eq ((==)), Number, Ord (max), Show, String, error, even, fromInteger, (*), (+), (-), (.), (>), (>=), (||), reverse, otherwise, elem, and, or, sum, map, filter, flip)
import Data.Type.Coercion (trans)

data Ciudad = Ciudad
  { nombre :: String,
    temperaturaPromedio :: Number
  }
  deriving (Show)

ciudades :: [Ciudad]
ciudades =
  [ Ciudad "Buenos Aires" 20,
    Ciudad "CABA" 25,
    Ciudad "La Plata" 30,
    Ciudad "Rosario" 35,
    Ciudad "Córdoba" 40
  ]

hacemasDe :: Ciudad -> Number -> Bool
hacemasDe ciudad temperatura  =  ( (> temperatura) .temperaturaPromedio  ) ciudad

ciudadesCalurosas :: [Ciudad] -> [Ciudad]
ciudadesCalurosas = filter (flip hacemasDe 30)
