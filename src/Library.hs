-- {-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where

import PdePreludat (Bool (..), Eq ((==)), Number, Ord (max, (<=)), Show, String, error, even, fromInteger, (*), (+), (-), (.), (>), (>=), (||), reverse, otherwise, elem, and, or, sum, map, filter, flip, (/), not, (++), (&&), (^), any, take, length)
import Data.Type.Coercion (trans)
import Data.Int (Int)
import GHC.Num (Num)
import Data.Char (toLower)
import Number (Number)

data Carta = Carta {
  nombre :: String,
  velocidad :: Number,
  altura :: Number,
  peso :: Number,
  fuerza :: Number,
  peleas :: Number,
  tags :: [String]
} deriving (Show, Eq)

maso :: [Carta]
maso = [
  Carta "batman" 10 10 10 10 10 ["humano", "vigilante"],
  Carta "superman" 10 10 10 10 10 ["alguien", "super poderoso"],
  Carta "flash" 10 10 10 10 10  ["humano", "velocista"],
  Carta "linterna verde" 10 10 10 10 10 ["alguien", "vigilante"],
  Carta "mujer maravilla" 10 10 10 10 10 ["humano", "avion invisible"],
  Carta "aquaman" 10 10 10 10 10  ["humano", "vigilante del mar"],
  Carta "Hulk" 10 10 10 10 10  ["alguien", "verde"]
  ]

largo :: Number
largo = 10

-- comienzan con bat
esBat :: Carta -> Bool
esBat carta = take 3 (map toLower (nombre carta)) == "bat"

comienzaConBat :: [Carta] -> [Carta]
comienzaConBat [] = []
comienzaConBat (carta:cartas) 
  | esBat carta = carta : comienzaConBat cartas
  | otherwise   = comienzaConBat cartas

-- tags muy largos

esTagLargo :: [String] -> Bool
esTagLargo [] = False
esTagLargo (tag:tags)
  | length tag > largo = True
  | otherwise          = esTagLargo tags 

tagMuyLargo :: [Carta] -> [Carta]
tagMuyLargo [] = []
tagMuyLargo (carta:cartas)
  | esTagLargo (tags carta) = carta : tagMuyLargo cartas
  | otherwise               = tagMuyLargo cartas

-- cambiar alguien por alien
cambiarAlguien :: [Carta] -> [Carta]
cambiarAlguien [] = []
cambiarAlguien (carta:cartas)
  | elem "alguien" (tags carta) = carta {tags = ["alien"]} : cambiarAlguien cartas
  | otherwise                   = cambiarAlguien cartas
