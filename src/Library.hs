{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where

import PdePreludat (Bool (..), Eq ((==)), Number, Ord (max, (<=)), Show, String, error, even, fromInteger, (*), (+), (-), (.), (>), (>=), (||), reverse, otherwise, elem, and, or, sum, map, filter, flip, (/), not, (++), (&&), (^), any, take, length)
import Data.Type.Coercion (trans)
import Data.Int (Int)
import GHC.Num (Num)
import Data.Char (toLower)


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
    Carta "Superman" 100 200 90 300 50 ["alguien", "kryptoniano"],
    Carta "batman" 80 180 85 250 40 ["alguien", "terrestre"],
    Carta "BatWonder Woman" 95 190 88 280 45 ["alien", "amazona"],
    Carta "Flash" 120 170 70 200 60 ["alien", "terrestre"],
    Carta "Aquaman" 85 185 92 260 35 ["alguien", "atlante"]
    ]

--obtener los nombres qe empiezan con bat
obtenerEmpiezaConBat :: [Carta] -> [Carta]
obtenerEmpiezaConBat [] = []
obtenerEmpiezaConBat (carta : cartas)
  | take 3 (map toLower (nombre carta)) == "bat" = carta : obtenerEmpiezaConBat cartas
  | otherwise = obtenerEmpiezaConBat cartas

-- averigar si hay cards con tags muy largos
hayTagsLargos :: [Carta] -> [Carta]
hayTagsLargos [] = []
hayTagsLargos (carta:cartas)
  | contieneTagLargo (tags carta) = carta : hayTagsLargos cartas
  | otherwise = hayTagsLargos cartas

-- Función auxiliar para ver si una lista de tags tiene alguno largo
contieneTagLargo :: [String] -> Bool
contieneTagLargo [] = False
contieneTagLargo (tag:resto)
  | length tag > 10 = True
  | otherwise = contieneTagLargo resto


-- corrgir tag
-- Corrige las cartas reemplazando el tag "alguien" por "alien"
corregirTag :: [Carta] -> [Carta]
corregirTag [] = []
corregirTag (carta:cartas) = carta { tags = corregirTags (tags carta) } : corregirTag cartas

-- Reemplaza "alguien" por "alien" en la lista de tags
corregirTags :: [String] -> [String]
corregirTags [] = []
corregirTags (tag:resto)
  | tag == "alguien" = "alien" : corregirTags resto
  | otherwise        = tag : corregirTags resto
