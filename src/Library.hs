{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where

import PdePreludat (Bool (..), Eq ((==)), Number, Ord (max, (<=)), Show, String, error, even, fromInteger, (*), (+), (-), (.), (>), (>=), (||), reverse, otherwise, elem, and, or, sum, map, filter, flip, (/), not, (++), (&&), (^))
import Data.Type.Coercion (trans)
import Data.Int (Int)
import GHC.Num (Num)

sumarLista :: [Number] -> Number
sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs

longitudDeLista :: [a] -> Number
longitudDeLista [] = 0
longitudDeLista (x : xs) = 1 + longitudDeLista xs

numeroElevado :: Number -> Number -> Number
numeroElevado _ 0 = 1
numeroElevado a b = (^b) a

mostrarPrimeros :: Number -> [a] -> [a]
mostrarPrimeros _ [] = []
mostrarPrimeros 0 _ = []
mostrarPrimeros n (x : xs) = x : mostrarPrimeros (n - 1) xs

--no se si esta bien
-- mostrarMientrasNoSuperaTope :: Number -> [Number] -> [Number]
-- mostrarMientrasNoSuperaTope _ [] = []
-- mostrarMientrasNoSuperaTope tope (x:xs)
--     | x > tope = []
--     | otherwise = x : mostrarMientrasNoSuperaTope (tope - x) xs
