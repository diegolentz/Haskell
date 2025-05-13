{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where

import PdePreludat (Bool (..), Eq ((==)), Number, Ord (max, (<=)), Show, String, error, even, fromInteger, (*), (+), (-), (.), (>), (>=), (||), reverse, otherwise, elem, and, or, sum, map, filter, flip, (/), not, (++), (&&))
import Data.Type.Coercion (trans)
import Data.Int (Int)

elMayorDeLosTres :: Ord a => a -> a -> a -> a
elMayorDeLosTres a b c
  | a >= b && a >= c = a
  | b >= a && b >= c = b
  | otherwise = c

xor' :: Bool -> Bool -> Bool
xor' a b
  | a == b = False
  | otherwise = True

esMuchoMayor :: Number -> Number -> Bool
esMuchoMayor a b = a - b > 10

