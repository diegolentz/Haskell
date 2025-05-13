{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where

import PdePreludat (Bool (..), Eq ((==)), Number, Ord (max), Show, String, error, even, fromInteger, (*), (+), (-), (.), (>), (>=), (||), reverse, otherwise, elem, and, or, sum, map)
import Data.Type.Coercion (trans)

data Cliente = Cliente
  { nombre :: String,
    deuda :: Number,
    facturas :: [Number]
  }
  deriving (Show)

palindromo :: String -> Bool
palindromo valor = reverse valor == valor

clientes :: [Cliente]
clientes =
  [ Cliente "Flores" 150 [100, 20, 30],
    Cliente "OhhO"  500 [100, 400],
    Cliente "Almada" 600 [600]
  ]


clientesNombrePalindromo :: [Cliente] -> [Cliente]
clientesNombrePalindromo [] = []
clientesNombrePalindromo (cliente : clientes)
  | (palindromo . nombre) cliente = cliente : clientesNombrePalindromo clientes
  | otherwise = clientesNombrePalindromo clientes

clientesConDeudaMayorA :: Number -> [Cliente] -> [Cliente]
clientesConDeudaMayorA _ [] = []
clientesConDeudaMayorA monto (cliente : clientes)
  | ((> monto) . deuda) cliente = cliente : clientesConDeudaMayorA monto clientes
  | otherwise = clientesConDeudaMayorA monto clientes

clientesConFacturaIgualA :: Number -> [Cliente] -> [Cliente]
clientesConFacturaIgualA _ [] = []
clientesConFacturaIgualA monto (cliente : clientes)
  | (elem monto . facturas) cliente = cliente : clientesConFacturaIgualA monto clientes
  | otherwise = clientesConFacturaIgualA monto clientes

--usamos filter

deudaMayorA :: Number -> Cliente -> Bool
deudaMayorA monto  = (> monto) . deuda

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar criterioFiltrado [] = []
filtrar criterioFiltrado (x : xs)
  | criterioFiltrado x = x : filtrar criterioFiltrado xs
  | otherwise = filtrar criterioFiltrado xs

--usamos map

transformar :: (a -> b) -> [a] -> [b]
transformar _ [] = []
transformar transformador (x : xs) = transformador x : transformar transformador xs

--usamos and
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos condicion lista = (and . transformar condicion) lista

-- any
alguno :: (a -> Bool) -> [a] -> Bool
alguno _ [] = True
alguno condicion lista = (or . transformar condicion) lista

--sumar
sumar :: (a -> Number) -> [a] -> Number
sumar f = sum . map f

