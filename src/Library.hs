-- {-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Library where

import Data.Char (toLower)
import Data.Int (Int)
import Data.Type.Coercion (trans)
import GHC.Num (Num)
import Number (Number)
import PdePreludat (Bool (..), Eq ((==)), Number, Ord (max, (<=)), Show, String, and, any, elem, error, even, filter, flip, fromInteger, length, map, not, or, otherwise, reverse, sum, take, (&&), (*), (+), (++), (-), (.), (/), (>), (>=), (^), (||))

type Titulo = String

type Autor = String

data Obra = UnaObra {titulo :: Titulo, autores :: [Autor]}
  deriving (Show, Eq)

todasLasObras :: [Obra]
todasLasObras = [UnaObra "sandman" ["gaiman"], UnaObra "socorro" ["ElsaBornemann"], UnaObra "buenos presagios" ["gaiman", "prachett"]]

-- es cierto que alguien escribio determinada obra?
-- escribioObra :: Autor -> Obra -> Bool
-- escribioObra autor obra = map toLower autor `elem` autores obra

escribioObra :: Autor -> Titulo -> [Obra] -> Bool
escribioObra autor tituloBuscado = any (\obra -> titulo obra == tituloBuscado)

autoresDe :: Titulo -> [Obra] -> [Autor]
autoresDe unTitulo todasLasObras =
  case filter (\obra -> titulo obra == unTitulo) todasLasObras of
    [] -> [] -- Si no hay obras con ese título, devuelve una lista vacía
    (obra : _) -> autores obra -- Si hay una obra, devuelve sus autores

-- que obra escribio cierta persona
queObraEscribio :: Autor -> [Obra] -> [Obra]
queObraEscribio autor = filter (\obra -> autor `elem` autores obra)

-- es cierto que cierta persona escribio alguna obra
esCiertoQueEscribio :: Autor -> [Obra] -> Bool
esCiertoQueEscribio autor = any (\obra -> autor `elem` (autores obra) )

--es cierto qe la obra existe
obraExiste :: Titulo -> [Obra] -> Bool
obraExiste tituloBuscado = any (\obra -> tituloBuscado == (titulo obra) )