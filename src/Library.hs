module Library where
import PdePreludat ( Eq((==)), Bool(..), fromInteger, (+), Number, even, (.), String, (>), (>=) )

lista :: [Number]
lista = [1 .. 10]

tupla :: (Number, Number)
tupla = (1, 2)

head' :: [a] -> a
head' (x : xs) = x

tail' :: [a] -> [a]
tail' (x : xs) = xs


fst' :: (a , b) -> a
fst' (primero, segundo) = primero

snd' :: (a , b) -> b
snd' (primero, segundo) = segundo


--ejercicio alumno con sinonimos
type Nombre = String
type Nota = [Number]
type Alumno = (Nombre, Nota)

kevin :: Alumno
kevin = ("Kevin", [10, 9, 8])

nombre :: Alumno -> Nombre
nombre = fst'

notas :: Alumno -> Nota
notas = snd'