module Library where
import PdePreludat ( Eq((==)), Bool(..), fromInteger, (+), Number, even, (.), String, (>), (>=), (*), (-), (||), Ord (max), error )

factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n - 1)

longitude :: [a] -> Number
longitude [] = 0
longitude (x:xs) = 1 + longitude xs

ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

tomar :: Number -> [a] -> [a]
tomar 0 xs = []
tomar n [] = []
tomar n (x:xs) | n > 0 = x : tomar (n - 1) xs

esta :: Eq a => a -> [a] -> Bool
esta elemento [] = False
esta elemento (x:xs) = elemento == x || esta elemento xs


--En Haskell, la clase de tipo Ord se utiliza para tipos cuyos valores pueden ser comparados en términos de orden. Esto incluye operaciones como <, <=, >, >=, y también permite determinar el máximo o mínimo entre dos valores.

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:y:ys) = maximo (x `max` y : ys)




