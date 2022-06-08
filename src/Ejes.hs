module Ejes where

import Data.Char;

--3.1
ordenadosMenor :: Int -> Int -> Int -> Bool
ordenadosMenor x y z = 
		if x < y && y < z
		then True
		else False
		
--3.2
--ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
--ordenarTupla x y z =
--		if ordenadosMenor x y z
--		then x y z
--		else 

--3.3
--descomponerReal :: 	Float -> Float
--descomponerReal x = 1 - ((ceiling x) - x)

--3.4
divisores :: Int -> [Int]
divisores x = [y | y <- [a,a-1..1], x `mod` y == 0] where a = x `div` 2

--3.5
esDigito :: Char -> Bool
esDigito x
			| ord x > 47 && ord x < 58 = True
			| otherwise = False
			
--3.6
esPrimo :: Int -> Bool
esPrimo x = if length (divisores x) == 1 then True else False

--3.7 - Funciona regular
listaPrimosImpares :: [Int] -> [Int]
listaPrimosImpares (x:xs) = [y | y <- [a,a-1..1], (y `div` 2 /= 0) && (esPrimo y)] where a = (length xs)

--3.8
primosMenorIgual :: Int -> [Int]
primosMenorIgual x = [y | y <- [x,x-1..1], (esPrimo y)]

--3.9
--codificacionTuplas :: [(Char,Char)] -> [Char]
--codificacionTuplas (x,y) = [z | z <- x,(ord y) == 97 || (ord y)==101 || (ord y)==105 || (ord y)==111 || (ord y)==118]

--f lista = filter codificacionTuplas lista

--codificacionTuplas ((x,y):zs) = [x | y <- [a,a-1..1], (ord y)==97 || (ord y)==101 || (ord y)==105 || (ord y)==111 || (ord y)==118] where a = (length zs)

--codificacionTuplas (x,y) = [x |

--3.10
filtrarTuplas :: [(Int,Int)] -> Int -> [(Int,Int)]
filtrarTuplas (x, y) n = [(z, t) | z <- x, t <- y, (t `div` 2 /= 0) && (t > n)]
 
--3.12
esMayus :: Char -> Bool
esMayus x = if a > 64 && a < 91 then True else False where a = (ord x)

--3.13
mayusAMinus :: [Char] -> [Char]
mayusAMinus x = [(toLower z) | z <- x,((ord z) > 64 && (ord z) < 91)]

--3.14
listaAscii :: [Char] -> [Int]
listaAscii x = [(ord y) | y <- x]
















