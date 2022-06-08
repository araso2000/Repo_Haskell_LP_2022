{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}







module EjerciciosHojaIITema4 where
import Data.Ratio ( (%) )

--Ejercicio 1

doble :: Int -> Int
doble = (\ x->x+x)

--Ejercicio 2

dobled:: [Int] -> Int
dobled = foldr (\a b-> a*2+b) 0

--Ejercicio 3
saberPar:: [Int] -> [Int]
saberPar = filter even

sumaCuadradosPares:: [Int] -> Int
sumaCuadradosPares = foldr(\a b-> a*a+b) 0

ejecutar:: [Int] -> Int
ejecutar lista = sumaCuadradosPares (saberPar lista)

ejecutar2:: [Int] -> [Int]
ejecutar2 lista = [x*x | x<-lista, even x]
siosi:: [Int] -> Int
siosi lista= sum (ejecutar2 lista)

--Ejercicio 4

eliminarNumr:: Int -> [Int] -> [Int]
eliminarNumr num = foldr(\a b-> if a==num then b else [a]++b) []

eliminarNuml:: Eq a => a -> [a] -> [a]
eliminarNuml num = foldl(\a b->if b==num then a else a++[b]) []

--Ejercicio 5
comprobarDu:: [Int] -> Int-> Bool
comprobarDu [] _ = True
comprobarDu (x:xs) num = if x==num then False else comprobarDu xs num

noDuplicados:: [Int] -> [Int]
noDuplicados = foldl(\a b-> if comprobarDu a b then a++[b] else a) []

--Ejercicio 6
contarRec :: Eq a => [a] -> [(a, Int)]
contarRec [] = []
contarRec l@(x:xs) = (x, freq):contarRec l_sin_x where
    freq = length (filter (== x) l) 
    l_sin_x = filter (/= x) xs

--Ejermplo de Contar cuanto se repite el prier valor de la lista en ella
contar :: Eq a => [a] -> Int
contar [] = 0;
contar l@(x:xs) = nuevo where
    nuevo = length (filter (== x) l)
-------------------------------------------------------




