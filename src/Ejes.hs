module Ejes where

import Data.Char;

--3.1
ordenadosMenor :: Int -> Int -> Int -> Bool
ordenadosMenor x y z = 
		if x < y && y < z
		then True
		else False
		
--3.2
ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
ordenarTupla (x,y,z)
		| ((x <= y) && (y <= z)) = (x, y, z)
		| ((x <= z) && (z <= y)) = (x, z, y)
		| ((y <= x) && (x <= z)) = (y, x, z)
		| ((y <= z) && (z <= x)) = (y, z, x)
		| ((z <= x) && (x <= y)) = (z, x, y)
		| otherwise = (z, y, x)

--3.3
descomponerReal :: Float -> (Int, Int)
descomponerReal x = (truncate x, mod (truncate (x * 100)) 100)

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
listaPrimosImpares (x:xs) = [y | y <- xs, (y `div` 2 /= 0) && (esPrimo y)]

--3.8
primosMenorIgual :: Int -> [Int]
primosMenorIgual x = [y | y <- [x,x-1..1], (esPrimo y)]

--3.9
esVocal :: Char -> Bool
esVocal x 
    | x == 'a' || x == 'A' = True
    | x == 'e' || x == 'E' = True
    | x == 'i' || x == 'I' = True
    | x == 'o' || x == 'O' = True
    | x == 'u' || x == 'U' = True
    | otherwise = False

codificacionTuplas :: [(Char, Char)] -> [Char]
codificacionTuplas l = [fst e| e <- l, esVocal (snd e)]

--3.10
filtrarTuplas :: [(Int, Int)] -> Int -> [(Int, Int)]
filtrarTuplas l x = [e | e <- l, even (fst e) && x < (fst e)]

--3.11
esPitagorica :: (Int, Int, Int) -> Bool
esPitagorica (t1, t2, t3)
    | abs(t1)^2 + abs(t2)^2 == abs(t3)^2 = True
    | otherwise = False

cuentasPitagoricas :: [(Int, Int, Int)] -> Int
cuentasPitagoricas l = length[e | e <- l, esPitagorica e]
 
--3.12
esMayus :: Char -> Bool
esMayus x = if a > 64 && a < 91 then True else False where a = (ord x)

--3.13
mayusAMinus :: [Char] -> [Char]
mayusAMinus x = [(toLower z) | z <- x,((ord z) > 64 && (ord z) < 91)]

--3.14
listaAscii :: [Char] -> [Int]
listaAscii x = [(ord y) | y <- x]

--3.15
mensajeLista :: [Int] -> String
mensajeLista x = "Primer elemento: " ++ show(head x) ++ ". Longitud de la lista: " ++ show(length x)

--3.16
contarMayusculas :: String -> Int
contarMayusculas s = length[c | c <- s, esMayus c]


--HOJA 3.2
--3.1
contarApariciones :: String -> Char -> Int
contarApariciones str c = length[e | e <- str, e == c]

--3.2
manipula3Tuplas :: ((String, Int), (String,Int), (String, Int)) -> (String, String, String)
manipula3Tuplas ((s1, _), (s2, _), (s3, _)) = (s1, s2, s3)

--3.3
sumaMenor10 :: [Int] -> Bool
sumaMenor10 (a:b:c:d:xs) = if (a+b+c+d) < 10 then True else False

--3.4
puntoCardinal :: Char -> String
puntoCardinal 'N' = "Norte"
puntoCardinal 'S' = "Sur"
puntoCardinal 'E' = "Este"
puntoCardinal 'O' = "Oeste"
puntoCardinal otherwise  = "El caracter introducido no pertenece a un punto cardinal"

--3.5
todosIguales :: Int -> [Int] -> Bool
todosIguales n (x:xs) = if (((length [y | y <- xs, y==n]) == a) && a > 0) then True else False where a = (length(xs))

--3.6
mensajeFrase :: String -> String
mensajeFrase str = "La primera letra de la frase es: " ++ show(head str) ++ " y la ultima letra es " ++ show(last str) 

--3.7
clasificarValorEntrada :: Int -> String
clasificarValorEntrada x 
					| x < 10				= "El valor de entrada es menor que 10"
					| x >= 10 && x <= 20	= "El valor de entrada es mayor o igual a 10 y menor o igual a 20"
					| x > 20 				= "El valor de entrada es mayor que 20"
					
--3.8
divisoresPropios :: Int -> [Int]
divisoresPropios n = [x | x <- [1..a], (n `rem` x == 0)] where a = (n `div` 2)

sumaDivisores :: [Int] -> Int
sumaDivisores = foldr (+) 0

amigos :: (Int,Int) -> Bool
amigos (a,b) = if (sumaDivisores(divisoresPropios a) == b) && (sumaDivisores(divisoresPropios b) == a) then True else False

--3.9
esConsonante :: Char -> Bool
esConsonante c = if esVocal c && a >= 65 && a <= 122 then True else False where a = ord(c)

contarConsonantes :: String -> Int
contarConsonantes str = length[x | x <- str, (esConsonante x) == False]

--3.10
mersenne :: Int -> [Int]
mersenne n = 


--HOJA 4.1

--4.1.
cribar :: [Int] -> Int -> [Int]
cribar (x:xs) n = [y | y <- xs, (y `mod` n) /= 0]

--4.2
ceros :: [Int] -> Int
ceros (x:xs) = length[y | y <- xs, y==0]

--4.3
existeEnLista :: [Int] -> Int -> Bool
existeEnLista (x:xs) n = elem n xs

repeticiones :: [Int] -> [Int]
repeticiones (x:xs) = [y | y <- xs]

--4.4
--incluye :: [Int] -> [Int] -> Bool
--incluye (x:xs) (y:ys) = [z | z <- xs,

--4.5



--HOJA 4.2
--4.1
doble :: Int -> Int
doble = (\ x->x+x)

--4.2
sumaDobles :: [Int] -> Int
sumaDobles x = foldr (+) 0 (map(doble) x)

-- Con recursividad no final
sumarDouble1 :: [Int] -> Int
sumarDouble1 [] = 0
sumarDouble1 (x:xs) = (doble x) + (sumarDouble1 xs)

-- Con recursividad final
sumarDouble2 :: [Int] -> Int -> Int
sumarDouble2 [] ac = ac
sumarDouble2 (x:xs) ac = sumarDouble2 xs ((doble x) + ac)

--4.3
cuadrado :: Int -> Int
cuadrado x = x*x

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares x = foldr (+) 0 (map(cuadrado) (filter even x))

--Con listas por compresión

sumaCuadradosPares' :: [Int] -> [Int]
sumaCuadradosPares' (x:xs) = [2*(y*y) | y <- xs, (even y)]

--sumaCuadradosPares'' :: [Int] -> Int
--sumaCuadradosPares'' x = foldr (+) 0 (map(sumaCuadradosPares') x)

--4.4
eliminaValor :: Int -> [Int] -> [Int]
eliminaValor n = foldr(\a b-> if a==n then b else [a]++b) []

eliminaValor' :: Int -> [Int] -> [Int]
eliminaValor' n = foldl(\a b-> if b==n then a else a++[b]) []

--4.5
eliminaDuplicados :: [Int] -> [Int]
eliminaDuplicados = foldl(\a b-> if (elem b a) then a else a++[b]) []

--4.6
listaPrimos :: [Int] -> [Int]
listaPrimos (x:xs) = filter esPrimo xs

--Con recursividad
--listaPrimos' :: [Int] -> [Int]
--listaPrimos' [] = 0
--listaPrimos' (x:xs) = 

