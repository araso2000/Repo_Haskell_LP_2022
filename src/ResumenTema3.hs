module ResumenTema3 where

funcion1 :: Int -> Int
funcion1 x = x + x


f :: (Int, Int) -> (Int, Int)
f (m, n)  = (m + n , m - n)


max:: (Int, Int) -> Int
max (m, n)  = if m > n then m else n


paridad :: Int -> Bool
paridad x = case (x `mod` 2) of
							0 -> True
							1 -> True
							otherwise -> False
							

mayor :: (Int , Int)->Int
mayor (x, y)
			| x > y  = x
			| otherwise = y
			

[1, 2, 3, 4, 5, 6] :: [Int]
--Esto equivale a
[1..6] :: [Int]


['a'..'z']
--equivale a
"abcdefghijklmnopqrstuvwxyz"

1:[3,10]
--equivale a
[1, 3, 10]

[1,2,3] ++ [4,5] = [1,2,3,4,5]

concat [[1,2],[],[3,2,1]] = [1,2,3,2,1]

reverse “Dabale el abad” = “daba le elabaD

length [2,3,6,777,8] = 5

head [1,2,3,4] = 1
tail [1,2,3,4] = [2,3,4]

init [1,2,3,4] = [1,2,3]
last [1,2,3,4] = 4

take 3 [1,2,3,4] = [1,2,3]
drop 1 [1,2,3,4] = [2,3,4]

--Obtener posicion 3
[2,1,3,5,4]!!3 = 5

zip[0..3]”hola” = [(0,’h’),(1,’o’),(2,’l’),(3,’a’)]

unzip [(1,True),(4,True),(3,False)] = ([1,4,3],[True,True,False])

[x*x| x <- [1..10]]
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

[(x,y)| x <- [1..3], y<- [1..2]]
[(1,1),(1,2),(2,1)(2,2),(3,1),(3,2)]

[x*x| x <- [1..10], even x]
[4, 16, 36, 64, 100]


procesarUrl' :: String -> String -> String 
procesarUrl' "http" uri = "Peticion por http: " ++ uri 
procesarUrl' "ftp" uri = "Peticion por ftp: " ++ uri 
procesarUrl' "sftp" uri = "Peticion por sftp: " ++ uri
procesarUrl' _ uri = "protocolo desconocido"


suma :: [Int] -> Int
suma [] = 0
suma [x] = x
suma [x,y] = x+y
suma [x,y,z] = x+y+z

f :: Integer -> Bool
f 1 = True
f 2 = False

[] lista vacia
[x] lista de un elemento
[x,y,z,t,] lista de 4 elementos
(x:xs) lista de al menos un elemento
(x:y:zs) listas de al menos 2 elementos

suma :: [Integer] -> Integer
suma [] = 0
suma [x,y,z] = x+y+z

primero3 :: (Int,Int,Int) -> Int
primero3 (x,y,z) = x

primero' :: [(Int,Int)] -> Int
primero' ((x,y):xs) = x

patron:: [(Int,Float,Bool)] -> [(Int,Float,Bool)]
patron [] = []
patron ((x,y,z):xs) = if (x > truncate y) then (x,y,z):patron xs else patron xs

patron':: [(Int,Float,Bool)] -> [(Int,Float,Bool)]
patron'[] = []
patron'(p@(x,y,z):xs) = if (x > truncate y) then p:patron' xs else patron' xs

> patron [(2,3.5,True),(6,5.2,False)] 
[(6,5.2,False)]
> patron'[(2,3.5,True),(6,5.2,False)]
[(6,5.2,False)]


isZero :: Int -> Bool
isZero x
	| 0 = True
	|otherwise = False
	
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False


f:: Int -> Int
f x = g (cuadrado (max  x 4)) + (if x <= 1 then 1 else g (cuadrado (max x 4))

f:: Int -> Int
f x  = f1(g(cuadrado(max x 4)), x))

f1:: (Int,Int) -> Int
f1(a,b) = a + (if b <= 1 then 1 else a)

f:: Int-> Int
f x = a + (if x <= 1 then 1 else a)
	where
		a = g(cuadrado(max x 4))

--O tambien:
f:: Int -> Int
f x = let a = g(cuadrado(max x 4))
		in a + (if x <= 1 then 1 else a)









			

