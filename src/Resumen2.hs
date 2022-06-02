module Resumen2 where

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






			

