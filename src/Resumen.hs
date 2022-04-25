module Resumen where

import Data.Char;

doble :: Int -> Int
doble x = 2 * x

suma' :: Int -> Int -> Int
suma' x y = (x + y)

multiplo :: Integer -> Integer -> Bool
multiplo p n = n `mod` p == 0

par :: Integer -> Bool
par = multiplo 2

composicion :: Int -> Int -> Int
composicion x = (suma' x).doble

celsius :: Int -> Int
celsius f = (f-32) * 5 `div` 9

multiploDe :: Int -> Int -> Bool
multiploDe p n = n `mod` p == 0

esPar :: Int -> Bool
esPar = multiploDe 2

sucesor :: Int -> Int
sucesor = (1 +)

esmayus :: Char -> Bool
esmayus c = isUpper c

divEntera:: (Int, Int) -> (Int, Int)
divEntera (m,n) = (m `div` n, m `rem` n)

paridad :: Int -> Bool
paridad x = case (x `mod` 2) of
							0	-> True
							otherwise	-> False
							
mayor :: (Int, Int)->Int
mayor (x, y)
			| x > y = x
			| otherwise = y


procesarUrl' :: String -> String -> String 
procesarUrl' "http" uri = "Peticion por http: " ++ uri 
procesarUrl' "ftp" uri = "Peticion por ftp: " ++ uri 
procesarUrl' "sftp" uri = "Peticion por sftp: " ++ uri
procesarUrl' _ uri = "protocolo desconocido"


f :: [Integer] -> Integer
f [1,x,y] = x + y


suma :: [Int] -> Int
suma [] = 0
suma [x] = x
suma [x,y] = x+y
suma [x,y,z] = x+y+z

patron:: [(Int,Float,Bool)] -> [(Int,Float,Bool)]
patron [] = []
patron ((x,y,z):xs) = if (x > truncate y) then
							(x,y,z):patron xs
					  else patron xs

					  
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False


headOrDefault :: Int -> [Int] -> Int
headOrDefault def list =
    if null list
    then def
    else head list
    

sign :: Int -> String
sign n
    | n == 0    = "Zero"
    | n < 0     = "Negative"
    | otherwise = "Positive"
    
    
sameThreeAround :: [Int] -> Bool
sameThreeAround list =
    let firstThree = take 3 list
        lastThree  = reverse (take 3 (reverse list))
    in firstThree == lastThree
    

eval :: Char -> Int -> Int -> Int
eval op x y = case op of
    '+' -> x + y
    '-' -> x - y
    '*' -> x * y
    '/' -> div x y
    _ -> 0
    
    
-- check if a list has at least two elements
atLeastTwo :: [Int] -> Bool
atLeastTwo [_, _] = True
atLeastTwo _      = False


secondIsZero :: [Int] -> Bool
secondIsZero (_ : 0 : _) = True
secondIsZero _ = False


suma2 :: [Int] -> Int
suma2      []  = 0
suma2 (x : xs) = x + sum xs

showTriple :: (Bool, Int, String) -> String
showTriple (b, n, string) =
    if b
    then "The number is: " ++ show n
    else "The string is: " ++ string
    
    
    
    
    
data User = MkUser
    { userName    :: String
    , userAge     :: Int 
    , userIsTired :: Bool
    } 
    
    
getUserName :: User -> String
getUserName (MkUser name _ _) = name

getUserAge :: User -> Int
getUserAge (MkUser _ age _) = age

setUserName :: String -> User -> User
setUserName name (MkUser _ age isTired) = MkUser name age isTired

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-1)

incremento :: Int -> Int
incremento x = x + 1
dosVeces :: (Int -> Int) -> Int -> Int
dosVeces f x = f (f x)


sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

sumaLista' :: [Int] -> Int
sumaLista' = foldr (+) 0

productoLista :: [Int] -> Int
productoLista = foldr (*) 1

verdad :: [Bool] -> Bool
verdad = foldr (&&) True

concatenar:: [[Int]] -> [Int]
concatenar = foldr (++) []

invertirLista :: [Int] -> [Int]
invertirLista = foldr (\x lista -> lista ++ [x]) []

impares n = map f [0..n-1]
		where f x = x * 2 + 1
		
odds n = map (\x->x * 2 + 1) [0..n-1]

--type Nombre = String
--type Edad = Integer
--type Persona = (Nombre, Edad)
--tocayos:: Persona -> Persona -> Bool
--tocayos (n,_) (nombre,_) = n == nombre

data Temperatura = Frio | Caliente deriving Show
data Estacion = Primavera | Verano | Otonyo | Invierno deriving Show

tiempo :: Estacion -> Temperatura
tiempo Primavera = Caliente
tiempo Verano = Caliente
tiempo _ = Frio


type Nombre = String
type Edad = Integer
data Persona = Pers Nombre Edad deriving Show
juan :: Persona
juan = Pers "Juan Lopez" 23
clara :: Persona
clara = Pers "Clara Fuentes" 35


esJoven :: Persona ->Bool
esJoven (Pers _ edad) = edad < 25


verPersona :: Persona -> String
verPersona (Pers nombre edad) = "Persona, nombre" ++ nombre ++ ", edad: " ++ show edad


data Arbol a = AV | Rama (Arbol a) a (Arbol a)

a2:: Arbol [Integer]
a2 = Rama (Rama AV [1,2,3] AV) [10] (Rama AV [4,5,6] AV)


hojas :: Arbol a -> [a]
hojas AV = []
hojas (Rama AV h AV) = [h]
hojas (Rama izq r der) = hojas izq ++ hojas der
