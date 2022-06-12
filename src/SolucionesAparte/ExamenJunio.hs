{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}







module ExamenJunio where

--Test

patron':: [(Int,Float,Bool)] -> [(Int,Float,Bool)]
patron'[] = []
patron'(p@(x,y,z):xs) = if x > truncate y then p:patron' xs else patron' xs



--Ejercicio 2

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show

numerosEnteros:: Ord a => a -> a -> Arbol a -> [a]
numerosEnteros _ _ AV = []
numerosEnteros x y (Rama izq h der)
    | x < h && y > h = noRepe ([h] ++ numerosEnteros x y izq ++ numerosEnteros x y der)
    | otherwise = numerosEnteros x y izq ++ numerosEnteros x y der

--Antigua Manera

--compro:: Eq a => a -> [a] -> Bool
--compro _ [] = True
--compro n (x:xs) = if n == x then False else compro n xs

noRepe:: Eq a => [a] -> [a]
noRepe [] = []
--noRepe (x:xs) = if compro x xs == True then [x]++noRepe xs else noRepe xs
noRepe l@(x:xs) = x:noRepe nuevo where
    nuevo = filter (/= x) xs

final::Ord a => a -> a -> Arbol a -> [a]
final x t z = noRepe (numerosEnteros x t z)


--Ejercicio 3
type Numero = String
type Nombre = String
type Edad = Int
type Dia = Int
type Mes = Int
type Año = Int
type Diagnostico = String
type Tratamiento = String
type Doctor = String
data Fecha = F Mes Dia Año
data Paciente = P Numero Nombre Edad
data Notas = N Paciente Fecha Diagnostico Tratamiento Doctor

pepe::Paciente
pepe = P "234567uHGFD" "Eduardo ASdf" 57
pepe2::Paciente
pepe2 = P "234543567uHGFD" "Edffddduardo ASdf" 50
pepe3::Paciente
pepe3 = P "234567uHG432FD" "Eduawdfvrdo ASdf" 09

fecha::Fecha
fecha = F 10 02 2012

nota::Notas
nota = N pepe fecha "Diagnostico : Reservado" "Amoxicilina" "Pedro pedro"
nota2::Notas
nota2 = N pepe3 fecha "Diagnostico : Hola" "ibuprofeno" "Pedro pedro"
nota3::Notas
nota3 = N pepe fecha "Diagnostico : Reservadfdscvfdcvfo" "Amoxfwsdvgfrdfvicilina" "Pedro pefdvfdro"

hospital::[Notas]
hospital = [nota2, nota, nota3]

instance Eq Paciente where
    P n m e == P n' m' e' = n==n' && m==m' && e==e'

getNombre::Paciente -> String
getNombre (P n _ _) = n

getMes::Fecha -> Int
getMes (F m d a) = m

getListado::Notas -> Int -> Paciente -> Bool
getListado (N p f _ _ _) num pa = if p==pa && getMes f == num then True else False

--notasClinicas:: Mes Paciente Hospital -> [Notas]
--notasClinicas m p (x:xs)

instance Show Paciente where
    show (P num nom ed) = "Paciente: " ++ show num ++" " ++ show nom ++" " ++ show ed

instance Show Fecha where
    show (F m d a) = ", Ingresa el: " ++ show d ++ "/" ++ show m ++"/"++show a

instance Show Notas where
    show (N p f d t l) = show p ++ show f ++ ", Con " ++ show d ++ ", y se le pauta como tratamiento Tomar " ++ show t ++ ", Firmado el doctor: " ++ show l ++"\n"

notasClinicas::Int -> Paciente -> [Notas] -> [Notas]
notasClinicas _ _ [] = []
notasClinicas mes p (x:xs) = if getListado x mes p then [x] ++ notasClinicas mes p xs else notasClinicas mes p xs







 