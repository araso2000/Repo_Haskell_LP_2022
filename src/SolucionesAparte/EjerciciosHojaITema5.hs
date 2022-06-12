
module EjerciciosHojaITema5 where
import Data.Function

--Ejercicio 1

type Nombre = String
type Edad = Integer
type Persona = (Nombre, Edad)

descuento::Persona -> Bool
descuento (nom,edad)= edad >= 65

--Ejercicio 3

type DNI = String
type Expediente = Int
type Nota = Float
data Alumno = A DNI Expediente Nota

aprobado::Alumno -> Bool
aprobado (A n m l) = if l>=5 then True else False

getNota::Alumno -> String
getNota (A n m l)
                | l>= 9 = "Sobresaliente"
                | l>=7 = "Notable"
                | l>=5 = "Suficiente"
                |otherwise = "Suspenso"


getNotaNum::Alumno -> Float
getNotaNum (A n m l) = l

getExp::Alumno -> Int
getExp (A n m l) = m

pepeqwe::Alumno
pepeqwe= A "11111111-H" 121 7

instance Show Alumno where
    show alumno = "Expediente: " ++ show (getExp alumno) ++ ", Nota: " ++ getNota alumno ++ "\n"

dameNota::Alumno->Float
dameNota (A n m l) = l

--Cuando es decimal

--mediaNotas::(Num a, Fractional a)=>[Alumno] -> a
---mediaNotas lista = calculoDecimal lista (length lista)
--
--calculo::Num a => [Alumno] -> a -> a
--calculo [] _= 0
----calculo (x:xs) p = (div (getNotaNum x) p) + (calculo xs p)

-------------
sacarLista::[Alumno] -> [Float]
sacarLista [] = []
sacarLista (x:xs) =  [getNotaNum x] ++ sacarLista xs

calculoDecimal:: [Alumno] -> Float
calculoDecimal lista = sum (sacarLista lista) / (fromIntegral (length lista))



--Ejercicio 5




