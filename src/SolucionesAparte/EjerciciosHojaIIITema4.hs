{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module EjerciciosHojaIIITema4 where

--Ejercicio 1

mezclar:: [a] -> [b] -> [(a,b,b)]
mezclar _ [] = []
mezclar [] _ = []
mezclar l@(x:xs) k@(y:z:ys) = if length (xs) >= 1 && length (ys) >= 2 then (x,y,z) : mezclar xs ys else [(x,y,z)]

--Ejercicio 2
alFinal:: a -> [a] -> [a]
alFinal n = foldr (\x xs -> if not(null xs) then [x]++xs else [x]++[n]) []


alFinal':: a -> [a] -> [a]
alFinal' n (x:xs) = if not(null xs) then [x] ++ alFinal n xs else [x]++[n]

--Ejercicio 3

conFuncion:: Eq a => (a->Bool) -> [a] -> [a]
conFuncion _ [] = []
conFuncion f (x:xs) = if f x then x : conFuncion f listaN else conFuncion f listaN where
     listaN = filter(/= x) xs

--Ejercicio 4
comprobarPosicion::Num b =>[a] -> b -> [(a,b)]
comprobarPosicion [] _ = []
comprobarPosicion (x:xs) num = (x,num):comprobarPosicion xs (num+1)

sacarPosicion:: Eq a => [(a,b)] -> a -> [b]
sacarPosicion lista n = foldr(\(l,t) x-> if l==n then [t]++x else x) [] lista

posicionesElem:: (Eq a, Num b) => (a,[a]) -> [b]
posicionesElem (v,lista) = sacarPosicion (comprobarPosicion lista  0) v


--Ejercicio 5

continue:: Eq a => a -> [a] -> Int
continue e = foldr(\l x-> if l==e then x+1 else x) 0
comprobarContinue:: Eq a => a -> [a] -> Bool 
comprobarContinue n lista= if continue n lista > 0 then True else False

continue':: Eq a => a -> [a] -> Int
continue' e = foldl(\l x-> if x==e then l+1 else l) 0
comprobarContinue':: Eq a => a -> [a] -> Bool 
comprobarContinue' n lista= if continue' n lista > 0 then True else False


--Ejercicio 6



--Hoja Tema 5

--Ejercicio 10
class Joinable c where
    join:: c -> c -> c

instance Joinable Integer where
    join a b = a+b

--Ejercicio 11

instance Joinable [a] where
    join a b = a++b

--Ejercicio 6

class Coleccion c where
    esVacias :: c a -> Bool
    insertar :: a -> c a -> a
    primero :: c a -> a
    eliminar :: c a -> c ab
    size :: c a -> Int

data Pila a = Pil [a]

instance Coleccion Pila where
    esVacias(Pil []) = True
    esVacias(Pil b) = False


