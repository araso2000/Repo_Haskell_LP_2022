{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module EjerciciosRepasoExamenIII where

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show


a1 = Rama (Rama (Rama AV 12 AV) 49
                (Rama (Rama AV 23 AV) 5 (Rama AV 13 AV)))
            123 (Rama AV 10 AV)


añadirElemento:: Ord a => Arbol a -> a -> Arbol a
añadirElemento AV e = Rama AV e AV
añadirElemento (Rama izq h der) num
        | num <= h = Rama (añadirElemento izq num) h der
        | num > h = Rama izq h (añadirElemento der num)

lista::Arbol a -> [a]
lista AV = []
lista (Rama izq h der) = [h] ++ lista izq ++ lista der

soloHojas::Arbol a -> [a]
soloHojas (Rama AV h AV) = [h]
soloHojas (Rama izq h der) = soloHojas izq ++ soloHojas der

--Ejercicio 2
vocales:: Char -> Bool
vocales c = c=='a' || c=='e' || c=='i' || c=='u'

noVocales::String -> (String,String)
noVocales lista = foldr(\l (a,b)-> if vocales l then ([l]++a,b) else (a,[l]++b)) ([],[]) lista


--Ejercicio 3
esPar::Num p => [a] -> p -> [(a,p)]
esPar [] _ = []
esPar (x:xs) n = [(x,n)] ++ (esPar xs (n+1))

separarPosicion:: [a] -> ([a],[a])
separarPosicion l = foldr(\(x,y) (a,b)-> if even y then ([x]++a,b) else (a,[x]++b)) ([],[]) (esPar l 0)

--Comprobar

comprobar:: [Int] -> Int
comprobar = foldr (\a b-> if even a then a+2 else b-1) 1