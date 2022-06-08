{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module ExamenAbril where

data Categoria = ATP1000 | ATP500 | ATP250 | GrandSlam deriving Show
type Nombre = String
data Torneo = Tor Nombre Categoria
data Temporada = Temp [Torneo]

openAustralia :: Torneo
openAustralia = Tor "Open de Australia" GrandSlam
indianWells :: Torneo
indianWells = Tor "Indian Wells" ATP1000
mutuaMadridOpen :: Torneo
mutuaMadridOpen = Tor "Mutua Madrid Open" ATP1000
wimbledon :: Torneo
wimbledon = Tor "Wimbledon" GrandSlam
temporada2013 :: Temporada
temporada2013 = Temp [openAustralia, indianWells, mutuaMadridOpen, wimbledon]

getCategoria::Torneo -> String
getCategoria (Tor _ c) = show c
getNombre::Torneo -> Nombre
getNombre (Tor n _) = n

instance Show Torneo where
    show torneo = "Torneo: " ++ getNombre torneo ++ "- Categoria: " ++ getCategoria torneo ++ "\n"
torneosPorCategoria::Temporada -> [Torneo]
torneosPorCategoria (Temp lista)= foldr(\l x-> if getCategoria l == "GrandSlam" then [l]++x else x) [] lista

--Ejercicio 5

data Arbol a = AV | Rama (Arbol a) a (Arbol a)

separarNodos:: Arbol a -> [a]
separarNodos AV = []
separarNodos (Rama izq h der) = [h] ++ separarNodos izq ++ separarNodos der

calcularRepetidos::Eq a =>[a] -> [(a,Int)]
calcularRepetidos [] = []
calcularRepetidos l@(x:xs) = [(x,y)]++calcularRepetidos lista where
    y = length (filter (==x) l)
    lista = filter(/= x) xs

final::Eq a =>Arbol a -> ([a],[a])
final e = foldr(\(x,y) (a,b)-> if y>1 then ([x]++a,b) else (a,[x]++b)) ([],[]) (calcularRepetidos (separarNodos e))