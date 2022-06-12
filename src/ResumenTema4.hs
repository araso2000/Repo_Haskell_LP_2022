module ResumenTema4 where

--Funciones recursivas
factorial :: Int -> Int
factorial 0 = 1 --Caso base
factorial n = n * factorial (n-1)


--Funciones como argumentos de otras funciones
incremento :: Int -> Int
incremento x = x + 1

dosVeces :: (Int -> Int) -> Int -> Int
dosVeces f x = f (f x)

sumac:: Int-> (Int-> Int)
sumac x y = x + y

--Funcion MAP: map f xs , obtiene una lista resultado de aplicar la función f a cada elemento de la lista xs -> ModuleName > map (*2) [3,4,7] [6,8,14]

Pruebas1> map even [1..5]
[False,True,False,True,False]

filter p xs , devuelve los elementos de la lista que cumplen alguna condición
ModuleName filter even [1..10]
[2, 4, 6, 8, 10]


all p xs, verifica si todos los elementos de la lista xs cumplen la propiedad p.
ModuleName> all even [2,4,8]
True

any p xs, verifica si algún elemento de la lista xs cumple la propiedad p.
ModuleName> any even [1,2,3]
True


--Funciones de plegado

foldr (+) e [w,x,y,z] es igual al valor de la expresión (w + (x + (y + (z + e))))

foldl op e [x1,x2,x3] ; (((e op x1) op x2) op x3


--Calculo Lambda: se trata de mini funciones definidas dentro de otro contexto, sin necesidad de crear una funcion completa
invertirLista :: [Int] -> [Int]
invertirLista = foldr (\x lista -> lista ++ [x]) []

ModuleName> invertirLista [1,2,3]
[3,2,1]

odds n = map (\x->x * 2 + 1) [0..n-1]

ModuleName> odds 5
[1,3,5,7,9]





