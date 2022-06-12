module EjerciciosRepaso where

--Ejercicio 1

pertenece::Eq a => a -> [a] -> Bool
pertenece num = foldr(\x l-> l || (x==num)) False

--Ejercicio 2

eliminar :: Eq a => a -> [a] -> [a]
eliminar num = foldr(\x l-> if x==num then l else [x]++l) []

--Ejercicio 3

productoEscalar:: [Float] -> [Float] -> Float
productoEscalar l1 l2 = foldl (+) 0.0 ([x*y | (x,y) <- zip l1 l2])