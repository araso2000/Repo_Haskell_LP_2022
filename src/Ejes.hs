module Ejes where

--3.1
ordenadosMenor :: Int -> Int -> Int -> Bool
ordenadosMenor x y z = 
		if x < y && y < z
		then True
		else False
		
--3.2
ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
ordenarTupla x y z=
		if (ordenadosMenor x y z)
		then x y z
		