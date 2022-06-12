module EjerciciosTema4_1 where
import EjerciciosTema3_2 
import EjerciciosTema3_1
import Data.Char (ord) 
import Data.Char (chr)


--1
cribar :: [Int] -> Int -> [Int]
cribar l x = [e | e <- l, mod e x /= 0]

cribar2 :: [Int] -> Int -> [Int]
cribar2 [] _ = []
cribar2 (l:ls) x
    | mod l x /= 0 = [l] ++ cribar2 ls x
    | mod l x == 0 = cribar2 ls x


--2
--ceros :: [Int] -> Int


--3
--repeticiones :: [Int] -> ([Int], [Int])
--repeticiones l = foldl() ([],[])
