module EjerciciosTema3_1 where
import Data.Char (ord) 
import Data.Char (chr)

--1
ordenadosMenor :: Int -> Int -> Int -> Bool
ordenadosMenor a b c
    | a <= b && a <= c && b <= c = True
    |otherwise = False


--2
ordenarTupla :: (Int, Int, Int) -> (Int, Int, Int)
ordenarTupla (x, y, z)
    | ordenadosMenor x y z = (x, y, z)
    | x <= z && x <= y && z <= y = (x, z, y)
    | y <= x && y <= z && x <= z = (y, x, z)
    | y <= z && y <= x && z <= x = (y, z, x)
    | z <= x && z <= y && x <= y = (z, x, y)
    | z <= y && z <= x && y <= x = (z, y, x)


--3
descomponerReal :: Float -> (Int, Int)
descomponerReal x = (truncate x, mod (truncate (x * 100)) 100)


--4
divisores :: Int -> [Int]
divisores n = [x | x <- [1..div n 2], mod n x == 0] ++ [n]


--5
esDigito :: Char -> Bool
esDigito c = (((ord c) >= 48) && ((ord c) <= 57))

--6
esPrimo :: Int -> Bool
esPrimo x 
    | length(divisores x) == 2 = True
    | x == 1 = True
    | otherwise = False

--7
listaPrimosImpares :: [Int] -> [Int]
listaPrimosImpares l = [x | x <- l, odd x && esPrimo x]


--8
primosMenorIgual :: Int -> [Int]
primosMenorIgual n = [x | x <- [1..n], esPrimo x]


--9
esVocal :: Char -> Bool
esVocal x 
    | x == 'a' || x == 'A' = True
    | x == 'e' || x == 'E' = True
    | x == 'i' || x == 'I' = True
    | x == 'o' || x == 'O' = True
    | x == 'u' || x == 'U' = True
    | otherwise = False

codificacionTuplas :: [(Char, Char)] -> [Char]
codificacionTuplas l = [fst e| e <- l, esVocal (snd e)]


--10
filtrarTuplas :: [(Int, Int)] -> Int -> [(Int, Int)]
filtrarTuplas l x = [e | e <- l, even (fst e) && x < (fst e)]


--11
esPitagorica :: (Int, Int, Int) -> Bool
esPitagorica (t1, t2, t3)
    | abs(t1)^2 + abs(t2)^2 == abs(t3)^2 = True
    | otherwise = False

cuantasPitagoricas :: [(Int, Int, Int)] -> Int
cuantasPitagoricas l = length[e | e <- l, esPitagorica e]


--12
esMayuscula :: Char -> Bool
esMayuscula c = ord(c) >= 65 && ord(c) <= 90


--13
cambiaCase :: Char -> Char
cambiaCase c
    | esMayuscula c = chr(ord(c) + 32)
    | otherwise = chr(ord(c) - 32)

mayusculasMinusculas :: String -> String
mayusculasMinusculas s = [cambiaCase (c) | c <- s]


--14
listaASCII :: String -> [Int]
listaASCII s = [ord(c) | c <- s]


--15
mensajeLista :: [Int] -> String
mensajeLista l = "Primer elemento: " ++ show(head l) ++ ", longitud: " ++ show(length l)


--16
contarMayusculas :: String -> Int
contarMayusculas s = length[c | c <- s, esMayuscula c]