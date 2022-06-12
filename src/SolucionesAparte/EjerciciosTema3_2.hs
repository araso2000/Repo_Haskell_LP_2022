module EjerciciosTema3_2 where
import EjerciciosTema3_1
import Data.Char (ord) 
import Data.Char (chr)

--1
contarApariciones :: String -> Char -> Int
contarApariciones "" _ = 0
contarApariciones _ ' ' = 0
contarApariciones str c = length[e | e <- str, e == c]


--2
manipula3Tuplas :: ((String, Int), (String,Int), (String, Int)) -> (String, String, String)
manipula3Tuplas ((s1, _), (s2, _), (s3, _)) = (s1, s2, s3)


--3
sumaMenor10 :: [Int] -> Bool
sumaMenor10 (a:b:c:d:xs)
    | 10 > (a + b + c + d) = True
    | otherwise = False


--4
puntoCardinal :: Char -> String
puntoCardinal p
    | p == 'N' = "Norte"
    | p == 'S' = "Sur"
    | p == 'E' = "Este"
    | p == 'O' = "Oeste"
    | otherwise = "Caracter introducido incorrecto"


--5
todosIguales :: Int -> [Int] -> Bool
todosIguales n l = length[e | e <- l, n == e] == length l 


--6
mensajeFrase :: String -> String
mensajeFrase s = "La primera letra es " ++ show (head s) ++ ", y el ultimo " ++ show (last s)


--7
clasificarValorEntrada :: Int -> String
clasificarValorEntrada x
    | x < 10 = interval1
    | x >= 10 && x <= 20 = interval2
    | otherwise = interval3
        where
            interval1 = "Valor menor que 10"
            interval2 = "Valor mayor o igual a 10 y menor o igual que 20"
            interval3 = "Valor mayor que 20"


--8
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista(xs)

amigos :: (Int, Int) -> Bool
amigos (a, b)
    | sumaLista (init (divisores a)) == b && sumaLista (init (divisores b)) == a = True
    | otherwise = False


--9
esConsonante :: Char -> Bool
esConsonante c
    | ((ord c) >= 65 && (ord c) <= 90 || (ord c) >= 97 && (ord c) <= 122) && not (esVocal c) = True
    | otherwise = False

contarConsonantes :: String -> Int
contarConsonantes s = length[c | c <- s, esConsonante c]


--10

--11
listasIguales :: [Int] -> [Int] -> Bool
listasIguales a b = length[i |i  <- a, [j | j <- b, j == i] /= []] == length a


--12
head' :: [Int] -> Int
head' (x:_) = x


--13
tail' :: [Int] -> [Int]
tail' (_:xs) = xs


--14
mayorDivision :: Int -> Int -> Int
mayorDivision x y
    | (mod x y) >= (div x y) = (mod x y)
    | otherwise = (div x y)


--15
sumaTipos :: Int -> Double -> Double
sumaTipos x y = fromIntegral x + y


--16 
cuadruple :: Int -> Int
cuadruple x = x*4


--17
calificacion :: Int -> String
calificacion x
    | x >= 0 && x < 5 = "Suspenso"
    | x >= 5 && x < 7 = "Aprobado"
    | x >= 7 && x < 9 = "Notable"
    | x >= 9 && x < 10 = "Sobresaliente"
    | x == 10 = "Matricula de Honor"
    | otherwise = "No valido"

--calificacion' :: Int -> String
--calificacion' x
--    if (x >= 0 && x < 5) then
--        "Suspenso"
--    else if (x >= 5 && x < 7) then 
--        "Aprobado" 
--    else if (x >= 7 && x < 9) then
--        "Notable"
--    else if (x >= 9 && x < 10) then
--        "Sobresaliente"
--    else if x == 10 then 
--        "Matricula de Honor" 
--    else 
--        "No valido"   

--18
cuadrado :: [Int] -> [Int]
cuadrado x = [i^2 | i <- x, even i ]


--19
posicionEnLista :: [Int] -> [(Int, Int)]
posicionEnLista x = zip x [0..(length x)-1]


--20
--long :: [Int] -> Int
--long [] = 0
--long (_:xs) = 1 + long xs
long :: [Int] -> Int
long [] = 0
long l = sum [1 | _ <- l]


--21
contiene :: Int -> [Int] -> Bool
contiene x l = length[e | e <- l, x == e] >= 1


--22
primeros :: [(Char, Int)] -> String
primeros l = [e | (e,_) <- l]


--23
primerosPares :: [(Char, Int)] -> String
primerosPares l = [x | (x,y) <- l, even y]


--24
partir :: Int -> [Int] -> ([Int], [Int])
partir x l = ([l!!e | e <- [0..x-1]], [l!!e | e <- [x..length l - 1]])


--25
insertar :: [Int] -> Int -> Int -> [Int]
insertar l n p = [l!!e | e <- [0..p-1]] ++ [n] ++ tail[l!!e | e <- [p..length l - 1]]


--26
codifica :: [Int] -> String
codifica l = [posToChar pos | pos <- [0..length l -1]]
    where
        posToChar :: Int -> Char
        posToChar x
            | even x = 'p'
            | otherwise = 'i'


--27
listaPotencias :: [Int] -> [Int]
listaPotencias l = [e^p | e <- l, p <- [length l..0]]


--28
listaPerfectos :: Int -> [Int]
listaPerfectos x = [i | i <- [1..100], esPerfecto i]
    where
        esPerfecto :: Int -> Bool
        esPerfecto x = sum (divisores x) == x


--29
