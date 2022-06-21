module Eje1 where

vocal :: Char -> Bool
vocal a = if (a == 'a' || a == 'e' || a == 'i' || a == 'o' || a == 'u' || a == 'A' || a == 'E' || a == 'I' || a == 'O' || a == 'U') then True else False

consonante :: Char -> Bool 
consonante a = if vocal a

extraerVocal :: String -> String
extraerVocal pal = [a | a <- pal, vocal a]

extraerConsonante :: String -> String
extraerConsonante pal = [a | a <- pal, consonante a]

separar :: [Char] -> String
separar = foldr (+) extraerVocal

--separar :: String -> (String,String)
--separar (p:ps) = (foldr (extraerVocal),foldr (extraerConsonante))
