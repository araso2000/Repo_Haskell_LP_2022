module EjesExamenes where

--1.1
pertenece :: (Eq a) => a -> [a] -> Bool
pertenece n = foldr (\x y -> y || (x==n)) False

--1.2
eliminar :: (Eq a) => a -> [a] -> [a]
eliminar n = foldl (\x y -> if y==n then x else x ++ [y]) []

--1.3
productoEscalar :: [Float] -> [Float] -> Float
productoEscalar xs ys = foldl (+) 0.0 ([x*y | (x,y) <- zip xs ys])

--2.1
type Marca = String
type Numero = Int
type Deporte = String

data Calzado = Bota Marca Numero | Zapatilla Marca Numero Deporte

productos :: [Calzado]
productos = [(Bota "Fosco" 38),(Zapatilla "Diesel" 25 "Futbol")]

instance Show Calzado where
	show (Bota n m) = "Bota de la marca " ++ n ++ " del numero " ++ show(m) ++ "\n"
	show (Zapatilla n m d) = "Zapatilla de la marca " ++ n ++ " del numero " ++ show(m) ++ " para " ++ d ++ "\n"

order :: Ord a => [a] -> [a]
order [] = []
order (x:xs) =
	order menores ++ [x] ++ order mayores
	where
		menores = [y | y <- xs, y<=x]
		mayores = [y | y <- xs, y>x]
		
instance Eq Calzado where
	(Bota m1 _) == (Bota m2 _) = m1 == m2
	(Bota m1 _) == (Zapatilla m2 _ _) = m1 == m2 
	(Zapatilla m1 _ _) == (Bota m2 _ ) = m1 == m2 
	(Zapatilla m1 _ _) == (Zapatilla m2 _ _) = m1 == m2

instance Ord Calzado where
	(Bota m1 _) <= (Bota m2 _) = m1 <= m2
	--(Bota m1 _) <= (Zapatilla m2 _ _) = m1 <= m2 
	--(Zapatilla m1 _ _) <= (Bota m2 _ ) = m1 <= m2 
	(Zapatilla m1 _ _) <= (Zapatilla m2 _ _) = m1 <= m2
	(Bota m1 _) > (Bota m2 _) = m1 > m2
	--(Bota m1 _) > (Zapatilla m2 _ _) = m1 > m2 
	--(Zapatilla m1 _ _) > (Bota m2 _ ) = m1 > m2
	(Zapatilla m1 _ _) > (Zapatilla m2 _ _) = m1 > m2
	
--2.2
type Name = String

data Version = Version {major :: Integer, minor :: Integer}
data Library = Library {name :: String, version :: Version}


instance Show Version where
	show (Version major minor) = show major ++ "." ++ show minor
	
instance Show Library where
	show (Library n v) = n ++ " " ++ show v
	

instance Eq Version where
	v1 == v2 = major v1 == major v2 && minor v1 == minor v2

instance Ord Version where
	v1 < v2 = major v1 < major v2 ||
		major v1 == major v2 && minor v1 < minor v2
	v1 <= v2 = major v1 < major v2 ||
		major v1 == major v2 && minor v1 <= minor v2 
	v1 > v2 = major v1 > major v2 ||
		major v1 == major v2 && minor v1 > minor v2 
	v1 >= v2 = major v1 > major v2 ||
		major v1 == major v2 && minor v1 >= minor v2

instance Eq Library where
	l1 == l2 = name l1 == name l2 && version l1 == version l2

instance Ord Library where
	l1 > l2 = (name l1 > name l2) ||
		(name l1 == name l2 && version l1 > version l2) 
	l1 < l2 = (name l1 < name l2) ||
		(name l1 == name l2 && version l1 < version l2) 
	l1 <= l2 = (name l1 <= name l2) ||
		(name l1 == name l2 && version l1 <= version l2) 
	l1 >= l2 = (name l1 >= name l2) ||
		(name l1 == name l2 && version l1 >= version l2)
		
library1 :: Library
library1 = Library "docker" (Version 2 1)
library2 :: Library
library2 = Library "docker" (Version 2 3)


class Compatible a where
	compatible :: a -> a -> Bool

instance Compatible Library where
	compatible l1 l2 = name l1 == name l2 && major (version l1) == major (version l2)


checkCompatibility :: Library -> [Library] -> [Library]
checkCompatibility l ls = [x | x <- ls, compatible x l]


--3.1
data Arbol a = AV | Rama (Arbol a) a (Arbol a)

insert :: Ord a => a -> Arbol a -> Arbol a
insert e AV = Rama AV e AV
insert e (Rama left root right)
	| e <= root = Rama (insert e left) root right
	| e > root = Rama left root (insert e right)

--3.2
funcion :: String -> (String,String)
funcion l = foldr(\a (b1,b2)-> if esVocal a then (a:b1,b2) else (b1,a:b2)) ([],[]) l

esVocal::Char->Bool
esVocal c= (c=='a')||(c=='e')||(c=='i')||(c=='o')||(c=='u')

--3.3
separar :: [a] -> ([a],[a])
separar (x:xs) = ([xs!!a | a <- [b,b+2..length xs -1]] , [xs!!c | c <- [d,d+2..length xs-1]]) where 
																									b = 0 
																									d = 1






















	