
module RepasoExamen where


--Ejercicio 1
pertenece:: Eq a => [a] -> a -> Bool
pertenece lista a = foldr(\l x-> if l==a then True||x else False||x) False lista

--Ejercicio 2
eliminar:: Eq a => a -> [a] -> [a]
eliminar a lista = foldr(\l x-> if a==l then x else [l]++x) [] lista

--Ejercicio 3

productoEscalar::Num a =>[a] -> [a] -> a
productoEscalar a b = foldr(\(a1,b1) x-> (a1*b1)+x) 0 (zip a b)


--Hoja 2

--Ejercicio 1

type Marca = String
type Numero = Int
type Deporte = String

data Calzado = Botas Marca Numero | Zapatillas Marca Numero Deporte
bota1::Calzado
bota1 = Botas "sdfgdsdc" 23
bota2::Calzado
bota2 = Botas "sdfggbfvfdsdc" 65
bota3::Calzado
bota3 = Botas "sdaaaaaaaaaaafgdsdc" 43
zapatilla2::Calzado
zapatilla2 = Zapatillas "qwe" 32 "PPPPPP"
zapatilla3::Calzado
zapatilla3 = Zapatillas "qaaaaaaaaawe" 67 "QQQQQQPP"

instance Show Calzado where
    show (Botas m n) = "Botas de la marca: " ++ show m ++ "| Talla: " ++ show n ++"\n"
    show (Zapatillas m n d) = "Zapatillas de la marca: " ++ show m ++ "| Talla: " ++ show n ++ " del deporte: " ++ show d++"\n"
productos :: [Calzado]
productos = [bota1,zapatilla2,bota2,zapatilla2,bota3,zapatilla3]
instance Eq Calzado where
    (Botas m _) == (Botas m' _) = m==m'
    (Zapatillas m _ _) == (Zapatillas m' _ _) = m==m'
    (Zapatillas m _ _) == (Botas m' _) = m==m'
    (Botas m _)==(Zapatillas m' _ _) = m==m'

instance Ord Calzado where
    (Botas m _) <= (Botas m' _) = m<=m'
    (Zapatillas m _ _) <= (Zapatillas m' _ _) = m<=m'
    (Botas m _)<=(Zapatillas m' _ _) = m<=m'
    (Zapatillas m _ _) <=(Botas m' _) = m<=m'


order :: Ord a => [a] -> [a]
order [] = []
order (x:xs) = order menores ++ [x] ++ order mayores where
    menores = [y | y <- xs, y <= x]
    mayores = [y | y <- xs, y > x]


--Ejercicio 2

type Nombre = String
type Major = Int
type Minor = Int
data Version = V Major Minor
data Libreria = L Nombre Version

instance Show Version where
    show (V m n) = show m ++"."++ show n

instance Show Libreria where
    show (L m v) = show m ++ " " ++ show v

instance Eq Libreria where
    --(L n (V a b)) == (L n' (V a' b')) = n==n' && a==a' && b==b'
    (L n v) == (L n' v') = n==n' && v==v'

instance Eq Version where
    (V n m) == (V n' m') = n==n' && m==m'

instance Ord Version where
    --(V n m) <= (V n' m') = n<=n'
    (V n m) <= (V n' m') = if n==n' then m<=m' else n<=n'

instance Ord Libreria where
    --(L n _) <= (L n' _) = n<=n'
    (L n v) <= (L n' v') = if n==n' then v<=v' else n<=n'

docker::Libreria
docker = L "docker" (V 2 3)
docker1::Libreria
docker1 = L "docker" (V 2 2)
docker2::Libreria
docker2 = L "docker" (V 3 5)
mtt::Libreria
mtt = L "mtt" (V 2 5)
mtt2::Libreria
mtt2 = L "mtt" (V 2 2)

librerias::[Libreria]
librerias = [docker,docker1,docker2,mtt,mtt2]

class Compatible a where
    compatible::a->a->Bool
instance Compatible Libreria where
    compatible (L n (V a b)) (L n' (V a' b')) = n==n' && a==a'

devolver:: [Libreria] -> Libreria -> [Libreria]
devolver [] _ = []
devolver (x:xs) a = if compatible x a then [x]++devolver xs a else devolver xs a

--Hoja 3

--Ejercicio 1

data Arbol a = AV | Rama (Arbol a) a (Arbol a)

insertar:: Ord a => a -> Arbol a -> Arbol a
insertar e AV = Rama AV e AV
insertar e (Rama izq h der)
        | e <= h = Rama (insertar e izq) h der
        | e > h = Rama izq h (insertar e der)

--Ejercicio 2

comprobarVocal::Char -> Bool
comprobarVocal a = a=='a' || a=='e' || a=='i' || a=='o' || a=='u'
sinVocales:: String -> (String,String)
sinVocales = foldr(\l (a,b)-> if comprobarVocal l then ([l]++a,b) else (a, [l]++b)) ([],[])

--Ejercicio 3

contabilizar:: [a] -> Int -> [(a,Int)]
contabilizar [] _ = []
contabilizar (x:xs) n = (x,n):contabilizar xs (n+1)

separarPosicion::[a] ->([a],[a])
separarPosicion lista = foldr(\(n,a) (l1,l2)-> if even a then ([n]++l1,l2) else (l1,[n]++l2)) ([],[]) (contabilizar lista 0)


--Examen Junio

--Ejercicio 2

numerosEntre::Ord a =>a -> a -> Arbol a ->[a]
numerosEntre _ _ AV = []
numerosEntre a b (Rama izq h der) = if h>a && h<b then [h] ++ numerosEntre a b izq ++ numerosEntre a b der else numerosEntre a b izq ++ numerosEntre a b der

definiftivo::Eq a =>[a] ->[a]
definiftivo [] = []
definiftivo (x:xs) = x : definiftivo lista where
    lista = filter (/= x) xs

final::(Eq a, Ord a) =>a -> a -> Arbol a ->[a]
final a b arbol = definiftivo (numerosEntre a b arbol)


