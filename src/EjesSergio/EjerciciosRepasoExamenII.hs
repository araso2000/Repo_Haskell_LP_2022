module EjerciciosRepasoExamenII where
import Data.Fixed (mod')

type Marca = String 
type Numero = Int
type Deporte = String

data Calzado = Bota Marca Numero | Zapatillas Marca Numero Deporte

bota1::Calzado
bota1 = Bota "Timberland" 40

bota2::Calzado
bota2 = Bota "Ogg" 42

bota3::Calzado
bota3 = Bota "Columbia" 45

zapatilla2::Calzado
zapatilla2 = Zapatillas "Kike" 39 "Furgol"

zapatilla3::Calzado
zapatilla3 = Zapatillas "Adidas" 39 "Tenis(RAFA, TENEIS QUE SER COMO RAFA EN EL TENIS!!!!! VAMOS RAFA)"

productos :: [Calzado]
productos = [bota1,zapatilla2,bota2,zapatilla2,bota3,zapatilla3]

instance Show Calzado where
    show (Bota m n) = "Bota de la marca " ++ show m ++ " del numero " ++ show n ++", \n"
    show (Zapatillas m n d) = "Zapatilla de marca " ++ show m ++ " del numeros " ++ show n ++ " para el deporte :" ++ show d ++", \n"

instance Eq Calzado where
    Bota m _ == Bota m' _ = m==m'
    Zapatillas m _ _ == Zapatillas m' _ _ = m==m'
    Bota m _ == Zapatillas m' _ _ = m==m'
    Zapatillas m' _ _ == Bota m _ = m==m'

instance Ord Calzado where
    Bota m _ <= Bota m' _ = m <= m'
    Bota m _ <= Zapatillas m' _ _ = m <= m'
    Zapatillas m _ _ <= Zapatillas m' _ _ = m <= m'
    Zapatillas m _ _ <= Bota m' _ = m <= m'
    Bota m _ > Bota m' _ = m > m'
    Bota m _ > Zapatillas m' _ _ = m > m'
    Zapatillas m _ _ > Zapatillas m' _ _ = m > m'
    Zapatillas m _ _ > Bota m' _ = m > m'

order :: Ord a => [a] -> [a]
order [] = []
order (x:xs) = order menores ++ [x] ++ order mayores where 
    menores = [y | y <- xs, y <= x] 
    mayores= [y | y<-xs,y> x]


--Ejercicio 2

type Nombre = String 
data VersionN = V Major Minor
data Libreria = L Nombre VersionN
type Major = Int
type Minor = Int

libreria::Libreria
libreria = L "docker" (V 3 1)

libreria2::Libreria
libreria2 = L "docker" (V 3 3)

libreria3::Libreria
libreria3 = L "docker" (V 4 3)

libreria4::Libreria
libreria4 = L "jdk" (V 2 3)

instance Eq Libreria where
   L n _ == L m' _ = n == m'

instance Ord Libreria where
    L n _ <= L n' _ = n<=n'
    L n p <= L n' p' = p<=p'


