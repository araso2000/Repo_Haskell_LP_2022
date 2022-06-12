module ResumenTema5 where


type Nombre = String
type Edad = Integer
type Persona = (Nombre, Edad)

tocayos:: Persona -> Persona -> Bool
tocayos (n,_) (nombre,_) = n == nombre

ModuleName> tocayos ("Pepe",35) ("Alejandro",23)
False


data Temperatura = Frio | Caliente
data Estacion = Primavera | Verano | Otonyo | Invierno

tiempo :: Estacion -> Temperatura
tiempo Primavera = Caliente tiempo Verano = Caliente
tiempo _ = Frio


data LetraOEntero = Letra Char | Entero Integer

type Nombre = String
type Edad = Integer
data Persona = Pers Nombre Edad deriving Show
juan :: Persona
juan = Pers "Juan Lopez" 23



esJoven :: Persona ->Bool
esJoven (Pers _ edad) = edad < 25

ModuleName> esJoven (Pers "Jaime" 50)
False

verPersona :: Persona -> String
verPersona (Pers nombre edad) = "Persona, nombre" ++ nombre ++ ", edad: " ++ show edad

ModuleName> verPersona (Pers "Jaime" 50)
"Persona, nombreJaime, edad: 50"


1.     Como sinónimo de tipos:
type Persona = (Nombre, Edad)
2.     Como tipo de datos:
data Persona = Pers Nombre Edad
3.     Mediante campos con nombre:
data Persona = Pers{nombre::Nombre, edad::Edad}



type Radio = Float
type Lado = Float
data Forma = Circulo Radio
			| Rectangulo Lado Lado
area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Rectangulo base altura) = base * altura


data Color = Rojo | Amarillo | Azul | Verde deriving (Eq, Ord, Show)

instance Eq Color where
Rojo == Rojo = True
Amarillo == Amarillo = True
Azul == Azul = True
Verde == Verde = True
_ == _ = False



