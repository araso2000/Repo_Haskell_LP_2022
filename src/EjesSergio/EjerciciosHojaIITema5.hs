module EjerciciosHojaIITema5 where

--Ejercicio 7

type Jugador = String
type Nombre = String
type Puntos = Int
data Resultado = P Jugador [Puntos] Jugador [Puntos]
data Torneo = Tor Nombre Resultado
data Temporada = Temp [Torneo]

rafa::Jugador
rafa = "Rafa Nadal"
djokovic::Jugador
djokovic = "djokovic"
murray::Jugador
murray = "Andy Murray"
delPotro::Jugador
delPotro = " J M Del Potro"

aus::Torneo
aus = Tor "Open de Australia" (P djokovic [6,7,6,6] murray [7,6,3,2])

indian::Torneo
indian = Tor "Indian Wells" (P delPotro [6,3,4] rafa [4,6,6])

mmo::Torneo
mmo = Tor "Mutua Mdrid Open" (P rafa [6,6] murray [2,4])

wim::Torneo
wim = Tor "Wimbeldon" (P djokovic [6,7,6] murray [4,5,4])

temporada2013::Temporada
temporada2013 = Temp [aus,indian,mmo,wim]

getNombre::Torneo -> String
getNombre (Tor nom _) = nom

compararResultado::Resultado -> Jugador
compararResultado (P j1 p1 j2 p2) = if sum(p1)>sum(p2) then j1 else j2

getGanador::Torneo -> Jugador
getGanador (Tor nom res) = compararResultado res

comprobarSets::Torneo -> Int
comprobarSets (Tor _ (P _ p1 _ p2)) = length p1

instance Show Torneo where
    show torneo = getNombre (torneo) ++ ", Ganador: " ++  getGanador torneo ++ " en: " ++ show (comprobarSets torneo) ++ " Sets." ++ "\n"

mostrarListadoOrdenadoTorneos::Temporada -> String
mostrarListadoOrdenadoTorneos (Temp lista)= foldr(\l t-> show l ++ t) "" lista





