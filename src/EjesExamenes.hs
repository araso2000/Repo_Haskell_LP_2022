module EjesExamenes where

--ABRIL 2021
--EJE 2

data Categoria = ATP1000 | ATP500 | ATP250 | GrandSlam deriving Show
type Nombre = String
data Torneo = Tor Nombre Categoria
data Temporada = Temp [Torneo]

openAustralia :: Torneo
openAustralia = Tor "Open de Australia" GrandSlam
indianWells :: Torneo
indianWells = Tor "Indian Wells" ATP1000
mutuaMadridOpen :: Torneo
mutuaMadridOpen = Tor "Mutua Madrid Open" ATP1000
wimbledon :: Torneo
wimbledon = Tor "Wimbledon" GrandSlam
temporada2013 :: Temporada
temporada2013 = Temp [openAustralia, indianWells, mutuaMadridOpen, wimbledon]


