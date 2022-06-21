module Ejes where



--Ejes 4 5 y 6

type Day = Int

type Month = Int

type Year = Int

data Date = D Day Month Year

data News = RedSocial Date String | Periodico Date String String

data NewsCollection = N [News]


NewsCollection = N [(RedSocial (21 6 2022) "TextoNoticia1RRSS"),(RedSocial (20 6 2022) "TextoNoticia2RRSS"),(Periodico (18 6 2022) "TitularNoticia1Periodico" "TextoNoticia1Periodico")]


--(RedSocial (21 6 2022) "TextoNoticia1RRSS")
--(RedSocial (20 6 2022) "TextoNoticia2RRSS")
--(Periodico (18 6 2022) "TitularNoticia1Periodico" "TextoNoticia1Periodico")




newsCollection :: Show NewsCollection => NewsCollection



instance Show Date where

            show (D d m y) = show d ++ "/" ++ show m ++ "/" ++ show y

instance Show News where

           show (SocialNet c d) = "Contenido: " ++ c ++ " para publicar en red social en la fecha "++ show d

           show (NewsPaper h c d) = "Titular: "++h ++" y contenido: "++ c ++" para publicar en periodico en la fecha "++ show d

instance Show NewsCollection where

          show (NC []) = ""

          show (NC (x:xs)) = show x ++ "\n" ++ show (NC xs)
          
          
          
          
          

inserta :: Ord a => a -> [a] -> [a]
inserta (_ (fecha) _ _) =  

instance Eq Date where

    (D d1 m1 y1) == (D d2 m2 y2) = (d1 == d2) && (m1 == m2) && (y1 == y2)

instance Ord Date where

    compare (D d1 m1 y1) (D d2 m2 y2) = if ((compare y1 y2) == EQ) then  (if ((compare m1 m2) == EQ) then compare d1 d2 else compare m1 m2)   

                                                                           else compare y1 y2

          
          