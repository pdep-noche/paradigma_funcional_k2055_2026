type Bien = (String,Float)
data Ciudadano = UnCiudadano {profesion :: String, sueldo :: Float, cantidadDeHijos :: Float, bienes :: [Bien] } deriving Show

homero = UnCiudadano "SeguridadNuclear" 9000 3 [("casa",50000), ("deuda",-70000)]
frink = UnCiudadano "Profesor" 12000 1 []
krabappel = UnCiudadano "Profesor" 12000 0 [("casa",35000)]
burns = UnCiudadano "Empresario" 300000 1 [("empresa",1000000),("empresa",500000),("auto",200000)]

type Ciudad = [Ciudadano]
springfield :: Ciudad
springfield = [homero, burns, frink, krabappel]

diferenciaDePatrimonio :: Ciudad -> Float
diferenciaDePatrimonio ciudad = (patrimonio. ciudadanoSegun maximoPatrimonio ) ciudad - (patrimonio.ciudadanoSegun minimoPatrimonio) ciudad

patrimonio ::  Ciudadano -> Float
patrimonio (UnCiudadano _ sueldo _ bienes) = foldl (\sem (_, valor)-> sem + valor) sueldo bienes

ciudadanoSegun fun (unCiudadano: ciudadanos) = foldl fun unCiudadano ciudadanos

maximoPatrimonio :: Ciudadano -> Ciudadano -> Ciudadano
maximoPatrimonio unCiu otroCiu | patrimonio unCiu > patrimonio otroCiu = unCiu
                               | otherwise = otroCiu


minimoPatrimonio :: Ciudadano -> Ciudadano -> Ciudadano
minimoPatrimonio unCiu otroCiu | patrimonio unCiu < patrimonio otroCiu = unCiu
                               | otherwise = otroCiu

{--
ghci> diferenciaDePatrimonio springfield
2011000.0
--}