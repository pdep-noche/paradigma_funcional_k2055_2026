import Text.Show.Functions
import Data.List (genericLength)

data Atraccion = Atraccion {nombre :: String, alturaMin :: Double, duracion :: Int, opiniones :: [String], estaEnMantenimiento :: Bool, reparaciones :: [Reparacion]} deriving Show

data Reparacion = Reparacion {dias :: Int, trabajo :: Trabajo } deriving Show


type Trabajo = Atraccion ->Atraccion
 
vueltaAlMundo :: Atraccion
vueltaAlMundo = Atraccion "vueltaAMundo" 120  4 ["genial"] False []

calificacion :: Atraccion -> Double
calificacion atraccion | (>10).duracion $ atraccion = 100
                       | (<3).length.reparaciones $ atraccion = calcularPuntaje atraccion
                       | otherwise =  (10*).alturaMin $ atraccion

calcularPuntaje :: Atraccion -> Double
calcularPuntaje atraccion = ((10*).genericLength.nombre $ atraccion) + ((2*).genericLength.opiniones $ atraccion)

{-
ghci> calificacion vueltaAlMundo
122.0
-}

ajusteDeTornilleria :: Int -> Trabajo
ajusteDeTornilleria tornillos atraccion =  atraccion {duracion = min (duracion atraccion  + tornillos) 10 }

{-
ghci> ajusteDeTornilleria 4 vueltaAlMundo
Atraccion {nombre = "vueltaAMundo", alturaMin = 120.0, duracion = 8, opiniones = ["genial"], estaEnMantenimiento = False, reparaciones = []}
   -}
engrase :: Double -> Trabajo
engrase grasa atraccion = actualizarAltMin grasa . agregarOpinion "para valientes" $ atraccion

actualizarAltMin :: Double ->Trabajo
actualizarAltMin grasa atraccion = atraccion { alturaMin = alturaMin atraccion  + (0.1 * grasa)}

agregarOpinion :: String -> Trabajo
agregarOpinion opinion atraccion = atraccion { opiniones = opiniones atraccion ++ [opinion]}

{-
ghci> engrase 5 vueltaAlMundo
Atraccion {nombre = "vueltaAMundo", alturaMin = 120.5, duracion = 4, opiniones = ["genial","para valientes"], estaEnMantenimiento = False, reparaciones = []}
-}

mantenimientoElectrico :: Trabajo 
mantenimientoElectrico atraccion = atraccion { opiniones = take 2.opiniones $ atraccion}

{-
ghci> mantenimientoElectrico vueltaAlMundo
Atraccion {nombre = "vueltaAMundo", alturaMin = 120.0, duracion = 4, opiniones = ["genial"], estaEnMantenimiento = False, reparaciones = []}
    
    -}

mantenimientoBasico ::  Trabajo
mantenimientoBasico atraccion = engrase 10. ajusteDeTornilleria 8 $ atraccion

{-
ghci> mantenimientoBasico vueltaAlMundo
Atraccion {nombre = "vueltaAMundo", alturaMin = 121.0, duracion = 10, opiniones = ["genial","para valientes"], estaEnMantenimiento = False, reparaciones = []}
    -}

meDaMiedito :: Atraccion -> Bool
meDaMiedito atraccion = any ((>4).dias) . reparaciones $ atraccion

cerramos :: Atraccion -> Bool
cerramos atraccion = (>7).totalDiasReparacion $ atraccion

totalDiasReparacion :: Atraccion ->Int
totalDiasReparacion atraccion = foldl (\sem reparacion -> sem  + dias reparacion ) 0  .reparaciones $ atraccion


type Parque = [Atraccion]

disneyNoExistis :: Parque -> Bool
disneyNoExistis parque = all (null. reparaciones).filter ((>5).length.nombre) $ parque

{-
ghci> disneyNoExistis [vueltaAlMundo]
True
-}

reparacionesPiolas :: Atraccion -> Bool
reparacionesPiolas atraccion = sonPiolas (reparaciones atraccion) atraccion

sonPiolas :: [Reparacion] -> Atraccion -> Bool
sonPiolas [] _= True
sonPiolas [_]  _ = True
sonPiolas (repa: otraRepa:reparaciones) atraccion = calificacion ((trabajo repa) atraccion ) < calificacion ((trabajo otraRepa) atraccion) && sonPiolas (otraRepa:reparaciones) atraccion   