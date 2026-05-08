
data Flor = Flor{nombreFlor :: String, aplicacion:: String, cantidadDeDemanda:: Integer} deriving Show


estaOrdenadaPorDemanda :: [Flor] -> Bool
estaOrdenadaPorDemanda [] = True
estaOrdenadaPorDemanda [_] = True
estaOrdenadaPorDemanda (x:y:xs) = (cantidadDeDemanda x >= cantidadDeDemanda y) && estaOrdenadaPorDemanda (y:xs)

rosa = Flor "rosa" "decorativo" 120
jazmin =  Flor "jazmin" "aromatizante" 100
violeta=  Flor "violeta" "infusión" 110
orquidea =  Flor "orquidea" "decorativo" 90

flores = [orquidea, rosa,violeta, jazmin]


cantidadDeElementos lista = foldl(\sem _ -> sem + 1) 0 lista

cantidadDeElementos' lista = foldr (\_ sem ->sem + 1) 0 lista

masGastador :: [(String, Integer)] -> (String, Integer)
masGastador (cab:cola) = foldl gastoMayor cab   cola

gastoMayor ::  (String, Integer) -> (String , Integer) -> (String, Integer)
gastoMayor primerPar segundoPar | snd primerPar >= snd segundoPar = primerPar
                                | otherwise = segundoPar 

  {-
  ghci> masGastador [("ana", 40000), ("pedro", 500000), ("lucas", 340333)]
("pedro",500000)
-}                              

--- 4
monto :: [(String, Integer )] -> Integer
monto empleados = foldl (\sem (_, gasto)-> sem + gasto ) 0 empleados

{-ghci> monto [("ana", 500000), ("julio", 600000)] 
1100000
-}

--foldl (\sem fun -> fun sem) 2 [(3+), (*2), (5+)]

-- foldl (flip ($)) 2 [(3+), (*2), (5+)]

-- foldr (\fun sem -> fun sem) 2  [(3+), (*2), (5+)]

--foldr ($) 2  [(3+), (*2), (5+)]  

type Nombre  = String
type InversionInicial = Int
type Profesionales = [String]


data  Proyecto = Proy {nombre:: Nombre, inversionInicial::  InversionInicial, profesionales:: Profesionales} deriving Show

proyectos = [Proy "red social de arte"  200000 ["ing. en sistemas", "contador"], Proy "restaurante" 50000 ["cocinero", "adm. de empresas", "contador"], Proy "ventaChurros" 10000 ["cocinero"] ]


maximoProyectoSegun ::  ( Proyecto -> Int   )  ->[Proyecto] -> Proyecto
maximoProyectoSegun f (proyecto:proyectos) = foldl (maximoSegun f) proyecto  proyectos   

maximoSegun :: (Proyecto -> Int) -> Proyecto -> Proyecto  -> Proyecto
maximoSegun f unProyecto otroProyecto | f unProyecto >= f otroProyecto = unProyecto
                                      | otherwise = otroProyecto


{- a
ghci> maximoProyectoSegun inversionInicial proyectos
Proy {nombre = "red social de arte", inversionInicial = 200000, profesionales = ["ing. en sistemas","contador"]}
-}

{-b
ghci> maximoProyectoSegun (length.profesionales) proyectos
Proy {nombre = "restaurante", inversionInicial = 50000, profesionales = ["cocinero","adm. de empresas","contador"]}
-}

{- c
ghci> maximoProyectoSegun (length.words.nombre) proyectos 
Proy {nombre = "red social de arte", inversionInicial = 200000, profesionales = ["ing. en sistemas","contador"]}
-}