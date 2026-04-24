sayHello :: String -> String
sayHello alguien = " Hello " ++ alguien ++ "!"

find' :: (a -> Bool) -> [a] -> a
find' funcion lista = (head . filter funcion) lista 


data Politico = Politico {proyectosPresentados :: [String], sueldo :: Integer,  edad :: Int } deriving Show 

politicos :: [Politico]
politicos = [ Politico ["ser libres", "libre estacionamiento coches politicos", "ley no fumar", "ley 19182"] 20000 81, Politico ["tratar de reconquistar luchas sociales"] 10000 63, Politico ["tolerancia 100 para delitos"] 15500 49 ]

{-
a)
ghci> find' ((<50).edad)  politicos 
Politico {proyectosPresentados = ["tolerancia 100 para delitos"], sueldo = 15500, edad = 49}
-}

{- b
ghci> find' ((>3).length.proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edad = 81}
-}

{- c
ghci> find' (any ((>3).length.words).proyectosPresentados) politicos
Politico {proyectosPresentados = ["ser libres","libre estacionamiento coches politicos","ley no fumar","ley 19182"], sueldo = 20000, edad = 81}
-}


type Nombre = String
type Notas = [Int]
data Persona = Alumno {nombre :: Nombre, notas :: Notas}
type Promedio = Int

juan :: Persona
juan = Alumno "juan" [8,6]

maria :: Persona
maria = Alumno "maria" [7,9,4]
ana :: Persona
ana = Alumno "ana" [6,2,4]
alumnos :: [Persona]
alumnos = [juan, ana, maria]


promediosAlumnos :: [Persona]-> [(Nombre, Promedio )]
promediosAlumnos alumnos = map (\(Alumno nombre notas) -> (nombre, promedio notas)) alumnos

promedio :: Notas -> Promedio
promedio notas = (sum notas) `div` (length notas)

{-
ghci> promediosAlumnos alumnos
[("juan",7),("ana",4),("maria",6)]
-}

promediosSinAplazo :: [Notas]-> [Promedio]
promediosSinAplazo notas = map (promedio.filter(>=6)) notas

{-
ghci> promediosSinAplazo [[8,6],[7, 9, 5], [4,7,8]]  
[7,8,7]
-}

aprobo :: Persona -> Bool
aprobo alumno = (all (>= 6).notas) alumno

aprobaron :: [Persona] -> [Nombre]
aprobaron alumnos = (map nombre . filter aprobo) alumnos

{-
ghci> aprobaron [juan, ana, maria]
["juan"]
-}


productos :: [String]-> [Int] -> [(String, Int)]
productos nombres precios = zip nombres precios

productos' :: [String] -> [Int] -> [(String, Int)]
productos' nombres precios = zipWith (\nom precio -> (nom, precio)) nombres precios

{-
ghci> productos' ["melon", "sandia"] [50, 40, 20]
[("melon",50),("sandia",40)]
-}