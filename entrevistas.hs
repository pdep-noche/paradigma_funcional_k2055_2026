data Postulante = UnPostulante {nombre :: String, edad :: Double, remuneracion :: Double, conocimientos :: [String]} |
                 Estudiante {legajo:: String, conocimientos :: [String]} deriving Show
pepe = UnPostulante "Jose Perez" 35 15000.0 ["Haskell", "Prolog", "Wollok", "C"]
tito = UnPostulante "Roberto Gonzalez" 20 12000.0 ["Haskell", "Php"]

type Nombre = String
type Conocimiento = String
type Edad = Double
data Puesto = UnPuesto {puesto:: String, conocimientoRequeridos :: [String]} deriving Show
jefe = UnPuesto "gerente de sistemas" ["Haskell", "Prolog", "Wollok"]
chePibe = UnPuesto "cadete" ["ir al banco"]
 
apellidoDueno:: Nombre
apellidoDueno = "Gonzalez"

-------- a --------------
tieneConocimientos  :: Puesto -> Requisito
tieneConocimientos puesto postulante  = all (tieneConcimiento postulante) . conocimientoRequeridos $ puesto

tieneConcimiento :: Postulante -> Conocimiento -> Bool
tieneConcimiento persona conocimiento = elem conocimiento . conocimientos $ persona


{--
ghci> tieneConocimientos jefe pepe
True
--}
--------- b------------
type Requisito =  Postulante -> Bool
edadAceptable :: Edad -> Edad -> Requisito
edadAceptable edadMin edadMax postulante = (edad postulante > edadMin) && (edad postulante < edadMax)

sinArreglo :: Requisito
sinArreglo postulante = not (terminaCon (nombre postulante) apellidoDueno)

terminaCon :: Nombre -> Nombre -> Bool
terminaCon nombre apellido = (== apellido).last.words $ nombre  

{--
ghci> sinArreglo tito
False
--}

--2---
 
preseleccion :: [Postulante]->[Requisito] -> [Postulante]
preseleccion postulantes requisitos = filter ( cumpleTodos requisitos ) postulantes

cumpleTodos :: [Requisito] -> Postulante -> Bool
cumpleTodos requisitos postulante = all (\requisito -> requisito postulante) requisitos

{--
ghci> preseleccion [tito, pepe] [edadAceptable 30 40, tieneConocimientos jefe, sinArreglo]
[UnPostulante {nombre = "Jose Perez", edad = 35.0, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
-}

{--

ghci> preseleccion [tito, pepe] [edadAceptable 30 40, tieneConocimientos jefe, sinArreglo, (\(UnPostulante _ _ _ conocimientos) -> not. elem "repetir]
[UnPostulante {nombre = "Jose Perez", edad = 35.0, remuneracion = 15000.0, conocimientos = ["Haskell","Prolog","Wollok","C"]}]
--}

{-- a Usando listas por comprension

actualizarPostulantes  :: [Postulante] -> [Postulante]
actualizarPostulantes postulantes = [ incrementarEdad. aumentarSueldo 27 $ postulante | postulante <- postulantes]

--}

{-- b usando composicion y aplicacion parcial

actualizarPostulantes'  :: [Postulante] -> [Postulante]
actualizarPostulantes' postulantes = map (incrementarEdad. (aumentarSueldo 27)) postulantes

--}

{--

actualizarPostulantes . repeat $ pepe


--}
juan = Estudiante "545343543" ["C"]
capacitar :: Postulante -> Conocimiento -> Postulante
capacitar (UnPostulante nom edad remu conocimientos) conocimiento = UnPostulante nom edad remu (conocimiento:conocimientos)
capacitar (Estudiante legajo conocimientos) conocimiento = Estudiante legajo (conocimiento: (init conocimientos))

capacitacion :: Puesto -> Postulante -> Postulante
capacitacion puesto persona = foldl capacitar persona . conocimientoRequeridos $ puesto

{--
ghci> capacitacion jefe pepe
UnPostulante {nombre = "Jose Perez", edad = 35.0, remuneracion = 15000.0, conocimientos = ["Wollok","Prolog","Haskell","Haskell","Prolog","Wollok","C"]}
--}

{--
ghci> capacitacion jefe juan
Estudiante {legajo = "545343543", conocimientos = ["Wollok"]}
--}