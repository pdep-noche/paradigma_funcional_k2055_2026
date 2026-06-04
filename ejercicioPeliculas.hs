

import Text.Show.Functions

data Pelicula = Pelicula { nombrePelicula :: String, genero :: String , duracion :: Int, origen :: String  } deriving (Show, Eq)

data Usuario = Usuario {nombre :: String, categoria :: String, edad :: Int, paisDeResidencia :: String, peliculasVistas :: [Pelicula], nivelDeSalud :: Int  } deriving Show

psicosis = Pelicula "Psicosis" "Terror" 109 "Estados Unidos"
perfumeDeMujer= Pelicula "Perfume de Mujer" "Drama" 150  "Estados Unidos"
elSaborDeLasCervezas = Pelicula "El sabor de las cervezas"  "Drama" 95 "Iran"
lasTortugasTambienVuelan = Pelicula "Las tortugas también vuelan" "Drama" 103 "Iran"


juan = Usuario "juan" "estandar" 23  "Argentina" [perfumeDeMujer] 60

-- 1

ver :: Pelicula -> Usuario -> Usuario
ver pelicula usuario = usuario {peliculasVistas =  peliculasVistas usuario ++ [pelicula]}

{-
ghci> ver psicosis juan
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisDeResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"},Pelicula {nombrePelicula = "Psicosis", genero = "Terror", duracion = 109, origen = "Estados Unidos"}], nivelDeSalud = 60}
-}

premiarInterFieles :: [Usuario] -> [Usuario]
premiarInterFieles usuarios = map premiarUsuarioSiCorresponde usuarios

premiarUsuarioSiCorresponde :: Usuario -> Usuario
premiarUsuarioSiCorresponde usuario | cumpleCondiciones usuario = subirCategoria usuario
                                    | otherwise = usuario

cumpleCondiciones :: Usuario -> Bool
cumpleCondiciones usuario = (> 20). length. peliculasQueNoSean "Estados Unidos" . peliculasVistas $ usuario

peliculasQueNoSean :: String -> [Pelicula] -> [Pelicula]
peliculasQueNoSean pais peliculas = filter ((pais /=).origen) peliculas

subirCategoria :: Usuario -> Usuario
subirCategoria usuario = usuario { categoria = nuevaCategoria. categoria $ usuario }

nuevaCategoria :: String -> String
nuevaCategoria "basica" = "estandar"
nuevaCategoria _ = "premium"

{-
ghci> premiarInterFieles [juan]  
[Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisDeResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"}], nivelDeSalud = 60}]
-}

type Criterio = Pelicula -> Bool

teQuedasteCorto :: Criterio
teQuedasteCorto pelicula = (<35).duracion $ pelicula

cuestionDeGenero :: [String] -> Criterio
cuestionDeGenero generos pelicula = any  (==(genero pelicula))  generos

deDondeSaliste :: String -> Criterio
deDondeSaliste unOrigen pelicula = (== unOrigen). origen $ pelicula

vaPorEseLado :: (Eq a) => Pelicula -> (Pelicula  -> a  ) -> Criterio
vaPorEseLado pelicula caracteristica otraPelicula = caracteristica pelicula  == caracteristica otraPelicula


{-
ghci> vaPorEseLado psicosis origen perfumeDeMujer
True
-}

--- 5
buscar :: Usuario -> [Criterio] -> [Pelicula] -> [Pelicula]
buscar usuario criterios peliculas = take 3. filter (cumpleCriterios usuario criterios) $ peliculas


cumpleCriterios:: Usuario -> [Criterio] ->  Pelicula -> Bool
cumpleCriterios usuario criterios pelicula = (not . vioPelicula pelicula) usuario && cumpleTodos pelicula criterios

cumpleTodos :: Pelicula -> [Criterio] -> Bool
cumpleTodos pelicula criterios = all ( $ pelicula) criterios


vioPelicula :: Pelicula -> Usuario -> Bool
vioPelicula pelicula usuario = elem pelicula. peliculasVistas $ usuario


data Capitulo = Capitulo {nombreCapitulo :: String, generoCapitulo :: String , duracionCapitulo :: Int, origenCapitulo :: String, afecta :: Usuario -> Usuario} deriving Show


--- 2
consumeSerie :: Usuario -> Capitulo -> Usuario
consumeSerie usuario capitulo =  (afecta capitulo) usuario

unCapitulo :: Capitulo
unCapitulo = Capitulo " capitulo 10" "comedia" 30 "Argentina"  (\usuario -> usuario { nivelDeSalud = (nivelDeSalud usuario) - 20})

{--
ghci> consumeSerie juan unCapitulo
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisDeResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"}], nivelDeSalud = 40}
--}

maraton :: Usuario -> [Capitulo] -> Usuario
maraton usuario serie = foldl  consumeSerie usuario serie


serieInfinita :: [Capitulo]
serieInfinita = repeat unCapitulo

{--
ghci> maraton juan (take 5 serieInfinita)
Usuario {nombre = "juan", categoria = "estandar", edad = 23, paisDeResidencia = "Argentina", peliculasVistas = [Pelicula {nombrePelicula = "Perfume de Mujer", genero = "Drama", duracion = 150, origen = "Estados Unidos"}], nivelDeSalud = -40}
--}