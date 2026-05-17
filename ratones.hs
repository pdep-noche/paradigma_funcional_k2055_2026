data Animal= Raton {nombre :: String, edad :: Double, peso :: Double, enfermedades :: [String]} deriving Show

-- Ejemplo de raton
cerebro = Raton "Cerebro" 9.0 0.2 ["C", "sarampión", "tuberculosis"]

-- Estos son las enfermedades infecciosas
enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]


modificarEdad :: (Double -> Double) ->Animal -> Animal
modificarEdad f animal = animal { edad =(f.edad)animal  }

{-ghci> modificarEdad (2 *) cerebro 
Raton {nombre = "Cerebro", edad = 18.0, peso = 0.2, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}
-}



modificarNombre :: (String -> String) -> Animal -> Animal
modificarNombre f animal = animal { nombre  = (f.nombre)animal}

{-
ghci> modificarNombre ((++) "Genio") cerebro 
Raton {nombre = "GenioCerebro", edad = 9.0, peso = 0.2, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}

-}

modificarPeso :: (Double -> Double) ->Animal -> Animal
modificarPeso f animal = animal { peso = (f.peso) animal}

{-
ghci> modificarPeso (2+) cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 2.2, enfermedades = ["brucelosis","sarampi\243n","tuberculosis"]}

-}
modificarEnfermedades :: ([String] -> [String]) -> Animal -> Animal
modificarEnfermedades f animal = animal { enfermedades = (f.enfermedades) animal }

{-
ghci> modificarEnfermedades ( ++ ["Tos"]) cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.2, enfermedades = ["brucelosis","sarampi\243n","tuberculosis","Tos"]}
-}


hierbaBuena :: Animal -> Animal
hierbaBuena animal = modificarEdad sqrt animal

hierbaVerde :: String -> Animal -> Animal
hierbaVerde enfermedad animal = modificarEnfermedades (filter (/= enfermedad)) animal


{-}
ghci> hierbaVerde "tuberculosis"  cerebro                                          
Rton {nombre = "Cerebro", edad = 9.0, peso = 0.2, enfermedades = ["brucelosis","sarampi\243n"]}
ghci>
-}

alcachofa :: Animal -> Animal
alcachofa animal = modificarPeso pierdeSegun  animal

pierdeSegun :: Double -> Double
pierdeSegun unPeso | unPeso > 2  = unPeso *  0.9   
                    | otherwise =  unPeso * 0.95 


{-
ghci> alcachofa cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.19, enfermedades = ["C","sarampi\243n","tuberculosis"]}
-}

hierbaMagica :: Animal -> Animal
hierbaMagica animal = (modificarEnfermedades (const []). modificarEdad (0*) ) animal

{-
ghci> hierbaMagica cerebro
Raton {nombre = "Cerebro", edad = 0.0, peso = 0.2, enfermedades = []}
    -}


medicamento :: [(Animal ->Animal)] ->Animal -> Animal
medicamento hierbas animal = foldl (flip ($))  animal  hierbas

{-
ghci> medicamento [alcachofa, hierbaBuena, hierbaVerde "tuberculosis"] cerebro
Raton {nombre = "Cerebro", edad = 3.0, peso = 0.19, enfermedades = ["C","sarampi\243n"]}
-}


antiAge :: Animal -> Animal
antiAge animal = medicamento (replicate 3 hierbaBuena ++ [alcachofa]) animal

{-
ghci> antiAge cerebro
Raton {nombre = "Cerebro", edad = 1.3160740129524924, peso = 0.19, enfermedades = ["C","sarampi\243n","tuberculosis"]}
-}


reduceFatFast :: Int -> Animal -> Animal
reduceFatFast potencia animal = medicamento ([hierbaVerde "obesidad"] ++ replicate potencia alcachofa) animal


{-
ghci> reduceFatFast 5 cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.15475618749999998, enfermedades = ["C","sarampi\243n","tuberculosis"]}
-}

hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa animal = medicamento (map hierbaVerde enfermedadesInfecciosas ) animal

{-
ghci> hierbaMilagrosa cerebro
Raton {nombre = "Cerebro", edad = 9.0, peso = 0.2, enfermedades = ["C","sarampi\243n"]}
-}


cantidadIdeal f = head . filter f $ [1..] 

estanMejoresQueNunca :: [Animal] -> (Animal -> Animal) -> Bool 
estanMejoresQueNunca animales medicamento = all ((<1).peso.medicamento) animales

juan = Raton "juan" 2 5 ["obesidad"]

{-
ghci> estanMejoresQueNunca [juan, cerebro] antiAge
False
-}

potenciaIdeal :: [Animal] -> Int
potenciaIdeal ratones = cantidadIdeal (estanMejoresQueNunca ratones. reduceFatFast)

{-
ghci> potenciaIdeal [juan, cerebro] 
22
-}
