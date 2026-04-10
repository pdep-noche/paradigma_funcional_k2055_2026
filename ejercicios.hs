import Distribution.Simple.Setup (trueArg)
siguiente :: Integer -> Integer
siguiente nro = nro + 1

calcular :: Integer -> Integer 
calcular nro | even nro = siguiente nro
             | otherwise = doble nro

doble nro = nro * 2

calcular' :: (Integer, Integer) -> (Integer , Integer)
calcular' (nro, otroNum) | even nro && odd otroNum = (doble nro, siguiente otroNum)
                         | even nro && even otroNum = (doble nro, otroNum)
                         | odd nro && odd otroNum = (nro, siguiente otroNum)
                         | otherwise = (nro, otroNum)    

calcular'' :: (Integer, Integer)  -> (Integer, Integer)
calcular'' (nro, otroNum) = (duplicaPar nro, sumarUnoImpar otroNum)

duplicaPar :: Integer -> Integer
duplicaPar nro  | even nro = doble nro
                | otherwise = nro

sumarUnoImpar :: Integer -> Integer
sumarUnoImpar nro | odd nro = siguiente nro
                  | otherwise  = nro      

-- II Declaratividad
and' :: Bool -> Bool -> Bool 
and' unaCond otraCond | unaCond  = otraCond
                      | otherwise = False

-- III Declaratividad
and'' :: Bool -> Bool -> Bool
and'' unaCond otraCond | not unaCond = False
                       | not otraCond = False
                       | otherwise = True

--I Declaratividad
-- Aprovechando Pattern Matching
and''' :: Bool ->Bool -> Bool
and''' True otraCond = otraCond
and''' _ _ = False


or' :: Bool -> Bool -> Bool 
or' False otraCond = otraCond
or' _ _ = True

or'' :: Bool -> Bool -> Bool
or'' True _ = True
or'' _ True = True
or'' _ _ = False

or''' :: Bool -> Bool -> Bool
or''' False False  = False
or''' _ _ = True


type Nota = Integer
type Alumno = (String, Nota, Nota, Nota)

notaMaxima :: Alumno -> Nota
notaMaxima (_, nota1, nota2, nota3) = nota1 `max` (nota2 `max` nota3)

cuadruple :: Integer -> Integer
cuadruple nro = doble (doble nro)

esMayorA :: Integer -> Bool
esMayorA nro = doble (siguiente (nro + 2)) > 10

--(\nro -> nro * 3)

--(\nro -> nro + 1)

--(\nro otroNum -> nro + otroNum) 

--(\nro -> nro + 2)