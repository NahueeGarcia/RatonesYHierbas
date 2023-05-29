module Lib where
--import PdePreludat
import Data.List (take,drop)


type Enfermedad = String

data Raton = UnRaton{
    nombre :: String,
    edad :: Number,
    peso :: Number,
    enfermedades :: [Enfermedad]
}deriving Show

cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis","sarampion","tuberculosis"]
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []
huesudo = UnRaton "Huesudo" 4 10 ["obesidad","sinusitis"]

type Potencia = Number
type Tipo = String
type Hierba = (Raton -> Tipo -> Raton)

hierbaBuena :: Raton -> Tipo -> Raton
hierbaBuena raton _ = raton{edad = sqrt (edad raton) }

terminaConElTipo :: Tipo -> Enfermedad -> Bool
terminaConElTipo tipo enfermedad = tipo == reverse (take (length tipo) (reverse enfermedad) )


eliminarEnfermedades :: (t -> a -> Bool) -> t -> [a] -> [a]
eliminarEnfermedades funcion x enfermedades = filter (not.(funcion x)) enfermedades

hierbaVerde :: Raton -> Tipo -> Raton
hierbaVerde raton tipo = raton{enfermedades = eliminarEnfermedades terminaConElTipo tipo (enfermedades raton)}

alcachofa :: Raton -> Tipo -> Raton
alcachofa raton _
    |peso raton > 2 = raton{peso = (peso raton - (peso raton * 0.1))}
    |otherwise = raton{peso = (peso raton - (peso raton * 0.05))}

hierbaZort :: Raton -> Tipo -> Raton
hierbaZort raton _ = raton{edad = 0, enfermedades = []}

cantLetrasMenorA :: Number -> Enfermedad -> Bool
cantLetrasMenorA n enfermedad = (length enfermedad) < n

hierbaDelDiablo :: Raton -> Tipo -> Raton
hierbaDelDiablo raton _ 
    |peso raton > 0.1 = raton{enfermedades = eliminarEnfermedades cantLetrasMenorA 10 (enfermedades raton), peso = (peso raton) - 0.1}
    |otherwise = raton{enfermedades = eliminarEnfermedades cantLetrasMenorA 10 (enfermedades raton)}

------------------------------------------------------------------------------------------------------------------------------------------

type Medicamento = (Raton -> [Hierba] -> Raton)

sufijosInfecciosas :: [Tipo]
sufijosInfecciosas = ["sis","itis","emia","cocos"]

comerHierba :: Raton -> Hierba -> Raton
comerHierba raton hierba = hierba raton "obesidad"

pondsAntiAge :: Raton -> Raton
pondsAntiAge raton = foldl comerHierba raton [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

nHierbas :: Number -> Hierba -> [Hierba] -> [Hierba]
nHierbas pot hierba lst
    |pot == 0 = lst
    |otherwise = nHierbas (pot-1) hierba (lst ++ [hierba])

reduceFatFast :: Raton -> Potencia -> Raton
reduceFatFast raton potencia = foldl comerHierba raton ([hierbaVerde] ++ (nHierbas potencia alcachofa []))

eliminarEnfermedadesInfecciosas :: Raton -> [Tipo] -> Raton
eliminarEnfermedadesInfecciosas raton [] = raton
eliminarEnfermedadesInfecciosas raton (x:xs) = eliminarEnfermedadesInfecciosas ( raton{enfermedades = eliminarEnfermedades terminaConElTipo x (enfermedades raton) } ) xs

pdepCilina :: Raton -> Raton
pdepCilina raton = eliminarEnfermedadesInfecciosas raton sufijosInfecciosas

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

cantidadIdeal :: (Number -> Bool) -> Number
cantidadIdeal condicion = head (filter condicion [1..])

seEstabilizaron :: Raton -> Bool
seEstabilizaron raton = (peso raton) <= 1 && (length (enfermedades raton)) < 3

lograEstabilizar :: [Raton] -> Medicamento -> Bool
lograEstabilizar ratones medicamento = all seEstabilizaron (map medicamento ratones)

--No anda
experimento :: [Raton] -> Number
experimento ratones = cantidadIdeal (\potencia -> lograEstabilizar ratones (reduceFatFast potencia))
