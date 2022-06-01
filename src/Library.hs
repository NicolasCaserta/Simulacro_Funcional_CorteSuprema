module Library where
import PdePreludat

data Ley = UnaLey {
    tema :: String,
    presupuesto :: Number,
    partidosImpulsores :: [String]
} deriving (Show,Eq)

-- Leyes de Ejemplo --

leyMedicinalCannabis = UnaLey {tema = "Cannabis", presupuesto = 5, partidosImpulsores = ["Cambio de Todos","Sector Financiero"]}
leyEducacionSuperior = UnaLey {tema = "Educacion Superior", presupuesto = 30, partidosImpulsores = ["Docentes Universitarios","Partido de Centro Federal"]}
leyProfesionalizacionTenistaMesa = UnaLey {tema = "Profesionalizacion", presupuesto = 1, partidosImpulsores = ["Partido de Centro Federal","Liga de Deportistas Autónomos","Club Paleta Veloz"]}
leyTenis = UnaLey {tema = "Tenis", presupuesto = 2, partidosImpulsores = ["Liga de Deportistas Autónomos"]}

-- Funciones Auxiliares --

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

------------------------------------- Constitucionalidad de las Leyes -------------------------------------

type Juez = Ley -> Bool

-- 5 Jueces de la Corte Suprema --

juezOpinionPublica :: Juez
juezOpinionPublica ley = elem (tema ley) temasEnAgenda
temasEnAgenda = []

juezSectorFinanciero :: Juez
juezSectorFinanciero ley = elem ("Sector Financiero") (partidosImpulsores ley)

juezArcasEstado :: Juez
juezArcasEstado ley = between 0 10 (presupuesto ley)

juezArcasEstado' :: Juez
juezArcasEstado' ley = between 0 20 (presupuesto ley)

juezPartidoConservador :: Juez
juezPartidoConservador ley = all (/= "Partido Conservador") (partidosImpulsores ley)

-- Corte Suprema conformada por todos los Jueces --

corteSuprema :: [Juez]
corteSuprema = [juezOpinionPublica, juezSectorFinanciero, juezArcasEstado, juezArcasEstado', juezPartidoConservador]

-- Punto 1 --

constitucionalidad :: [Juez] -> Ley -> Bool
constitucionalidad jueces ley = (length . filter (== True)) (map (aplicarJuez ley) jueces) >= (div (length jueces) 2)

aplicarJuez :: Ley -> Juez -> Bool
aplicarJuez ley juez = juez ley 

-- Punto 2 --

juezAfirmativo :: Juez
juezAfirmativo ley = True

juezInventadoVago :: Juez
juezInventadoVago ley = between 0 20 (length (tema ley)) 

juesPresupuestario :: Juez
juesPresupuestario ley = between 0 30 (presupuesto ley)

añadirjueces :: [Juez] -> [Juez] -> [Juez]
añadirjueces corte nuevosJueces = corte ++ nuevosJueces

-- Punto 3 --

leyesAprobadas :: [Juez] -> [Ley] -> [Ley]
leyesAprobadas corte ley = filter ((==True).constitucionalidad corte) ley

leyesNoAprobadas :: [Juez] -> [Ley] -> [Ley]
leyesNoAprobadas corte ley = filter ((==False).constitucionalidad corte) ley

constitucionalesConNuevosJueces :: [Juez] -> [Ley] -> [Juez] -> [Ley]
constitucionalesConNuevosJueces jueces leyes corte = leyesAprobadas (añadirjueces jueces corte) (leyesNoAprobadas corte leyes)

----------------------------------------- Cuestión de principios ------------------------------------------

