module Library where
import PdePreludat

data Ley = UnaLey {
    tema :: String,
    presupuesto :: Number,
    partidosImpulsores :: [String]
} deriving (Show,Eq)

---- Leyes de Ejemplo ----

leyMedicinalCannabis = UnaLey {tema = "Cannabis", presupuesto = 5, partidosImpulsores = ["Cambio de Todos","Sector Financiero"]}
leyEducacionSuperior = UnaLey {tema = "Educacion Superior", presupuesto = 30, partidosImpulsores = ["Docentes Universitarios","Partido de Centro Federal"]}
leyProfesionalizacionTenisMesa = UnaLey {tema = "Tenis de Mesa", presupuesto = 1, partidosImpulsores = ["Partido de Centro Federal","Liga de Deportistas Autónomos","Club Paleta Veloz"]}
leyTenis = UnaLey {tema = "Tenis", presupuesto = 2, partidosImpulsores = ["Liga de Deportistas Autónomos"]}

---- Funciones Auxiliares ----

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

estaDentroDe :: Eq a => [a] -> [a] -> Bool
estaDentroDe [] _ = True
estaDentroDe _ [] = False
estaDentroDe a b | a == take (length a) b = True
                 | otherwise = estaDentroDe a (tail b)

---- Punto 1 -- Las Leyes ----

impulsanteEnComun :: Ley -> String -> Bool
impulsanteEnComun ley1 impulsante = elem impulsante (partidosImpulsores ley1)

tienenImpulsanteEnComun :: Ley -> Ley -> Bool
tienenImpulsanteEnComun ley1 ley2 = any (impulsanteEnComun ley1) (partidosImpulsores ley2) 

algunTemaEstaContenido :: Ley -> Ley -> Bool
algunTemaEstaContenido ley1 ley2 = (estaDentroDe (tema ley1) (tema ley2)) || (estaDentroDe (tema ley2) (tema ley1))

esCompatible :: Ley -> Ley -> Bool
esCompatible ley1 ley2 = (tienenImpulsanteEnComun ley1 ley2) && (algunTemaEstaContenido ley1 ley2)

--------------------------------------------------- Constitucionalidad de las Leyes ---------------------------------------------------

type Juez = Ley -> Bool

---- 5 Jueces de la Corte Suprema ----

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

---- Corte Suprema conformada por todos los Jueces ----

corteSuprema :: [Juez]
corteSuprema = [juezOpinionPublica,juezSectorFinanciero,juezArcasEstado,juezArcasEstado',juezPartidoConservador]

leyesEjemplo :: [Ley]
leyesEjemplo = [leyMedicinalCannabis,leyEducacionSuperior,leyProfesionalizacionTenisMesa,leyTenis]

---- Punto 1 ----

constitucionalidad :: [Juez] -> Ley -> Bool
constitucionalidad jueces ley = (length . filter (== True)) (map (aplicarJuez ley) jueces) >= (div (length jueces) 2)

aplicarJuez :: Ley -> Juez -> Bool
aplicarJuez ley juez = juez ley 

---- Punto 2 ----

juezAfirmativo :: Juez
juezAfirmativo ley = True

juezInventadoVago :: Juez
juezInventadoVago ley = between 0 20 (length (tema ley)) 

juezPresupuestario :: Juez
juezPresupuestario ley = between 0 30 (presupuesto ley)

añadirjueces :: [Juez] -> [Juez] -> [Juez]
añadirjueces corte nuevosJueces = corte ++ nuevosJueces

---- Punto 3 ----

leyesAprobadas :: [Juez] -> [Ley] -> [Ley]
leyesAprobadas corte ley = filter ((==True).constitucionalidad corte) ley

leyesNoAprobadas :: [Juez] -> [Ley] -> [Ley]
leyesNoAprobadas corte ley = filter ((==False).constitucionalidad corte) ley

constitucionalesConNuevosJueces :: [Juez] -> [Ley] -> [Juez] -> [Ley]
constitucionalesConNuevosJueces jueces leyes corte = leyesAprobadas (añadirjueces jueces corte) (leyesNoAprobadas corte leyes)

------------------------------------------------------- Cuestión de Principios -------------------------------------------------------

---- Punto 1 ----

borocotizar :: [Ley -> Bool] -> [Ley -> Bool]
borocotizar = map (not . )

seCumpleCondicion :: Ley -> [Juez] -> Bool
seCumpleCondicion unaLey corte = ((constitucionalidad corte unaLey) /= (constitucionalidad (borocotizar corte) unaLey))

---- Punto 2 ----

coincideConPosicionSocial :: Juez -> [Ley] -> [String] -> Bool
coincideConPosicionSocial juez leyesEnTratamiento sector = all ((== sector) . partidosImpulsores) (votadasAFavor juez leyesEnTratamiento)

votadasAFavor :: Juez -> [Ley] -> [Ley]
votadasAFavor juez leyes = filter ((==True) . juez) leyes

---- Para Pensar ----

{-
Si una ley que tiene presupuesto 5 y se trata de un tema de agenda va a ser aprobada por juez1, juez3, juez4 por lo tanto va a ser 
        aprobada por toda la corteSuprema. Esto sucede porque la funcion de aprobar una ley, no evalua los impulsantes si
                                        no es necesario (para cada juez) (evaluación diferida)
-}