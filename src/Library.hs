module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Punto 1

data Pais = UnPais {
    ipc :: Number,
    sectorPublico :: Number,
    sectorPrivado :: Number,
    recursos :: [String],
    deuda :: Number
} deriving(Eq, Show)

namibia :: Pais
namibia = UnPais 4140 400 650 ["mineria", "ecoturismo"] 50

-- Punto 2

type Estrategia = Pais -> Pais

porcentaje :: Number -> Number -> Number
porcentaje porciento n = div (n * porciento) 100

alterarDeuda :: Number -> Pais -> Pais
alterarDeuda n pais = pais {deuda = (deuda pais) + n}

alterarSectorPublico :: Number -> Pais -> Pais
alterarSectorPublico n pais = pais {sectorPublico = (sectorPublico pais) + n}

alterarIPC :: Number -> Pais -> Pais
alterarIPC n pais = pais {ipc = (ipc pais) + n}

eliminarRecurso :: String -> Pais -> Pais
eliminarRecurso recurso pais = pais {recursos = filter (/= recurso) (recursos pais)}

pbi :: Pais -> Number
pbi pais = ((* ((sectorPublico pais) + (sectorPrivado pais))).ipc) pais

prestarDolares :: Number -> Estrategia
prestarDolares prestado = alterarDeuda (porcentaje 150 prestado)

reducirIPCsegunPuestos :: Number -> Pais -> Pais
reducirIPCsegunPuestos n pais
    | n > 100 = (alterarIPC (-(porcentaje 20 (ipc pais)))) pais
    | otherwise = (alterarIPC (-(porcentaje 15 (ipc pais)))) pais

reducirSectorPublico :: Number -> Estrategia
reducirSectorPublico puestosDeTrabajo = (reducirIPCsegunPuestos puestosDeTrabajo).(alterarSectorPublico (-puestosDeTrabajo))

explotarRecurso :: String -> Estrategia
explotarRecurso recurso = (eliminarRecurso recurso).(alterarDeuda (-2))

blindaje :: Estrategia
blindaje pais = ((reducirSectorPublico 500).(prestarDolares ((pbi pais)/2))) pais

-- Punto 3

type Receta = [Estrategia]

nuevaReceta :: Receta
nuevaReceta = [(prestarDolares 200), (explotarRecurso "mineria")]

aplicarEstrategia :: Estrategia -> Pais -> Pais
aplicarEstrategia estrategia = estrategia

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr aplicarEstrategia pais receta

-- Punto 4

tieneRecurso :: String -> Pais -> Bool
tieneRecurso recurso = (elem recurso).recursos

quienesZafan :: [Pais] -> [Pais]
quienesZafan = filter (tieneRecurso "petroleo")

deudaTotal :: [Pais] -> Number
deudaTotal = sum.(map deuda)

-- Punto 5

deltaPBI :: Receta -> Receta -> Pais -> Number
deltaPBI receta1 receta2 pais = (pbi.(aplicarReceta receta2 pais)) - (pbi.(aplicarReceta receta1 pais))

dePeorAMejor :: [Receta] -> Pais -> Bool
dePeorAMejor [_] _ = True
dePeorAMejor (receta1:receta2) pais = (deltaPBI receta1 receta2 pais) >= 0
dePeorAMejor (receta1:receta2:resto) pais = dePeorAMejor (receta1:receta2) && dePeorAMejor (receta2:resto)