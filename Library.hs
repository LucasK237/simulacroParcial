module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Requisito = Persona -> Bool

data Criatura = Criatura{
    nombre :: String,
    peligrosidad :: Number,
    requisito :: Requisito
}deriving Show

data Persona = Persona{
    edad :: Number,
    items :: [String],
    experiencia :: Number
}deriving Show

tito = Persona 15 ["soplador de hojas"] 30

cambiarExperiencia :: (Number -> Number) -> Persona -> Persona
cambiarExperiencia funcion persona = persona{experiencia = funcion . experiencia $ persona}

siempreDetras :: Criatura
siempreDetras = Criatura "siempreDetras" 0 (const False)

gnomos :: Number -> Criatura
gnomos cantidad = Criatura "gnomos" (2 ^ cantidad) personaPoseeSoplador

fantasmas :: Number -> Requisito -> Criatura
fantasmas categoria requisito = Criatura "fantasmas" (categoria * 20) requisito

personaPoseeSoplador :: Requisito
personaPoseeSoplador persona = elem "soplador de hojas" . items $ persona

puedeDeshacerse :: Persona -> Criatura -> Bool
puedeDeshacerse persona criatura =  (requisito criatura) persona

enfrentar :: Persona -> Criatura -> Persona
enfrentar persona criatura | puedeDeshacerse persona criatura = cambiarExperiencia (+(peligrosidad criatura)) persona
                           | otherwise = cambiarExperiencia (+1) persona

criaturas :: [Criatura]
criaturas = [siempreDetras, gnomos 10, fantasmas 3 ( (<13) . edad ) ,fantasmas 1 ( (>10) . experiencia ) ]

enfrentarseACriaturas :: Persona -> [Criatura] -> Persona
enfrentarseACriaturas persona grupoDeCriaturas = foldl enfrentar persona grupoDeCriaturas