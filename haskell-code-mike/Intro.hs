module Intro where

x :: Integer
x = 5 + 7

-- Ein Aggregatzustand ist eins der folgenden:
-- - fest
-- - flüssig
-- - gasförmig

-- data: *neuer* Typ
data State =
  Solid | Liquid | Gas
  deriving (Show, Eq) -- generiert Typklassen-Instanzen für Show, Eq

state1 :: State
state1 = Solid
state2 :: State
state2 = Liquid
state3 :: State
state3 = Gas

-- Aggregatzustand von Wasser
waterState :: Double -> State

waterState = \ temp ->
  if temp < 0
  then Solid
  else if temp <= 100
  then Liquid
  else Gas

-- Ein Haustier ...
data Pet = Hund | Katze | Schlange
 deriving Show -- einrückungssensibel

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

isCute Hund = True
isCute Katze = True
isCute Schlange = False

-- Tiere auf dem texanischen Highway

data Liveness = Dead | Alive
  deriving Show

{-
-- Record-Definition: zusammengesetzte Daten
data Dillo = Dillo { alive :: Liveness, weight :: Integer }
--           ^^^^^ Konstruktor
  deriving Show

dillo1 :: Dillo
dillo1 = Dillo { alive = Alive, weight = 10} -- Gürteltier, lebendig, 10kg
dillo2 :: Dillo
dillo2 = Dillo Dead 12 -- Gürteltier, tot, 12 kg

data Parrot = Parrot String Integer -- Satz, Gewicht

-}
-- Geht nicht:
-- data Animal = Dillo | Parrot

-- algebraischer Datentyp:
-- gemischte Daten aus zusammengesetzte Daten
data Animal =
  -- Selektoren sind global
    Dillo { dilloAlive :: Liveness, dilloWeight :: Integer }
  | Parrot String Integer -- Satz, Gewicht

dillo1 :: Animal
dillo1 = Dillo { dilloAlive = Alive, dilloWeight = 10}
dillo2 :: Animal
dillo2 = Dillo Dead 12
parrot1 :: Animal
parrot1 = Parrot "Hello!" 1
parrot2 :: Animal
parrot2 = Parrot "Goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo liveness w) = Dillo Dead w
runOverAnimal (Parrot "Good morning!" weight) =
  Parrot "" weight
