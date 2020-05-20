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

-- Record-Definition: zusammengesetzte Daten
data Dillo = Dillo { alive :: Bool, weight :: Integer }
--           ^^^^^ Konstruktor

dillo1 = Dillo { alive = True, weight = 10} -- Gürteltier, lebendig, 10kg
