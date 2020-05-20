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

