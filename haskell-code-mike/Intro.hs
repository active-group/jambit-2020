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
  deriving Show

{-
sealed trait Animal
case class Dillo(alive : Liveness, weight : Integer)
  extends Animal
case class Parrot(sentence : String, weight : Integer)
  extends Animal
-}

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
runOverAnimal dillo@(Dillo Dead weight) = dillo -- Alias-Pattern, redundant
runOverAnimal (Dillo liveness w) = Dillo Dead w
-- runOverAnimal (Parrot "Good morning!" weight) =
runOverAnimal (Parrot _ weight) =
  Parrot "" weight

-- Tier füttern
feedAnimal :: Integer -> (Animal -> Animal)
feedAnimal amount (Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

-- Tupel
feedAnimal' :: (Integer, Animal) -> Animal
feedAnimal' (amount, Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)

{-
(define curry
  (lambda (f)
    (lambda (a)
      (lambda (b)
        (f a b)))))

(define uncurry
  (lambda (f)
    (lambda (a b)
      ((f a) b))))
-}

-- a, b, c: Typvariablen - klein geschrieben
curryA :: ((a, b) -> c) -> (a -> (b -> c))
{-
curryA =
  \ f ->
    \ a ->
      \ b ->
        f (a, b)
-}
curryA f a b = f (a, b)

uncurryA :: (a -> (b -> c)) -> ((a, b) -> c)
{-
uncurryA =
  \f ->
    \ (a, b) ->
      f a b
-}
-- uncurryA f (a, b) = f a b
uncurryA f =
  \ (a, b) ->
    f a b

flipA :: (a -> b -> c) -> (b -> a -> c)
flipA f a b = f b a

flipT :: ((a, b) -> c) -> ((b, a) -> c)
-- flipT f (a, b) = f (b, a)
-- flipT f = uncurryA (flipA (curryA f))
flipT = uncurryA . flipA . curryA

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

-- Idris / Agda: Typen zuerst, Definition wird generiert

data IntList =
    EmptyIntList
  | ConsIntList Integer IntList

list0 :: [a]
list0 = [] -- leere Liste
list1 :: [Integer]
list1 = 1:[] -- : = cons
list1' :: [Integer]
list1' = [1] -- gleiche Liste
list2 :: [Integer]
list2 = [1, 2]

listSum :: [Integer] -> Integer
listSum [] = 0
-- listSum (first:rest) = first + (listSum rest)
-- first -> head, rest -> tail
listSum (x:xs) = x + listSum xs

cons :: a -> [a] -> [a]
cons x xs = x : xs

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) =
  (f x) : (listMap f xs)


