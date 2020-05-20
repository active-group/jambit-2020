module Intro where

x :: Integer
x = 5 + 7

data State =
  Solid | Liquid | Gas
  deriving (Show, Eq) -- generiert Typklassen-Instanzen für Show, Eq
