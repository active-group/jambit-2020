module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- "DSL für Datenbank-Programmierung"

-- put "Mike" 15
-- x = get "Mike"
-- put "Mike" (x + 1)
-- Ergebnis: x

-- 1. Schritt: Datentyp

{-
data DBCommand =
    Put String Integer
  | Get String

put = Put
get = Get

type DBProgram = [DBCommand]

p1 = let x = get "Mike
     in [put "Mike" 15,
         get "Mike",
         put "Mike" (x + 1)] -- nicht gebunden
-}

-- Beschreibung einer Datenbankinteraktion

data DB result =
     Get String (Integer -> DB result) -- Callback, Continuation
   | Put String Integer (() -> DB result)
   | Done result

{-
data Maybe a =
    Nothing | Just a 
-}

p1 :: DB (Maybe Integer)
p1 = Put "Mike" 15 (\() ->
     Get "Mike" (\ x ->
     Put "Mike" (x + 1) (\() ->
     if x > 15
     then Done (Just x)
     else Done Nothing)))

runDB :: Map String Integer -> DB result -> result