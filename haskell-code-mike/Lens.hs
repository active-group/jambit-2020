module Lens where

data Dillo =
    Dillo { dilloAlive :: Bool, dilloWeight :: Integer}

-- dilloAlive :: Dillo -> Bool

updateDilloAlive :: Bool -> Dillo -> Dillo
updateDilloAlive newAlive (Dillo _ weight) =
    Dillo newAlive weight

data Lens container attribute =
    Lens {
        lensGetter :: container -> attribute,
        lensUpdater :: attribute -> container -> container
    }

alive = Lens dilloAlive updateDilloAlive

get :: Lens container attribute -> container -> attribute
get (Lens getter _) container = getter container




--- get alive dillo1