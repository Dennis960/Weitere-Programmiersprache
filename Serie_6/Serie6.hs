module Serie6 where

data Person = Person
  { name :: String,
    adresse :: String
  }
  deriving (Show, Eq)

data Auto = Auto {bezeichnung :: String, preis :: Int, alter :: Int, besitzer :: Person} deriving (Show)

printAuto :: Auto -> String
printAuto Auto {bezeichnung, preis, alter, besitzer = Person {name, adresse}} =
  "Das Auto mit der Bezeichnung "
    ++ bezeichnung
    ++ " gehört "
    ++ name
    ++ ". "
    ++ name
    ++ " hat den Wohnort "
    ++ adresse
    ++ ". Das Auto ist "
    ++ show alter
    ++ " Jahre alt und kostet "
    ++ show preis
    ++ " Euro."

collectionWorth :: Person -> [Auto] -> Int
collectionWorth person autos = sum [preis | Auto {preis, besitzer} <- autos, person == besitzer]

oldTimers :: [Auto] -> [Auto]
oldTimers autos = [auto | auto@(Auto {alter}) <- autos, alter >= 30]