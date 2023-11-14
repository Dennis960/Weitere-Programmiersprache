data Wochentag = Montag | Dienstag | Mittwoch | Donnerstag | Freitag | Samstag | Sonntag deriving (Show, Ord, Enum)

instance Eq Wochentag where
  (==) :: Wochentag -> Wochentag -> Bool
  (==) tagA tagB = ((tagA < Samstag) && (tagB < Samstag)) || ((tagA > Freitag) && (tagB > Freitag))

successor :: Wochentag -> Wochentag
successor Sonntag = Montag
successor wochentag = succ wochentag

predecessor :: Wochentag -> Wochentag
predecessor Montag = Sonntag
predecessor wochentag = pred wochentag

determineDay :: Wochentag -> Integer -> Wochentag
determineDay wochentag n
  | n < 0 = determineDay (predecessor wochentag) (n + 1)
  | n > 0 = determineDay (successor wochentag) (n - 1)
  | otherwise = wochentag

data MultiTree a = Node a [MultiTree a] deriving (Show)

testTree :: MultiTree Int
testTree = Node 1 [Node 2 [Node 4 []], Node 3 [], Node 8 [Node 5 [Node 6 [], Node 7 []]]]

allePfade :: MultiTree a -> [[a]]
allePfade (Node v []) = [[v]]
allePfade (Node v trees) = concat [map (v :) (allePfade tree) | tree <- trees]

kantenListe = [(1, 2), (2, 4), (1, 3), (1, 8), (8, 5), (5, 6), (5, 7)]

kantenListeZuWurzel :: (Eq a) => [(a, a)] -> a
kantenListeZuWurzel list = fst $ head $ filter (\(a, b) -> not (any (\(c, d) -> a == d) list)) list

kantenListeZuMultiTree :: (Eq a) => [(a, a)] -> MultiTree a
kantenListeZuMultiTree ((a, _) : xs) = Node a [] -- TODO I don't want to do that

myAny :: (a -> Bool) -> [a] -> Either a String
myAny pr list = let f = filter pr list in if null f then Right "Kein passsender Wert" else Left (head f)

replaceEventWithNthOdd :: (Integral a) => [a] -> [a]
replaceEventWithNthOdd list = replaceEvenWithNthOddIndexed list 0
  where
    replaceEvenWithNthOddIndexed [] _ = []
    replaceEvenWithNthOddIndexed (x : xs) n = (if odd x then x else 2 * n + 1) : replaceEvenWithNthOddIndexed xs (if odd x then n else n + 1)