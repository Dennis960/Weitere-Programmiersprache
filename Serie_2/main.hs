import Data.Char

-- Aufgabe 2-1
-- 1.
f1 = (\n -> n > 0, True) -- (Ord a, Num a) => (a -> Bool, Bool)
-- Typ ist ein Tupel mit einer Funktion und einem Bool, die Funktion nimmt einen Parameter der gleichzeitig von Typ Ord und Num ist und gibt einen Bool zurück

-- 2.
f2 = x - 1 where x = 2 -- Num a => a
-- Typ ist Num (2-1 = 1)

-- 3.
f3 y = x - y where x = 2 -- Num a => a -> a
-- Typ ist eine funktion, die Num auf Num abbildet

-- 4.
f4 ls = head ls : tail ls -- [a] -> [a]
-- Type ist eine Funktion, die eine Liste nimmt und dieselbe Liste zurückgibt

-- 5.
f5 = snd (\a b -> a + b, (+)) -- Num a => a -> a -> a
-- Gibt den zweiten Teil des Tupels zurück, also (+)

-- 6.
f6 = [x ^ 2 | x <- reverse [1..10]] -- (Num a, Enum a) => [a]
-- Gibt eine Liste von Werten zurück, die gleichzeitig Num und Enum sind

-- Aufgabe 2-2
str = ['a']
logic a b = a && b
fun0 a = if a == a then a else a
fun1 a b = a + b
fun2 a b = [head a == head b]

-- Aufgabe 2-3
decodeIBANCountryCode :: [Char] -> [Char]
decodeIBANCountryCode (c1:c2:_) = show ((ord c1 - 64 + 9) * 100 + ord c2 - 64 + 9)

getBBAN :: [Char] -> [Char]
getBBAN = drop 4

getRewrittenIBAN :: [Char] -> Integer
getRewrittenIBAN a = read (getBBAN a ++ decodeIBANCountryCode a ++ take 2 (drop 2 a))

verifyRewrittenIBAN :: Integer -> Bool
verifyRewrittenIBAN a = mod a 97 == 1

evaluateIBAN :: [Char] -> Bool
evaluateIBAN a = verifyRewrittenIBAN (getRewrittenIBAN a)