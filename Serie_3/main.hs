import Text.XHtml (base)
myLength :: [a] -> Integer
myLength [] = 0
myLength (x:ax) = 1 + myLength ax

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:ax) = x + mySum ax

stringConcat :: [String] -> String
stringConcat [] = ""
stringConcat (x:ax) = x ++ stringConcat ax

-- fun3 :: Eq a => (a, b, a) -> (b, a, Bool)
fun3 (a, b, c) = (b, a, a == c)
-- apply :: (a -> b) -> a -> b
apply fun = fun
-- applyList :: (a -> b) -> [a] -> [b]
applyList fun [a] = [fun a]
-- applyString :: (Char -> Char) -> [Char] -> [Char]
applyString fun [x] = [fun x, ',', x]
-- fun4 :: Num a => a -> a -> b -> (a,b)
fun4 a b c = (a+b, c)
-- fun5 :: Num a => p -> [a -> a]
fun5 p = [(+) 0]

f1 x = 1
f2 x = x ^ 2
myd = [-3, -2, -1, 0, 1, 2, 3]

-- Schnittpunkte der Funktion f und g in Definitionsmenge
intersect :: Eq b => (a -> b) -> (a -> b) -> [a] -> [(a, b)]
intersect f1 f2 [] = []
intersect f1 f2 (d:dd) = (if f1 d == f2 d then [(d, f1 d)] else []) ++ intersect f1 f2 dd

higherFunction :: Ord b => (a -> b) -> (a -> b) -> a -> b
higherFunction f1 f2 x = if f1 x > f2 x then f1 x else f2 x
-- higherFunction f1 f2 x = max (f1 x) (f2 x)

addTuple :: (String, Integer) -> (String, Integer) -> (String, Integer)
addTuple (a, b) (c, d) = (a++c, b+d)
nospace :: String -> (String, Integer)
nospace " " = ("", 1)
nospace [] = ("", 0)
nospace (s:ss) = addTuple (if s == ' ' then nospace [s] else ([s], 0)) (nospace ss)

input = [1,-5,3,0,11,3,25,100,7]
a = [i | i <- input, i >= 1 && i <= 7]
b = [i | i <- [1..100], mod i 6 == 0 && mod (i - 2) 4 == 0]
c = [is | is <- [i ^ 2 | i <- input], is > 100 && mod is 2 == 1]
d = [i `elem` a | i <- input]
count [] = 0
count (x:ax) = 1 + count ax
e = count [x | x <- b, mod (x-1) 5 == 0]

containsSet :: [Int] -> [Int] -> Bool
containsSet setA setB = and [x `elem` setA | x <- setB]

filterDigits :: [Int] -> Int -> [Int]
filterDigits list d = [a | a <- list, a /= d]

deleteVowels :: [Char] -> [Char]
deleteVowels word = [c | c <- word, c `notElem` "aeiou"]

myElem :: Eq a => a -> [a] -> Bool
myElem a list = not (and [l == a | l <- list])

countElems :: Eq a => [a] -> [a] -> Int
countElems l1 l2 = count [a | a <- l2, a `elem` l1]
