import Distribution.Simple.Program.HcPkg (list)
fun2 :: (Num a, Enum a) => a -> [a -> (a, a, a)]
fun2 n = [\a -> (a, b, a * b) | b <- [1 .. n]]

this :: [Char]
this = ['a', 'b'] ++ "cd"

fun3 :: [a] -> a
fun3 (x : xs) = x

fun4 :: (Num a) => [a] -> a
fun4 (x : xs) = x + 1

fun5 :: (Num a) => [a] -> a -> a
fun5 (x : xs) = (+) x

fun6 :: (Num a) => [[a]] -> a -> a
fun6 (x : xs) = fun5 x

fun7 :: (Num a) => [a] -> a -> a
fun7 (x : xs) = fun5 xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] list = list
merge list [] = list
merge list1 list2 =
  let (smaller, l1, l2) =
        if head list1 < head list2
          then (head list1, drop 1 list1, list2)
          else (head list2, list1, drop 1 list2)
   in smaller : merge l1 l2

half :: [a] -> ([a], [a])
half list = splitAt (length list `div` 2) list
-- half list = (take (length list `div` 2) list, drop (length list `div` 2) list)

msort :: (Ord a, Num a) => [a] -> [a]
msort [] = []
msort [a] = [a]
msort list = merge (msort l1) (msort l2) where (l1, l2) = half list

qsort :: (Ord a, Num a) => [a] -> [a]
qsort [] = []
qsort (p:list) = sm ++ [p] ++ lg where
    sm = qsort [x | x <- list, x <= p]
    lg = qsort [x | x <- list, x > p]

myTake :: Integer -> [a] -> [a]
myTake n list
  | n <= 0 = []
  | null list = []
  | otherwise = head list : myTake (n - 1) (tail list)

myDrop :: Integer -> [a] -> [a]
myDrop _ [] = []
myDrop 0 list = list
myDrop n (_:list) = myDrop (n - 1) list

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p list = [x | x <- list, p x]

moreOften :: Eq a => a -> a -> [a] -> Bool
moreOften x y xs = count x xs > count y xs where
    count :: (Num b, Eq a, Eq b) => a -> [a] -> b
    count el [] = 0
    count el (l:list) = (if el == l then 1 else 0) + count el list

sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (l:list) = let newList = [x | x <- list, x `mod` l /= 0] in l : sieve newList

primesUntil :: Integral a => a -> [a]
primesUntil n = sieve [2..n]

isPrime :: Integral a => a -> Bool
isPrime n = or [True | p <- primesUntil n, n == p]