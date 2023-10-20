import Data.Char

nospace :: String -> (String, Integer)
nospace s@(x:xs) | s == "" = ("", 0)
                 | s == " " = ("", 1)
                 | length s == 1 = (s, 0)
                 | otherwise = let (res, num) = nospace xs in let (resX, numX) = nospace [x] in (resX ++ res, numX + num)

deleteVowels :: [Char] -> [Char]
deleteVowels word@(x:xs) | null word = ""
                         | x `elem` "aeiouAEIOU" = deleteVowels xs
                         | otherwise = x:deleteVowels xs

countElems :: Eq a => [a] -> [a] -> Int
countElems [] _ = 0
countElems [needle] haystack = length [occ | occ <- haystack, occ == needle]
countElems (needle:needles) haystack = countElems [needle] haystack + countElems needles haystack

multipleElems :: [Integer] -> [Integer]
multipleElems [] = []
multipleElems (x:xs) = ([x | x <- xs, x `notElem` xs]) ++ multipleElems xs

descending :: [Integer] -> Bool
descending [] = True
descending [a] = True
descending [a,b] = a > b
descending (x1:xs@(x2:_)) = descending [x1, x2] && descending xs

-- without importing Data.Char
myToLowerCase :: [Char] -> [Char]
myToLowerCase ['A'] = ['a']
myToLowerCase ['B'] = ['b']
myToLowerCase ['C'] = ['c']
myToLowerCase ['D'] = ['d']
myToLowerCase ['E'] = ['e']
myToLowerCase ['F'] = ['f']
myToLowerCase ['G'] = ['g']
myToLowerCase ['H'] = ['h']
myToLowerCase ['I'] = ['i']
myToLowerCase ['J'] = ['j']
myToLowerCase ['K'] = ['k']
myToLowerCase ['L'] = ['l']
myToLowerCase ['M'] = ['m']
myToLowerCase ['N'] = ['n']
myToLowerCase ['O'] = ['o']
myToLowerCase ['P'] = ['p']
myToLowerCase ['Q'] = ['q']
myToLowerCase ['R'] = ['r']
myToLowerCase ['S'] = ['s']
myToLowerCase ['T'] = ['t']
myToLowerCase ['U'] = ['u']
myToLowerCase ['V'] = ['v']
myToLowerCase ['W'] = ['w']
myToLowerCase ['X'] = ['x']
myToLowerCase ['Y'] = ['y']
myToLowerCase ['Z'] = ['z']
myToLowerCase [any_other] = [any_other]
myToLowerCase (letter:word) = myToLowerCase [letter] ++ myToLowerCase word

-- with importing Data.Char
myToLowerCase2 :: [Char] -> [Char]
myToLowerCase2 [char] | isUpper char = [chr (ord 'a' + (ord char - ord 'A'))]
                      | otherwise = [char]
myToLowerCase2 (char:chars) = myToLowerCase2 [char] ++ myToLowerCase2 chars

smallestSum :: [Integer] -> Integer
smallestSum [] = 0
smallestSum [a] = a
smallestSum [a,b] = a + b
smallestSum [a,b,c] = a + b + c
smallestSum (a:list@(b:c:_)) = let firstSum = smallestSum [a,b,c] in let secondSum = smallestSum list in min firstSum secondSum