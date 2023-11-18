largestMatchingElement :: (Enum a) => a -> (a -> Bool) -> a
largestMatchingElement a p = if p a then a else largestMatchingElement (pred a) p

a1 = 20

p1 = (> 10)

a2 = 10

p2 a = (a `mod` 4) == 0

a3 = 54

p3 a = (a + 1) == 52

res1 = largestMatchingElement a1 p1

res2 = largestMatchingElement a2 p2

res3 = largestMatchingElement a3 p3

squareSumA = sum $ takeWhile (< 100000) $ map (^ 2) [2, 4 ..]

squareSumB = sum $ takeWhile (< 100000) [sq | x <- [2, 4 ..], let sq = x ^ 2]

series :: (Enum a1) => (a1 -> a2) -> Int -> a1 -> [a2]
series _ 0 _ = []
series fun n val = map fun $ take n [val ..]

sumIf :: (Num a) => (a -> Bool) -> [a] -> a
sumIf p list = sum $ filter p list

correctAnswers :: (Eq a) => (e -> a) -> [(e, a)] -> Bool
correctAnswers fun = all (\(e, a) -> fun e == a)


loop :: IO ()
loop = do
    answer <- getLine
    putStrLn $ last (words answer) ++ "?"
    getLine
    putStrLn "Warum?"
    loop

chatBot :: IO ()
chatBot = do
    putStrLn "Wie geht es dir?"
    loop
