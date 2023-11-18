module Serie8 where
groupByResult :: Eq b => (a -> b) -> [a] -> [[a]]
groupByResult fun [] = []
groupByResult fun (x:xs) = (x : filter (\x2 -> fun x == fun x2) xs) : groupByResult fun (filter (\x2 -> fun x /= fun x2) xs)

unwordsWith :: Char -> [String] -> String
unwordsWith _ [] = ""
unwordsWith _ [a] = a
unwordsWith sep (x:xs) = foldl (\a b -> a ++ sep : b) x xs
