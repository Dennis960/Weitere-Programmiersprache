module Serie4 where
    listDoubles :: String -> [String]
    listDoubles sentence = let list = words sentence in [list!!index | index <- [0..(length list - 2)], list!!index == list!!(index+1)]

    testDoubles :: String -> Bool
    testDoubles sentence = not (null (listDoubles sentence))