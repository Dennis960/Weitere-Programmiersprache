module Serie4 where
    listDoubles :: String -> [String]
    listDoubles scentence = let list = words scentence in [list!!index | index <- [0..(length list - 2)], list!!index == list!!(index+1)]

    testDoubles :: String -> Bool
    testDoubles scentence = not (null (listDoubles scentence))