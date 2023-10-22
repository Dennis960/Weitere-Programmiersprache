module Serie5 where
    -- Es wird davon ausgegangen, dass Monotonie gemeint ist und nicht strenge Monotonie,
    -- im Fall von strenger Monotonie müsste nur das "=" aus "<=" und ">=" in "isAscending" und "isDescending" entfernt werden
    monotoneFolge = [1,1,2,3,3,4,5,6,10,22]
    isMonotonous :: Ord a => [a] -> Bool
    isMonotonous [] = True
    isMonotonous [_] = True
    isMonotonous list = isAscending list || isDescending list where
        isAscending [] = True
        isAscending [_] = True
        isAscending (x:xs) = x <= head xs && isAscending xs
        isDescending [] = True
        isDescending [_] = True
        isDescending (x:xs) = x >= head xs && isDescending xs

    -- Es wird davon ausgegangen, dass die Zahl Null kein Vorzeichen besitzt und somit nicht alternierend sein kann
    alternierendeFolge = [-1,5,-2,20,-11,13,-11,13]
    isAlternating :: (Ord a, Num a) => [a] -> Bool
    isAlternating [] = True
    isAlternating [x] = x /= 0
    isAlternating (x:xs) = x * head xs < 0 && isAlternating xs

    konstanteFolge = [5,5,5,5,5,5,5,5]
    isConstant :: Eq a => [a] -> Bool
    isConstant [] = True
    isConstant [a] = True
    isConstant (x:xs) = x == head xs && isConstant xs
