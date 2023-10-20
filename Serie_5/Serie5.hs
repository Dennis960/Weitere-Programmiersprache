module Serie5 where
    -- monoton bedeutet, dass jede Differenz aus jedem aufeinanderfolgenden Zahlenpaar das gleiche Vorzeichen hat oder Null ist
    -- entweder steigt die Folge oder bleibt gleich
    -- oder die Folge sinkt oder bleibt gleich
    -- (Es wird davon ausgegangen, dass Monotonie gemeint ist und nicht strenge Monotonie,
    -- im Fall von strenger Monotonie müsste nur das "=" aus "<=" und ">=" in "isAscending" und "isDescending" entfernt werden)
    monotoneFolge = [1,1,2,3,3,4,5,6,10,22]
    isMonotonous :: Ord a => [a] -> Bool
    isMonotonous [] = True
    isMonotonous [_] = True
    isMonotonous list = isAscending list || isDescending list where
        isAscending [a,b] = a <= b
        isAscending (a:list@(b:_)) = isAscending [a,b] && isAscending list
        isDescending [a,b] = a >= b
        isDescending (a:list@(b:_)) = isDescending [a,b] && isDescending list

    alternierendeFolge = [1,5,1,5,1,5,1,5]
    isAlternating :: (Ord a, Num a) => [a] -> Bool
    isAlternating [] = True
    isAlternating [a] = True
    isAlternating [a,b] = True
    isAlternating [a,_b,c] = a == c
    isAlternating (a:list@(b:c:_)) = isAlternating [a,b,c] && isAlternating list

    konstanteFolge = [5,5,5,5,5,5,5,5]
    isConstant :: Eq a => [a] -> Bool
    isConstant [] = True
    isConstant [a] = True
    isConstant [a,b] = a == b
    isConstant (a:list@(b:_)) = isConstant [a,b] && isConstant list
