module Serie2 where
    type QuaderEdge = Double
    type Quader = (QuaderEdge, QuaderEdge, QuaderEdge)
    quader :: QuaderEdge -> QuaderEdge -> QuaderEdge -> Quader
    quader a b c = (a, b, c)

    volume :: Quader -> QuaderEdge
    volume (a, b, c) = a * b * c

    surfaceArea :: Quader -> QuaderEdge
    surfaceArea (a, b, c) = 2 * (a * b + a * c + b * c)

    isCube :: Quader -> Bool
    isCube (a, b, c) = a == b && b == c

    diagonal :: Quader -> QuaderEdge
    diagonal (a, b, c) = sqrt (a ** 2 + b ** 2 + c ** 2)

    isBigger :: Quader -> Quader -> Bool
    isBigger q1 q2 = volume q1 > volume q2