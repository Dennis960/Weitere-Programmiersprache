type QuaderEdge = Double
type Quader = (QuaderEdge, QuaderEdge, QuaderEdge)
quader :: QuaderEdge -> QuaderEdge -> QuaderEdge -> Quader
quader a b c = (a, b, c)

volume :: Quader -> QuaderEdge
volume q = let (a, b, c) = q in a * b * c

surfaceArea :: Quader -> QuaderEdge
surfaceArea q = let (a, b, c) = q in 2 * a * b + 2 * a * c + 2 * b * c

isCube :: Quader -> Bool
isCube q = let (a, b, c) = q in a == b && b == c

diagonal :: Quader -> QuaderEdge
diagonal q = let (a, b, c) = q in sqrt (a ** 2 + b ** 2 + c ** 2)

isBigger :: Quader -> Quader -> Bool
isBigger q1 q2 = volume q1 > volume q2