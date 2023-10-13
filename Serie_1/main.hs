-- :t 1 -- 1 :: Num p => p
-- :t Bool -- <interactive>:1:1: error: Data constructor not in scope: Bool
-- :t (+) -- (+) :: Num a => a -> a -> a
-- :t gibt den Typ einer oder eines Ausdrucks zurück

f x = x + 1

-- starten mit ghci 1-1-3.hs
-- oder
-- starten mit ghci, dann :l 1-1-3.hs zum Laden
-- Dann f 1 eingeben, Rückgabewert ist 2

add1 a b c = a + b + c
add2 (a, b, c) = a + b + c
-- anonymous function:
add3 = \a b c -> a + b + c

-- call in infix notation
-- (1 `add1` 2) 3

successor :: Int -> Int
successor x = x + 1

-- <interactive>:2:11: error:
--     • Couldn't match expected type ‘Int’ with actual type ‘Char’
--     • In the first argument of ‘successor’, namely ‘'c'’
--       In the expression: successor 'c'
--       In an equation for ‘it’: it = successor 'c'

-- <interactive>:3:11: error:
--     • No instance for (Fractional Int) arising from the literal ‘2.0’
--     • In the first argument of ‘successor’, namely ‘2.0’
--       In the expression: successor 2.0
--       In an equation for ‘it’: it = successor 2.0

proofMorgan :: Bool -> Bool -> Bool
proofMorgan a b = not (a && b) == (not a || not b)

-- Now proof it
main = do
  print (proofMorgan True True)
  print (proofMorgan True False)
  print (proofMorgan False True)
  print (proofMorgan False False)