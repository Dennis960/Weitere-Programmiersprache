type Koeffizient = Double
type Parabel = (Koeffizient, Koeffizient, Koeffizient)

p :: Parabel
p = (4,2,1)

eval :: Parabel -> Koeffizient -> Koeffizient
eval (a,b,c) x = a * x ^ 2 + b * x + c

derive :: Parabel -> Parabel
derive (a, b, c) = (0, 2*a, b)

slope :: Parabel -> Koeffizient -> Koeffizient
slope parabel = eval (derive parabel)

type Line = [String]
--type Double = [Int] -- möglich, führt aber zu Problemen wegen doppelter Belegung von Double
--type Liste = (Char, Liste) -- nicht möglich wegen Rekursion
--type Bool = Bool -- nicht möglich wegen Rekursion
--type words = String -- nicht möglich, muss mit Großbuchstaben anfangen

data NewList a = Element a (NewList a) | NULL deriving (Show)

list :: NewList Integer
list = Element 1 (Element 2 (Element 3 (Element 4 (Element 5 (Element 6 NULL)))))

newLength :: NewList a -> Int
newLength NULL = 0
newLength (Element a list) = 1 + newLength list

newDrop :: Int -> NewList a -> NewList a
newDrop 0 list = list
newDrop _ (Element a NULL) = NULL
newDrop n (Element a list) = newDrop (n-1) list

(#) :: a -> NewList a -> NewList a
(#) = Element

newTake :: Int -> NewList a -> NewList a
newTake 0 list = NULL
newTake n (Element a list) = a # newTake (n-1) list

(...) :: NewList a -> NewList a -> NewList a
(...) NULL listB = listB
(...) (Element a listA) listB = a # (listA ... listB)

data MyTree a = NoLeaf | Leaf a | Node (MyTree a) a (MyTree a) deriving (Show)

insertIntoTree :: (Ord a) => MyTree a -> a -> MyTree a
insertIntoTree NoLeaf newValue = Leaf newValue
insertIntoTree (Leaf value) newValue | newValue > value = Node NoLeaf value (Leaf newValue)
                                     | newValue == value = Leaf value
                                     | newValue < value = Node (Leaf newValue) value NoLeaf
insertIntoTree (Node leftTree value rightTree) newValue | newValue > value = Node leftTree value (insertIntoTree rightTree newValue)
                                                        | newValue == value = Node leftTree value rightTree
                                                        | newValue < value = Node (insertIntoTree leftTree newValue) value rightTree

binarySearch :: (Ord a) => MyTree a -> a -> Bool
binarySearch NoLeaf _ = False
binarySearch (Leaf haystack) needle = needle == haystack
binarySearch (Node leftTree value rightTree) needle | needle == value = True
                                                    | needle > value = binarySearch rightTree needle
                                                    | needle < value = binarySearch leftTree needle

tree :: MyTree Integer
tree = Node (Node (Leaf 1) 3 NoLeaf) 5 (Node NoLeaf 42 (Node (Node NoLeaf 45 (Leaf 64)) 534 NoLeaf))