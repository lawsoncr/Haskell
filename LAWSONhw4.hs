
--Connor Lawson

--Data type of Tree
data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
    deriving Show

--1
getLeaves :: LTree a -> [a]
getLeaves (LLeaf n) = [n]
getLeaves (LNode n t1 t2) = getLeaves t1 ++ getLeaves t2

--2
countNodes :: LTree a -> Integer
countNodes (LLeaf n) = 0
countNodes (LNode n t1 t2) = 1 + countNodes t1 + countNodes t2

--3
minTree :: LTree Integer -> Integer
minTree (LLeaf n) = n
minTree (LNode n t1 t2) | n < min (minTree t1) (minTree t2) = n
                        | otherwise = min (minTree t1) (minTree t2)

--4
occursInLeaves :: (a -> Bool) -> LTree a -> Bool
occursInLeaves p (LLeaf n) = p n
occursInLeaves p (LNode n t1 t2) = occursInLeaves p t1 || occursInLeaves p t2

--5
checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover e (LLeaf n) = n == e
checkNoCover e (LNode n t1 t2) = n /= e && (checkNoCover e t1 || checkNoCover e t2)


--Part 2
foldTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldTree comb base (LLeaf x) = base x
foldTree comb base (LNode y t1 t2) = comb y (foldTree comb base t1)
                                            (foldTree comb base t2)

--1
getLeaves' :: LTree a -> [a]
getLeaves' = foldTree (\x t1 t2 -> t1 ++ t2)(: [])

--2
countNodes' :: LTree a -> Integer
countNodes' = foldTree(\x t1 t2 -> 1 + t1 + t2)(\x -> 0)

--3
minTree' :: LTree Integer -> Integer
minTree' = foldTree (\x t1 t2 -> min x (min t1 t2))(\x -> x)

--4
occursInLeaves' :: (a -> Bool) -> LTree a -> Bool
occursInLeaves' = foldTree(\x t1 t2 -> t1 || t2)

--5
checkNoCover' :: (Eq a) => a -> LTree a -> Bool
checkNoCover' e = foldTree(\x t1 t2 -> e ==x || (t1 || t2))(e ==)