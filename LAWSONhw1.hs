
--Connor Lawson

--Part 1
--1. 
minList :: [Integer] -> Integer
minList [] = error "EmptyList"
minList [x] = x
minList (x:xs) = if x < minList xs then x else minList xs

--2.
multiplyList :: [Integer] -> Integer
multiplyList [] = 1
multiplyList (x:xs) = x * multiplyList xs


--3
existsOdd :: [Integer] -> Bool
existsOdd [] = False
existsOdd (x:xs) = if odd x then True else existsOdd xs

--4
findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd (x:xs) = if odd x then Just x else findOdd xs

--5
removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs) = if null x then removeEmpty xs else x : removeEmpty xs

--6
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
                      Nothing -> catMaybes xs
                      Just y -> y : catMaybes xs

--7
collect :: [Either a b] -> ([a], [b])
collect [] = ([],[])

--8
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] [] = True
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys

--9 
findIndex :: Integer -> [Integer] -> Maybe Integer
findIndex n [] = Nothing
--findIndex n y = if n == head y then Just (y !! n)  else findIndex n y

--10
repeatInt :: a -> Integer -> [a]
repeatInt n 0 = []
repeatInt n y = if y > 0 then [n] else repeatInt n (y-1)

--Part 2
--1
addIndex :: [a] -> [(Integer,a)] 
addIndex [] = []
addIndex n = zip [0..] n

--2
swapAll :: [(a,b)] -> [(b,a)]
swapAll [] = []
swapAll ((x,y):xs) = (y, x) : swapAll xs

--3
findDouble :: Eq a => [(a,a)] -> Maybe a
findDouble [] = Nothing
findDouble ((x,y):xs) = if x == y then Just x else findDouble xs

--4
defined :: Maybe a -> Bool
defined Nothing = False
defined (Just n) = True

--5
skip :: [a] -> [a]
skip [] = []
skip [x] = [x]
skip (x:(y:xs)) = x : skip xs

--6
removeEvens :: [Integer] -> [Integer]
removeEvens [] = []
removeEvens n = [ x | x <- n, odd x]

--7
doubleAll :: [Integer] -> [Integer]
doubleAll [] =[]
doubleAll n = [ 2* x | x <- n]

--8
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

--9
countInt :: Integer -> [Integer] -> Integer
countInt n [] = 0
countInt n l = if n == head l then 1 + countInt n (tail l) else countInt n (tail l)


--10
countEq :: Eq a => a -> [a] -> Integer
countEq n [] = 0
countEq n (x:xs) = if n == x then 1 + countEq n xs else countEq n xs