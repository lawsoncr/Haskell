--Connor Lawson, I did 10 problems

--Number 1
radius :: Double -> Double -> Double
radius x y = if (x == 0) && (y == 0) then 0 else sqrt(x^2 + y^2)

--Number 2
radius' :: (Double,Double) -> Double
radius' (x,y) = if (x,y) == (0,0) then 0 else sqrt(x^2 + y^2)

--Number 3
sumEvens :: Integer -> Integer
sumEvens 0 = 0
sumEvens x = if odd x then sumEvens (x-1) else x + sumEvens (x-1)

--Number 4
sumEvens' :: Integer -> Integer
sumEvens' 0 = 0
sumEvens' x = sum [ x | x <- [1..x], even x]

--Number 5
collatz :: Integer -> Integer
collatz n | (n == 1) || (n == 0) = 1
          | odd n = collatz (3 * n + 1)
          | otherwise = collatz (n `div` 2)

--Number 6
checkCollatz :: Integer -> [Integer]
checkCollatz 0 = [1]
checkCollatz  x = [collatz x | x <- [1..x]]

--Number 7
threeList :: Integer -> [Integer]
threeList 0 = [0]
threeList n = [n | n <- [1..n], n `mod` 3 == 0]
problem7 = threeList 100

--Number 8
init' :: [a] -> [a]
init' [] = error "Cannot take empty list"
init' [x] = []
init' (x:xs) = x : init' xs

--Number 9
findEmpty :: [String] -> Bool
findEmpty [] = True
findEmpty (x:xs) = null x


--Number 10
getLengths :: [String] -> [Int]
getLengths [""] = [0]
getLengths x = [length x | x <- x]


