--Connor Lawson

--1.1 Currying and uncurrying

--1
mapPair :: (a -> b -> c) -> [(a,b)] -> [c]
mapPair z [] = []
mapPair z x = map(\(a,b) -> z a b) x

--2
mapPair' :: (a -> b -> c) -> [(b,a)] -> [c]
mapPair' z [] = []
mapPair' z x = map (\(b,a) -> z a b) x

--1.2 zipWith

--1
diff :: [Integer] -> [Integer] -> [Integer]
diff [] [] =[]
diff l1 l2 = zipWith (-) l1 l2

--2
splice :: [String] -> [String] -> [String]
splice [] []= []
splice l1 l2 = zipWith (\x y -> x++y++x) l1 l2

--1.3 map

--1
sqLens :: [String] -> [Integer]
sqLens [] = []
sqLens l = map ((fromIntegral . (^2)) . length) l

--2
bang :: [String] -> [String]
bang [] = []
bang l = map (++"!") l

--1.4 filter

--1
digitsOnly :: [Integer] -> [Integer]
digitsOnly [] = []
digitsOnly l = filter (\x -> x >= 0 && x <= 9) l

--2
xhelp :: String -> Bool
xhelp "" = True
xhelp s = head s /= 'X'

removeXs :: [String] -> [String]
removeXs [] = []
removeXs l = filter xhelp l

--2 Higher-order: using folds

--1
findNum :: Integer -> [Integer] -> Bool
findNum n [] = False
findNum n (x:xs) = if n == x then True else findNum n xs

findNum' :: Integer -> [Integer] -> Bool
findNum' n = foldr (\x -> (||) (n == x)) False

--2
exists :: (a -> Bool) -> [a] -> Bool
exists n [] = False
exists n (x:xs) = (n x) || exists n xs

exists' :: (a -> Bool) -> [a] -> Bool
exists' n [] = False
exists' n xs = foldr ((||) . n) False xs

--3
noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x:xs) = if x `elem` xs then noDups xs else x: noDups xs

noDups' :: Eq a => [a] -> [a]
noDups' xs = []

--4
countHelp :: Integer -> Integer
countHelp 0 = 0 + 1

countOverflow :: Integer -> [String] -> Integer
countOverflow x [] = 0
--countOverflow x xs = if fromIntegral(x < length head xs) then 0 + 1 else countOverflow x xs


--5
concatList :: [[a]] -> [a]
concatList [] = []
concatList (x:xs) = x ++ concatList xs

concatList' :: [[a]] -> [a]
concatList' xs = foldr (++) [] xs

--6
bindList :: (a -> [b]) -> [a] -> [b]
bindList z [] = []
bindList z xs = concatMap z xs

bindList' :: (a -> [b]) -> [a] -> [b]
bindList' z [] = []