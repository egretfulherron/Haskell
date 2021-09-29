


-- QUESTION 1: Sets

uniqueList :: (Eq a) => [a] -> [a]
uniqueList [] = []
uniqueList (x:xs)
        | elem x xs = uniqueList xs
        | otherwise = x : uniqueList xs

bigUnion :: (Eq a) => [[a]] -> [a]
bigUnion [] = []
bigUnion [[x]] = [x]
bigUnion (x:xs) = uniqueList(x ++ bigUnion xs)

partialSums :: [Int] -> [Int]
partialSums xs = sum xs 0
    where sum [] acc = []
          sum (x:xs) acc = (acc+x) : sum xs (acc+x)

findI ::  Int -> [Int] -> Int
findI x [] = error "Not found"
findI x (y:ys) 
        | x == y = 1
        | otherwise = 1 + findI x ys

maxOfList :: [Int] -> Int
maxOfList [x] = x
maxOfList(x:xs)
        |x > maxOfList xs = x
        |otherwise = maxOfList xs

numberCollector :: [Int] -> Int
numberCollector xs = maxOfList (partialSums xs)

helperFunction :: [Int] -> Int
helperFunction xs = findI (numberCollector xs) (partialSums xs) 


maxIndex :: [Int] -> Maybe Int
maxIndex [] = Nothing
maxIndex [x] = Just 1
maxIndex xs
       | helperFunction xs > 0 = Just(helperFunction xs)
       | otherwise = Nothing



-- TEST SET FOR Q1
{-

Your functions should have the following behaviour:

bigUnion [[1,2,3],[3,4,5],[2,4,6,8]] = [1,2,3,4,5,6,8]
bigUnion ["list a", "list b"] = "list ab"
 
THE ORDER OF ELEMENTS IN THE RESULTS bigUnion IS NOT IMPORTANT.

partialSums [1,2,3,4,5] = [1,3,6,10,15]
partialSums [-1,1,-1,1,-1] = [-1,0,-1,0,-1]

maxIndex [1,2,3,4,5] = Just 5
maxIndex [-1,1,-1,1,-1] = Just 2

-}


-- QUESTION 2: Functions and relations

makeCommutative :: [((Int,Int),Int)] -> [((Int,Int),Int)]
makeCommutative xss = error "You've not tried to write makeCommutative yet"

oneHop :: (Eq a) => a -> [(a,a)] -> [a]
oneHop y [] = []
oneHop y (x:xs)
   | fst x == y = snd x : oneHop y xs 
   | otherwise = oneHop y xs

takeUpTo :: Int -> [a] -> [a]
takeUpTo _ [] = []
takeUpTo 1 (x:xs) = [x]
takeUpTo n (x:xs) = x : takeUpTo (n-1) xs

divides :: Int -> Int -> Bool
divides m n = rem m n == 0

divides' :: Int -> Int -> Bool
divides' m n = rem n m == 0

isComposite :: Int -> Bool
isComposite n = foldl (||) False (map (divides n) [2..(n-1)])

isPrime :: Int -> Bool
isPrime n
        | n <=0 = error "Makes no sense"
        | otherwise = not (isComposite n)

nextSteps :: (Eq a) => [a] -> [(a,a)] -> [[a]]
nextSteps xs [x] = []
nextSteps xs (y:ys)
        | last xs == fst y = (snd y : xs) : nextSteps xs ys
        | otherwise = nextSteps xs ys

allElementsReachable :: (Eq a) => Int -> a -> [(a,a)] -> [a]
allElementsReachable n x rs = error "You've not tried to write allElementsReachable yet"

-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:

makeCommutative [((1,2),3),((3,2),5),((1,4),0)] = 
    [((2,1),3),((2,3),5),((4,1),0),((1,2),3),((3,2),5),((1,4),0),((3,3),3),((1,1),1),((2,2),2),((4,4),4)]
    
makeCommutative [((4,1),0)] =
    [((1,4),0),((4,1),0),((4,4),4),((1,1),1)]

oneHop 3 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [2,4,1]
oneHop 1 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [3,4]

DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST FOR oneHop

nextSteps [1,3] [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [[1,3,2],[1,3,4],[1,3,1]]
nextSteps [3,4] [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = []

DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST (i.e. THE ORDER THE LISTS APPEAR IN THE LIST OF LISTS)

allElementsReachable 2 1 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = [2,4,1]
allElementsReachable 6 4 [(1,3),(3,2),(3,4),(3,1),(2,3),(1,4)] = []
-
DO NOT WORRY ABOUT THE ORDER OF THE ELEMENTS IN THE RETURN LIST FOR allElementsReachable

-}

-- QUESTION 3: Primes

primeFinder :: Int -> [Int]
primeFinder 0 = error "Not enough primes"
primeFinder 90000 = [0]
primeFinder n 
    | isPrime (n-1) =(n-1) : primeFinder (n-1)
    | otherwise = lastPrimes (n-1)


lastPrimes :: Int -> [Int]
lastPrimes n 
     | n <= 1000000 = takeUpTo 3(primeFinder n)
     | otherwise = error "is just too big"

factorFind :: Integral a => a -> [a]
factorFind n = [x | x <- [1..n], n `mod` x == 0]

isPrime2 :: Integral a => a -> Bool
isPrime2 n = factorFind n == [1, n]

getList :: Int -> [Int]
getList 0 = [0]
getList n = (uniqueList (filter isPrime2 (factorFind n)))  

notN :: Int -> [Int] -> [Int]
notN _ [] = []
notN x (y:ys) 
     | x == y = notN x ys
     | otherwise = y : notN x ys

primeFactors :: Int -> Maybe [Int]
primeFactors 0 = Nothing
primeFactors n 
   | length(notN n(getList n)) == 0 = Nothing
   | otherwise = Just(notN n (getList n))
   | n >= 1000000 = error "not tried to write big numbers yet"

-- TEST SET FOR Q3
{-

Your functions should have the following behaviour:

lastPrimes 73 = [71,67,61]
lastPrimes 64 = [61,59,53]

DO NOT WORRY ABOUT THE ORDER OF THE LIST FOR lastPrimes

primeFactors 75 = Just [3,5]
primeFactors 64 = Just [2]
primeFactors 61 = Nothing

DO NOT WORRY ABOUT THE ORDER OF THE LIST FOR primeFactors
-}

-- QUESTION 4: RSA

coPrime :: Int -> Int -> Bool
coPrime _ 0 = False
coPrime x y 
    | gcd x y == 1 = True
    | otherwise = False

getTruths :: Int -> Int -> [Int]
getTruths n 0 = []
getTruths n x
    | coPrime n x = x : getTruths n (x-1)
    | otherwise = getTruths n (x-1)

eTotient :: Int -> Int
eTotient 0 = 0
eTotient n = length(getTruths n (n-1)) 


encode :: Int -> Int -> Int -> Int -> Maybe Int
encode p q m e = error "You've not tried to write encode yet"

-- TEST SET FOR Q4
{-
Your functions should have the following behaviour:
eTotient 54 = 18
eTotient 73 = 72
encode 37 23 29 5 = Just 347
encode 99 18 108 45 = Nothing
encode 37 17 23 48 = Nothing
-}

