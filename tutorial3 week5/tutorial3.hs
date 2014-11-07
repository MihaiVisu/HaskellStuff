 -- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 16/17 Oct.

import Data.Char
import Test.QuickCheck
import Data.List



-- 1. Map
-- a.
uppers :: String -> String
uppers xs = map toUpper xs

-- b.
doubles :: [Int] -> [Int]
doubles xs = map (2*) xs

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map penceToP xs 
              where penceToP x = fromIntegral x/100

-- d.
uppers' :: String -> String
uppers' xs = [toUpper x | x<- xs]

prop_uppers :: String -> Bool
prop_uppers xs =uppers xs == uppers' xs



-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

-- b.
rmChar ::  Char -> String -> String
rmChar x xs = filter (/=x) xs

-- c.
above :: Int -> [Int] -> [Int]
above x xs = filter (>=x) xs

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = undefined

-- e.
rmCharComp :: Char -> String -> String
rmCharComp  a xs = [x | x <- xs , x /= a]

prop_rmChar :: Char -> String -> Bool
prop_rmChar a xs = rmChar a xs == rmCharComp a xs



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

upperChars' :: String -> String
upperChars' = filter isAlpha.map toUpper

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (2*).filter (>3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' xs = map reverse (filter stringy xs) 
             where stringy xs = even (length xs) == True


prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold = foldr (*) 1

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) | x == True = andRec xs
              | otherwise = False

andFold :: [Bool] -> Bool
andFold xs = length xs == length(foldr (:) [] (filter (/=False) xs))

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [String] -> String
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.

belongsToStr :: Char -> String -> Bool
belongsToStr n [] = False
belongsToStr n (x:xs) | n /= x = belongsToStr n xs
                      | otherwise = True 

rmCharsRec :: String -> String -> String
rmCharsRec x [] = []
rmCharsRec x (y:ys)
           | belongsToStr y x = rmCharsRec x ys
           | otherwise        = y : rmCharsRec x ys


rmCharsFold :: String -> String -> String
rmCharsFold = (flip.foldr) rmChar

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a.
uniform :: [Int] -> Bool
uniform (x:xs) = all (==x) xs

-- b.

trans :: [[Int]] -> [Int]
trans [] = []
trans (x:xs) = length x : trans xs

valid :: Matrix -> Bool
valid [] = False
valid xs = uniform (trans xs) && length (trans xs) > 0

-- 6.

-- 7.

compareM :: Matrix -> Matrix -> Bool
compareM m1 m2 
        | valid m1 == False = False
        | valid m2 == False = False
        | otherwise = (length m1, length (head m1)) == (length m2, length (head m2))

plusRow :: [Int] -> [Int] -> [Int]
plusRow xs ys = zipWith (+) xs ys

plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2 
      |compareM m1 m2 == True = zipWith (plusRow) m1 m2
      |otherwise = error "Fuck!"

-- 8.

dot :: [Int] -> [Int] -> Int
dot xs ys = sum (zipWith (*) xs ys)

dotM :: [Int] -> Matrix -> [Int]
dotM xs m = [dot xs col| col <- transpose m]

timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 = [dotM rw m2 | rw <- m1]

-- Optional material
-- 9.

type PrecisionMatrix = [[Double]]

msize :: PrecisionMatrix -> Int
msize = length

mapMatrix :: (Double -> Double) -> PrecisionMatrix -> PrecisionMatrix
mapMatrix f = map (map f)

coords :: PrecisionMatrix -> [[(Int, Int)]]
coords = zipWith (map . (,)) [0..] . map (zipWith const [0..])

delmatrix :: Int -> Int -> PrecisionMatrix -> PrecisionMatrix
delmatrix i j = dellist i . map (dellist j)
	where dellist i xs = take i xs ++ drop (i + 1) xs

determinant :: PrecisionMatrix -> Double
determinant m
    | msize m == 1 = head (head m)
    | otherwise    = sum $ zipWith addition [0..] m
	where addition i (x:_) =  x * cofactor i 0 m

cofactor :: Int -> Int -> PrecisionMatrix -> Double
cofactor i j m = ((-1.0) ** fromIntegral (i + j)) * determinant (delmatrix i j m)

cofactorM :: PrecisionMatrix -> PrecisionMatrix
cofactorM m = map (map (\(i,j) -> cofactor j i m)) $ coords m

inverse :: PrecisionMatrix -> PrecisionMatrix
inverse m = mapMatrix (* recip det) $ cofactorM m
  where det = determinant m