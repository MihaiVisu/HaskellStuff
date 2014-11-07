-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 9/10 Oct.

import Data.Char
import Data.List
import Test.QuickCheck
import Data.Maybe


-- 1.
rotate       :: Int -> [Char] -> [Char]
rotate n str 
    | n >= 0 && n <= length str = (drop n str) ++ (take n str)
    | otherwise = "Error"

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey    :: Int -> [(Char, Char)]
makeKey nr = zip ['A'..'Z'] (rotate nr ['A'..'Z'])

-- 4.

lookUp          :: Char -> [(Char, Char)] ->Char
lookUp ch [] = ch
lookUp ch pairs
    | length strSearch == 0 = ch
    | otherwise = head strSearch
    where strSearch = [value | (key,value)<- pairs, key==ch]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((key,value) : pairs)
    | key == ch = value
    | otherwise = lookUpRec ch pairs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp ch pairs = lookUp ch pairs == lookUpRec ch pairs

-- 5.
encipher          :: Int -> Char -> Char
encipher shift ch =  lookUp ch (makeKey shift)

-- 6.
normalize    :: String -> String
normalize [] = []
normalize (x:str)
    | isAlpha x = toUpper x : normalize str
    | isDigit x = x : normalize str
    | otherwise = normalize str

-- 7.
encipherStr :: Int -> String -> String
encipherStr shift str = [encipher shift ch | ch <- normalize str]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(b,a) | (a,b) <- xs ]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((key,value):xs) = (value,key) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs
-- 9.
decipher :: Int -> Char -> Char
decipher revShift ch = lookUp ch (reverseKey (makeKey revShift))

decipherStr :: Int -> String -> String
decipherStr i str = [decipher i s | s <- str, isUpper s || isDigit s || s == ' ']

-- 10.
contains :: String -> String -> Bool
contains _ [] = True 
contains [] _ = False
contains str subStr = isPrefixOf subStr str || contains (tail str) subStr

-- The code '_' means that you don't care what parameter you have;
-- _ stands for any parameter (general case);
-- In this case, str could be used instead;

-- 11.
candidates :: String -> [(Int, String)]
candidates str = [(i, decipherStr i str) | i <- [0..25], validate (decipherStr i str)]
    where validate str = str `contains` "AND" || str `contains` "THE"



-- Optional Material

-- 12.
splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive str 
    | l >= 5 = take 5 str : splitEachFive (drop 5 str)
    | otherwise = (str ++ (replicate (5-l) 'X')) : []
    where l = length str

-- 13.
prop_transpose :: String -> Bool
prop_transpose str = transpose (transpose (splitEachFive str)) == splitEachFive str

-- 14.
encrypt :: Int -> String -> String
encrypt shift str = concat (transpose (splitEachFive (encipherStr shift str)))

-- 15.

helperSplit2 :: String -> [String]
helperSplit2 str  
    | length str >= 2 = take 2 str : helperSplit2 (drop 2 str)
    | otherwise = str : []

decrypt :: Int -> String -> String
decrypt shift str = concat (transpose (helperSplit2 (decipherStr shift str)))

-- Challenge (Optional)

-- 16.
countFreqs :: String -> [(Char, Int)]
countFreqs str = [(c,length (filter (==c) str)) | c<-str] 

-- 17
freqDecipher :: String -> [String]
freqDecipher = undefined
