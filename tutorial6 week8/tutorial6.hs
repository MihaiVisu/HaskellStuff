-- Informatics 1 Functional Programming
-- Tutorial 6
--
-- Due: 6/7 November

import System.Random


-- Importing the keymap module

import KeymapList


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProduct :: [(Barcode, Item)] -> Int
longestProduct = maximum.map (length.fst.snd)

formatLine :: Int -> (Barcode, Item) -> String
formatLine x xs = fst xs ++ "..." ++ fst (snd xs) ++ [y | y<-(take (x - length (fst (snd xs))) (replicate x '.'))] ++ "..." ++ snd (snd xs)

showCatalogue :: Catalogue -> String
showCatalogue c = unlines ( map ( formatLine n ) xs)
	where
	  xs = toList c
 	  n = longestProduct xs
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList = undefined

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined

catMaybes :: [Maybe a] -> [a]
catMaybes = undefined

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bcs cat = foldr (++) [] (map (unpack.func) bcs)
	where
	  unpack (Just a) = [a]
	  unpack Nothing = []
	  func bc = get bc cat






-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
