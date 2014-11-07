-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (23/24 Oct)

import Data.List 
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: HTML
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [String]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")
               , ("Mihai Visuian name1 name2","testEmail@ed.ac.uk")
               , ("William Dylan Josh","another@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString xs ys = and [toUpper x == toUpper y | (x,y) <- zip xs ys]


-- 2.
prefix :: String -> String -> Bool
prefix xs ys = isPrefixOf s1 s2
	where
	  s1 = map toUpper xs
	  s2 = map toUpper ys

prop_prefix :: String -> Int -> Bool
prop_prefix str n  =  prefix substr (map toUpper str) &&
		      prefix substr (map toUpper str)
                          where
                            substr  =  take n str


-- 3.
contains :: String -> String -> Bool
contains _ []  = True
contains [] _ = False
contains str substr = prefix s2 s1 || contains (tail s1) s2
	where
          s1 = map toUpper str
          s2 = map toUpper substr
prop_contains :: String -> Int -> Int -> Bool
prop_contains str x y = contains str (substr) && 
			contains str (substr)
				where
				  substr = take y (drop x str)


-- 4.
takeUntil :: String -> String -> String
takeUntil [] str = str
takeUntil _ [] = []
takeUntil xs (y:ys)
	| prefix xs (y:ys) = []
	| otherwise = y: takeUntil xs ys

dropUntil :: String -> String -> String
dropUntil [] str = []
dropUntil _ [] = []
dropUntil xs ys 
	| contains ys xs = drop (length xs + length (takeUntil xs ys)) ys 
	| otherwise = []


-- 5.
split :: String -> String -> [String]
split _ [] = [""]
split [] str = error "No separator taken."
split sep xs 
	| contains xs sep == False = [xs]
	| otherwise = takeUntil sep xs : split sep (dropUntil sep xs)

reconstruct :: String -> [String] -> String
reconstruct sep [x] = x
reconstruct sep (str:strList) = str ++ sep ++ reconstruct sep strList

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML scriptHTML = split sep (dropUntil sep scriptHTML)
	where
	  sep = "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [x|x <- links, contains x "mailto:"]


-- 8.
link2pair :: Link -> (Name, Email)
link2pair lnk
	| contains lnk "mailto:" == False = error "No compatible mailto link"
	| otherwise =  (name,email)
	where
	  name = takeUntil "</a>" (dropUntil "\">" lnk)
	  email = takeUntil "\">" (dropUntil "mailto:" lnk)


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML scriptHTML = nub (map link2pair (takeEmails (linksFromHTML scriptHTML)))

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail searchName searchList = [(xname, xemail) | (xname,xemail) <- searchList, contains  xname searchName]


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML scriptHTML findName = findEmail findName (emailsFromHTML scriptHTML)


-- Optional Material

-- 12.
formatName :: Name -> String
formatName name = reconstruct ", " (tail names ++ [head names])
	where names = split " " name

ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ formatName name ++ replicate (difSpaces name) ' ' ++ email | (name,email) <- addr ]
	where difSpaces nm = maximum [length (formatName x) | (x,y) <- addr] - length (formatName nm) + 1

