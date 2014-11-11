-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 14/15 Nov.

import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Sit :#: b) = split b
split (a :#: Sit) = split a
split (a :#: b)   = split a ++ split b
split a           = [a]


-- 1b. join
join :: [Command] -> Command
join (c:[]) = c
join (c:cs) = c :#: (join cs)


-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent as bs = split as == split bs


-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = c `equivalent` (join (split c))


prop_split :: Command -> Bool
prop_split = all bust . split
 where
   bust (_:#:_) = False
   bust (Sit)   = False
   bust _       = True


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy times com
  | times <= 0 = Sit
  | otherwise = com :#: ( copy (times-1) com )


-- 2b. pentagon
pentagon :: Distance -> Command
pentagon dist = copy 5 ( Go dist :#: Turn 72.0 )


-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon dist sides = copy sides ( Go dist :#: Turn (fromIntegral $ div 360 sides) )



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _    0 _    _     = Sit
spiral side n step angle = (Go side :#: Turn angle) :#: ( spiral (side+step) (n-1) step angle )


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise c
  | split c == op (split c) = c
  | otherwise = optimise $ join (op $ split c)
  where
    op []                     = []
    op ((Sit):xs)             = op xs
    op ((Go a):(Go b):xs)     = (Go (a+b)) : op xs
    op ((Turn a):(Turn b):xs) = (Turn (a+b)) : op xs
    op ((Go 0):xs)            = op xs
    op ((Turn 0):xs)          = op xs
    op (x:xs)                 = x : op xs



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
  where
    f 0 = GrabPen blue :#: Go 10
    f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
    g 0 = GrabPen red :#: Go 10
    g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
    n   = Turn (-60)
    p   = Turn ( 60)


-- 6. snowflake
snowflake :: Int -> Command
snowflake x = (f x) :#: n :#: (f x) :#: n :#: (f x) :#: n
  where
    f 0 = GrabPen black :#: Go 10 
    f x = f (x-1) :#: p :#: f(x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
    n   = Turn (120)
    p   = Turn (-60)


-- 7. hilbert
-- WTF is F?
hilbert :: Int -> Command
hilbert x = l x
  where
    l 0 = GrabPen blue :#: Go 10 
    l x = p :#: r (x-1) :#: n
    r x = Sit
    p   = Turn 90
    n   = Turn (-90)


-- OPTIONAL MATERIAL

peanoGosper :: Int -> Command
peanoGosper x = f x
  where
    f 0 = GrabPen blue :#: Go 10
    f x = f (x-1) :#: p :#: g (x-1) :#: p :#: p :#: g (x-1) :#: n :#: f (x-1) :#: n :#: n :#: f (x-1) :#: f (x-1) :#: n :#: g (x-1) :#: p
    g 0 = GrabPen red :#: Go 10
    g x = n :#: f (x-1) :#: p :#: g (x-1) :#: g (x-1) :#: p :#: p :#: g (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: n :#: g (x-1)
    p   = Turn (60)
    n   = Turn (-60)


cross :: Int -> Command
cross x = (f x) :#: n :#: (f x) :#: n :#: (f x) :#: n :#: (f x) :#: n
  where
    f 0 = GrabPen red :#: Go 10
    f x = f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
    p   = Turn (90)
    n   = Turn (-90)
