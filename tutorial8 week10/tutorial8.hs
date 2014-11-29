-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 10 - due: 26/27 Nov.

import Data.List
import Test.QuickCheck



-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (v, _, _, _, _) = v
alph   (_, v, _, _, _) = v
start  (_, _, v, _, _) = v
final  (_, _, _, v, _) = v
trans  (_, _, _, _, v) = v

test1 = states m1 == [0,1,2,3,4] && final m2 ==  "ABC"


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm source0 symbol0 = map extractStart . filter check $ trans fsm 
    where
        extractStart (_,_,target) = target
        check (source1,symbol1,_) = source1 == source0 && symbol1 == symbol0 

test2 = delta m1 0 'a' == [1,2] && delta m2 'B' '0' ==  "A"


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts fsm str = accept' fsm str $ start fsm
    where
        accept' fsm []     state = state `elem` final fsm
        accept' fsm (x:xs) state = (not . null ) d && any (accept' fsm xs) d
            where
                d = delta fsm state x


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = nub . sort 


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta fsm sources symbol0 = canonical . map extractStart . filter check $ trans fsm 
    where
        extractStart (_,_,target) = target
        check (source1,symbol1,_) = source1 `elem` sources && symbol1 == symbol0 

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm s0 = canonical $ s0 ++ [ ddelta fsm source symbol | source <- s0, symbol <- alph fsm]



-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm ss | ss == nss = ss
                 | otherwise = reachable fsm nss
                 where
                   nss = next fsm ss

-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm ss = [ s | s <- ss, length (intersect s (final fsm)) > 0 ]


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm is = [ (i, c, ddelta fsm i c) | c <- alph fsm, i <- is ]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic fsm = (sts, alp, sta, fin, tra)
    where
        sts = reachable fsm [[start fsm]]
        alp = alph fsm
        sta = [start fsm]
        fin = dfinal fsm sts
        tra = dtrans fsm sts

