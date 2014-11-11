import Data.List
import Data.Char
import System.Random

----------------------------

getRandFloat :: IO Float
getRandFloat = getStdRandom (randomR (1,6))
