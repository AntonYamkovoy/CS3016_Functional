-- Name: Anton Yamkovoy,  Username: yamkovoa
module Ex01 where
import Data.Char (toUpper) -- needed for Part 1
import Data.List


{- Part 1

Write a function 'raise' that converts a string to uppercase

Function 'toUpper :: Char -> Char' converts a character to uppercase
if it is lowercase. All other characters are unchanged

-}

raise = map toUpper

{- Part 2

Write a function 'nth' that returns the nth element of a list

-}
nth :: Int -> [a] -> a

nth n list = list !! (n-1)   -- (!!) list (n-1)       

{- Part 3

write a function commonLen that compares two sequences
and reports the length of the prefix they have in common.

-}
commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix (x:xs) (y:ys) | x == y = x : commonPrefix xs ys
commonPrefix _ _ = []

commonLen :: Eq a => [a] -> [a] -> Int
commonLen [] x = 0
commonLen x [] = 0
commonLen [] [] = 0
commonLen (x:xs) (y:ys) =length( commonPrefix (x:xs) (y:ys))

