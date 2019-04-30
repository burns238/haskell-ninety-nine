-- Copyright 2019 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


{-

I ripped the foundations of this and the tests from 
https://github.com/google/haskell-trainings/tree/master/haskell_101/codelab

To run this file, make sure you have the haskell platform, and simply:
$ make
$ ./ninety-nine-solutions

-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults  #-}

module NinetyNineSolutions where

import Control.Monad        (void)
import Data.Maybe           (isJust)
import Text.Read            (readMaybe)
import Prelude     
import Math.NumberTheory.Powers.Squares
import Data.Foldable
import System.Random
import Data.List


implementThis :: a
implementThis = error "SOMETHING IS NOT IMPLEMENTED!"





{- #####################################################################
   Problems 1 to 10
-}

-- Find the last element of a list
myLast :: [a] -> a
myLast [] = error "Nowt 'ere" 
myLast [x] = x
myLast (_:xs) = myLast xs

-- Find the last but one element of a list
myButLast :: [a] -> a 
myButLast [] = error "Empty list"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Find the K'th element of a list, the first element is number 1
elementAt :: [a] -> Int -> a
elementAt _ 0 = error "Elements start at 1"
elementAt [] _ = error "Empty list"
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs $ i - 1 

-- Find the number of elements of a list
myLength :: [a] -> Int
myLength l = foldl (+) 0 (map (const 1) l) 

-- Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (\a x -> (x:a)) [] 

-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = l == myReverse l 

-- Flatten a nested list structure.
-- -- We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Eliminate consecutive duplicates of list elements. e.g. aaabbbccc -> abc
compress :: (Eq a) => [a] -> [a]
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys

-- Pack consecutive duplicates of list elements into sublists. e.g. aaabbbccc -> (aaa,bbb,ccc)
pack :: (Eq a) => [a] -> [[a]]
pack = getReps []
        where
          getReps :: (Eq a) => [a] -> [a] -> [[a]]
          getReps [] (x:xs) = getReps [x] xs
          getReps l [] = [l]
          getReps (z:zs) (y:ys)
                  | z == y    = getReps ((z:zs) ++ [y]) ys
                  | otherwise = (z:zs):(getReps [y] ys)   

-- Run-length encoding of a list. Use the result of the last problem to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode l = map (\pl -> (myLength pl, head pl)) $ pack l



{- #####################################################################
   Problems 11 to 20
-}

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiple Int a
    deriving (Show, Eq)

packedToListItem :: (Int, a) -> ListItem a 
packedToListItem (1, a) = Single a
packedToListItem (x, a) = Multiple x a

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified l = map (\pl -> packedToListItem (myLength pl, head pl)) $ pack l

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [ListItem a] -> [a]
decodeModified l = foldl (++) [] $ map decodeItem l  
     where
          decodeItem :: ListItem a -> [a]
          decodeItem (Single a) = [a]
          decodeItem (Multiple 1 a) = [a] 
          decodeItem (Multiple i a) = a:decodeItem (Multiple (i-1) a)    

-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by a Single X.
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect [a] = [Single a]
encodeDirect (l:ls) = getReps (Single l) ls
        where
          getReps :: (Eq a) => ListItem a -> [a] -> [ListItem a]
          getReps li [] = [li]
          getReps s@(Single z) (y:ys)
                  | z == y    = getReps (Multiple 2 z) ys
                  | otherwise = s:(getReps (Single y) ys) 
          getReps m@(Multiple i z) (y:ys)
                  | z == y    = getReps (Multiple (i+1) z) ys
                  | otherwise = m:(getReps (Single y) ys)

-- Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Replicate the elements of a list a number of times 
repli :: [a] -> Int -> [a]
repli l i = foldl (++) [] (map (duplicates i) l)  
  where 
    duplicates :: Int -> a -> [a]
    duplicates 0 a = []
    duplicates j a = a:(duplicates (j-1) a)

-- Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery l i = reverse (foldl (\acc (x, y) -> if x `rem` i /= 0 then y:acc else acc) [] $ zip [1..] l) 

-- Split a list into two parts; the length of the first part is given.
-- -- Do not use any predefined predicates.
split :: [a] -> Int -> ([a],[a])
split l i = reps [] l i 
  where 
    reps :: [a] -> [a] -> Int -> ([a],[a])
    reps l1 l2 0 = (reverse l1, l2)
    reps _ [] _ = error "The list is too short" 
    reps l1 (y:ys) x = reps (y:l1) ys $ x - 1

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice l i1 i2 = [ x | (i, x) <- zip [1..] l, i >= i1 && i <= i2 ]

-- Rotate a list N places to the left.
-- e.g. rotate ['a','b','c','d','e','f','g','h'] 3 -> "defghabc"
-- -- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate l@(x:xs) i 
      | i == 0 = l 
      | i > 0 = rotate (xs ++ [x]) (i-1)
      | i < 0 = reverse (rotate (reverse l) (-i))  
rotate l _ = l

-- Remove and separate the K'th element from a list.
removeAt :: [a] -> Int -> (a,[a])
removeAt l n = (l !! (n-1),  [ x | (i, x) <- zip [1..] l, i /= n ])



{- #####################################################################
   Problems 21 to 30
-}

-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt a l 1 = a:l 
insertAt _ [] _ = error "Index out of bounds" 
insertAt a (x:xs) n = x:insertAt a xs (n-1) 

-- Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range i1 i2 = [i1..i2]

-- Extract a given number of randomly selected elements from a list.
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

-- Generate a random permutation of the elements of a list.
rnd_permu :: [a] -> [a]
rnd_permu = implementThis

-- Generate the combinations of K distinct objects chosen from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)


-- Multinomial Coefficients 
--Group the elements of a set into disjoint subsets.

-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

--Example:

-- (group3 '(aldo beat carla david evi flip gary hugo ida))
--( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )... )

-- Example:

-- (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
-- ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) ... )
-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

--You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs =
    [ g:gs | (g,rs) <- combination n xs
           ,  gs    <- NinetyNineSolutions.group ns rs ]
-- Sorting a list of lists according to length of sublists

-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

lsort :: (Ord a) => [[a]] -> [[a]]
lsort = sortBy (\l1 l2 -> compare (length l1) (length l2))

--b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
lfsort ::(Ord a) => [[a]] -> [[a]]
lfsort x = sortBy (\l1 l2 -> compare (getLenFreq x (length l1)) (getLenFreq x (length l2))) x
  where 
    getLenFreq :: [[a]] -> Int -> Int
    getLenFreq ls len = foldl (+) 0 (map (const 1) (filter (\y -> length y == len) ls))  


{- #####################################################################
   Problems 31 to 40
-}

-- Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime = implementThis

-- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGCD :: Int -> Int -> Int
myGCD = implementThis

-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Int -> Int -> Bool
coprime = implementThis

-- Calculate Euler's totient function phi(m).
--  Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
--  Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
totient :: Int -> Int
totient = implementThis

-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors = implementThis

-- Determine the prime factors of a given positive integer. Construct a list containing the prime factors and their multiplicity.
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = implementThis

-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
primesR :: Int -> Int -> [Int]
primesR = implementThis

-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. 
-- Example: 28 = 5 + 23. 
-- It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
goldbach :: Int -> (Int, Int)
goldbach = implementThis

-- Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList = implementThis





















