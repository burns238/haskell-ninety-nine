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
import Prelude       hiding (null, head, tail, length, and, or, (++),
                             map, filter, foldr, foldl, gcd)
import Math.NumberTheory.Powers.Squares
implementThis :: a
implementThis = error "SOMETHING IS NOT IMPLEMENTED!"





{- #####################################################################
   Problems 1 to 10
-}

-- Find the last element of a list
myLast :: [a] -> a
myLast = implementThis

-- Find the last but one element of a list
myButLast :: [a] -> a 
myButLast = implementThis

-- Find the K'th element of a list, the first element is number 1
elementAt :: [a] -> Int -> a
elementAt = implementThis

-- Find the number of elements of a list
myLength :: [a] -> Int
myLength = implementThis

-- Reverse a list
myReverse :: [a] -> [a]
myReverse = implementThis

-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: [a] -> Bool
isPalindrome = implementThis

-- Flatten a nested list structure.
-- -- We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten = implementThis

-- Eliminate consecutive duplicates of list elements. e.g. aaabbbccc -> abc
compress :: [a] -> [a]
compress = implementThis

-- Pack consecutive duplicates of list elements into sublists. e.g. aaabbbccc -> (aaa,bbb,ccc).
pack :: [a] -> [[a]]
pack = implementThis

-- Run-length encoding of a list. Use the result of the last problem to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: [a] -> [(Int, a)]
encode = implementThis


{- #####################################################################
   Problems 11 to 20
-}

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiple Int a
    deriving (Show, Eq)
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = implementThis

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [ListItem a] -> [a]
decodeModified = implementThis

-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by a Single X.
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect = implementThis

-- Duplicate the elements of a list
dupli :: [a] -> [a]
dupli = implementThis

-- Replicate the elements of a list a number of times 
repli :: [a] -> Int -> [a]
repli = implementThis

-- Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery = implementThis

-- Split a list into two parts; the length of the first part is given.
-- -- Do not use any predefined predicates.
split :: [a] -> Int -> ([a],[a])
split = implementThis

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice = implementThis

-- Rotate a list N places to the left.
-- e.g. rotate ['a','b','c','d','e','f','g','h'] 3 -> "defghabc"
-- -- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a] 
rotate = implementThis

-- Remove and separate the K'th element from a list.
removeAt :: [a] -> Int -> (a,[a])
removeAt = implementThis



