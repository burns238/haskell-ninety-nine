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







