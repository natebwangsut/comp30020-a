--
--  Utility.hs
--  Nate B. Wangsutthitham
--  <@natebwangsut | nate.bwangsut@gmail.com>
--
--  COMP30020: Declarative Programming
--  Created on 27 August 2017
--
--  Provide shared functions across different modules.
--  Contains basic data manipulation and light algorithm
--

-------------------------------------------------------------------------------
-- Utility Module
-------------------------------------------------------------------------------

module Utility where

import Data.List
import Data.Char

------------------------------------------------------------------------------
-- Manipulation Functions
------------------------------------------------------------------------------

-- Shared function of Note and Octave module, explore based on pattern.
nextPattern :: [String] -> String -> String
nextPattern [] a = a
nextPattern [[]] a = a
nextPattern (pattern:patterns) guess
    | guess == pattern && not (null patterns)   = head patterns
    | length patterns == 0                      = pattern
    | otherwise                                 = nextPattern patterns guess


-- Check the validity of guess: if there's duplicate element
isValidGuess :: [String] -> Bool
isValidGuess [] = True
isValidGuess (x:xs)
    | x `elem` xs   = False
    | otherwise     = isValidGuess xs

------------------------------------------------------------------------------
-- Increment Functions
------------------------------------------------------------------------------

-- Range the pitch from [A..G]
nextNote :: String -> Int -> String
nextNote pitch n
    | ord note + n > ord 'G'    = nextNote pitch (n-7)
    | ord note + n < ord 'A'    = nextNote pitch (n+7)
    | otherwise                 = chr (ord note + n):[octv]
        where
            note = head pitch
            octv = last pitch


-- Range the octave from [0..3]
nextOctv :: String -> Int -> String
nextOctv pitch n
    | ord octv + n > ord '3'  = nextOctv pitch (n-3)
    | ord octv + n < ord '1'  = nextOctv pitch (n+3)
    | otherwise                 = note:[chr (ord octv + n)]
        where
            note = head pitch
            octv = last pitch

------------------------------------------------------------------------------
-- List Functions
------------------------------------------------------------------------------

-- Get the possible combination based on the given answers and function
combine :: (String -> Char) -> [String] -> Int -> [String]
combine f guess n
    | n == 1    = [[f x] | x <- guess]
    | n == 2    = [f x : [f y] | x <- guess, 
                                 y <- guess, 
                                 x /= y]
    | n == 3    = [f x : (f y : [f z]) | x <- guess, 
                                         y <- guess, 
                                         z <- guess,
                                         x /= y, 
                                         y /= z, 
                                         x /= z]
    | otherwise = []


-- Combine the possible correct notes based on the number of correct guesses
combineNote :: [String] -> Int -> [String]
combineNote guess correctNotes = combine head guess correctNotes


-- Combine the possible correct octaves based on the number of correct guesses
combineOctv :: [String] -> Int -> [String]
combineOctv guess correctOctaves = combine last guess correctOctaves


-- Remove a combine of List of inner list as well
removeDuplicate :: (Ord a, Eq a) => [[a]] -> [[a]]
removeDuplicate [] = []
removeDuplicate [[]] = []
removeDuplicate a
    = purgeDup [sort x | x <- a]


-- Just purge a list of duplicate
purgeDup :: (Eq a) => [a] -> [a]
purgeDup [] = []
purgeDup [x] = [x]
purgeDup (x:xs)
    | x `elem` xs   = purgeDup xs
    | otherwise     = x: purgeDup xs
