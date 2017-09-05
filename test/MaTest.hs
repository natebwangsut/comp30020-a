--
--  MaTest.hs
--  Nate B. Wangsutthitham
--  <@natebwangsut | nate.bwangsut@gmail.com>
--
--  COMP30020: Declarative Programming
--  Created on 31 August 2017
--
--  Modified from the original author: Peter Schachte.
--  Runs all possible guesses.
--

------------------------------------------------------------------------------
-- MaTest Module
------------------------------------------------------------------------------

module MaTest where

import Data.List
import Data.Ratio
import System.Environment
import System.Exit

import Proj1
import Utility

------------------------------------------------------------------------------
-- Core Algorithms
------------------------------------------------------------------------------

-- Compute the correct answer to a guess.  
-- First argument is the target, second is the guess.
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) 
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) 
                    - right

-- 
eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)

-- Main function
test :: Ratio Int
test = average
    where
        note = ['A'..'G']
        octave = ['1'..'3']
        pitch = [n : [o] | n <- note, o <- octave]
        combo = 
            Utility.removeDuplicate [[a, b, c] | a <- pitch, 
                                                 b <- pitch, 
                                                 c <- pitch, 
                                                 a/=b, b/=c, c/=a]

        (guess,other) = initialGuess
        result = [loop com guess other 0 | com <- combo]
        average = sum result % length result

-- TestLoop
loop :: [String] -> [String] -> Proj1.GameState -> Int -> Int
loop target guess other guesses =
    if answer == (3,0,0) then
        guesses
    else
        loop target guess' other' (guesses+1)
        where 
            (guess', other') = nextGuess (guess,other) answer
            answer = (response target guess)
