--
--  Proj1.hs
--  Nate B. Wangsutthitham
--  <@natebwangsut | nate.bwangsut@gmail.com>
--
--  COMP30020: Declarative Programming
--  Created on 23 August 2017
--
--  The code runs with a fixed pattern as a secondary algorithm
--  in order to minimise the guesses that it may run.
--
--  By leveraing Haskells advantage: parallel-list processing, 
--  the computation could be easily calculated.
--
--  Running all possible cases: average of 4.747368421052632 guesses.
--

------------------------------------------------------------------------------
-- Proj1 Module
------------------------------------------------------------------------------

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.Char
import Data.List

-- Import Utility functions and/or secondary calculation if the main fails
import Note
import Octave
import Utility

-- GameState
type GameState = (
    (Guess, Answer),
    NoteCandidate,
    OctvCandidate, 
    FinalCombination
    )

-- Possible combinations left
type FinalCombination = [[String]]
type Guess  = [String]
type Answer = (Int, Int, Int)

------------------------------------------------------------------------------
-- Core Functions
------------------------------------------------------------------------------

-- Initial guess without anything
initialGuess :: ([String], GameState)
initialGuess = (guess, state)
    where
        note    = ['A'..'G']
        octave  = ['1'..'3']
        pitch   = [n : [o] | n <- note, o <- octave]
        combinations = removeDuplicate [[a, b, c] | a <- pitch, 
                                                    b <- pitch, 
                                                    c <- pitch, 
                                                    a /= b, 
                                                    b /= c, 
                                                    c /= a]
        guess = ["A1","B1","C1"]
        state = ((guess, (0,0,0)), [], [], combinations)


-- Second to later guesses
nextGuess :: ([String], GameState) -> (Int, Int, Int) 
                                   -> ([String], GameState)
nextGuess (lastGuess, state) answer = (finalGuess, currentState)
    where
        -- Initialise variables
        (_, n, o, combination)  = state
        (pitch, note, octave)   = answer

        -- Update candidates based on answers
        totalN = pitch + note
        totalO = pitch + octave

        -- Check if there is any new candidate to be added
        -- For debugging purpose
        newNoteCandidate = 
            removeDuplicate (combineNote lastGuess totalN)
        newOctvCandidate = 
            removeDuplicate (combineOctv lastGuess totalO)

        noteCandidate
            | null newNoteCandidate     = n
            | otherwise = removeDuplicate (newNoteCandidate : n)
        octvCandidate 
            | null newOctvCandidate   = o
            | otherwise = removeDuplicate (newOctvCandidate : o)

        -- From existing combination, check if we can filter some
        -- pitch combinations out based on the answer given
        filteredCombinations
            -- There is both correct note and octave in the latest guess
            | not (null newNoteCandidate) && not (null newOctvCandidate)
                = removeDuplicate 
                    [c | c <- combination,
                         containsNote c (extractNote lastGuess) totalN, 
                         containsOctv c (extractOctv lastGuess) totalO, 
                         c /= lastGuess]
            -- Only certain octave were correct
            | not (null newNoteCandidate) && (null newOctvCandidate)
                = removeDuplicate 
                    [c | c <- combination, 
                         containsNote c (extractNote lastGuess) totalN,
                         containsOctv c (extractOctv lastGuess) 0,
                         c /= lastGuess]
            -- Only certain notes were correct
            | (null newNoteCandidate) && not (null newOctvCandidate)
                = removeDuplicate 
                    [c | c <- combination,
                         containsNote c (extractNote lastGuess) 0,
                         containsOctv c (extractOctv lastGuess) totalO, 
                         c /= lastGuess]
            -- There isn't correct note or octave in the latest guess
            | otherwise 
                = removeDuplicate
                    [c | c <- combination, 
                         containsNote c (extractNote lastGuess) 0,
                         containsOctv c (extractOctv lastGuess) 0, 
                         c /= lastGuess]

        -- If possible, pull from the list of possible combinations
        finalGuess 
            | (not . null) filteredCombinations = last filteredCombinations
            | otherwise                         = pattern
                where 
                    newGuess = exploreOctave lastGuess totalO octvCandidate
                    pattern  = exploreNote newGuess totalN noteCandidate

        currentState = 
            ((lastGuess, answer), 
            noteCandidate, 
            octvCandidate, 
            filteredCombinations)

------------------------------------------------------------------------------

-- Check if the pitches combination contains the given note 
--      based on number argument
containsNote :: [String] -> String -> Int -> Bool
containsNote pitches note num = contains head pitches note num


-- Check if the pitches combination contains the given octave 
--      based on number argument
containsOctv :: [String] -> String -> Int -> Bool
containsOctv pitches octv num = contains last pitches octv num


-- Main function to ask if there's an item in the list
-- Will also check for the EXACT number of correct "note or octave"
-- (f) = [a] -> a
contains :: (Eq a) => ([a] -> a) -> [[a]] -> [a] -> Int -> Bool
contains _ [] _ n           = (n == 0)
contains _ _ [] n           = (n == 0)
contains f [x] pattern n    
    | f x `elem` pattern    = n == 1
    | otherwise             = n == 0
contains f (x:xs) pattern n 
    | f x `elem` pattern    = contains f xs (delete (f x) pattern) (n-1)
    | otherwise             = contains f xs pattern n
