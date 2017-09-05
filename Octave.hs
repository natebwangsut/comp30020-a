--
--  Octave.hs
--  Nate B. Wangsutthitham
--  <@natebwangsut | nate.bwangsut@gmail.com>
--
--  COMP30020: Declarative Programming
--  Created on 27 August 2017
--
--  Explore Octave by fixed pattern
--

------------------------------------------------------------------------------
-- Octave Module
------------------------------------------------------------------------------

module Octave where

import Data.List
import Utility

type OctvCandidate = [[String]]

------------------------------------------------------------------------------
-- Explore Octave Functions [LEGACY]
------------------------------------------------------------------------------

-- Core function: exploring by pattern
exploreOctave :: [String] -> Int -> OctvCandidate -> [String]
exploreOctave guess _ _ = exploredGuess
    where
        guessCandidate = exploreOctaveByPattern guess ["111", "222", "333"]
        -- Check if it conflicts with the current guess, 
        --      if not valid, returning the old octave
        exploredGuess
            | isValidGuess guessCandidate   = guessCandidate
            | otherwise                     = guess


-- Detect pattern, if possible to shift across the pattern, do it
exploreOctaveByPattern :: [String] -> [String] -> [String]
exploreOctaveByPattern guess patterns = octaveGuess
    where
        lastOctvGuess = [last x | x <- guess]
        nextItemOnPattern = nextPattern patterns lastOctvGuess
        octaveGuess
            | nextItemOnPattern == lastOctvGuess
                = guess
            | otherwise
                = changeOctv guess nextItemOnPattern


-- Change octave by given pattern
changeOctv :: [String] -> String -> [String]
changeOctv [] [] = []
changeOctv [] _ = []
changeOctv _ [] = []
changeOctv (pitch:pitches) (pattern:patterns) 
    = [head pitch:[pattern]] ++ changeOctv pitches patterns


-- Extract the octaves from the given list of pitches
extractOctv :: [String] -> String
extractOctv [] = []
extractOctv [[]] = []
extractOctv [x] = [last x]
extractOctv (x:xs) = [last x] ++ extractOctv xs
