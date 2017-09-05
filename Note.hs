--
--  Note.hs
--  Nate B. Wangsutthitham
--  <@natebwangsut | nate.bwangsut@gmail.com>
--
--  COMP30020: Declarative Programming
--  Created on 27 August 2017
--
--  Explore Note by fixed pattern
--

------------------------------------------------------------------------------
-- Note Module
------------------------------------------------------------------------------

module Note where

import Data.List
import Utility

type NoteCandidate = [[String]]

------------------------------------------------------------------------------
-- Explore Note Functions [LEGACY]
------------------------------------------------------------------------------

-- Core function: exploring by pattern
exploreNote :: [String] -> Int -> NoteCandidate -> [String]
exploreNote guess _ _ = exploredGuess
    where
        guessCandidate = exploreNoteByPattern guess ["ABC", "EFG", "CDE"]
        -- Check if it conflicts with the current guess, 
        --      if not valid, returning the old octave
        exploredGuess
            | isValidGuess guessCandidate   = guessCandidate
            | otherwise                     = guess


-- Detect pattern, if possible to shift across the pattern, do it
exploreNoteByPattern :: [String] -> [String] -> [String]
exploreNoteByPattern guess patterns = noteGuess
    where
        lastNoteGuess   = [head x | x <- guess]
        nextItemOnPattern = nextPattern patterns lastNoteGuess
        noteGuess
            | nextItemOnPattern == lastNoteGuess  
                = guess
            | otherwise                     
                = changeNote guess nextItemOnPattern


-- Change note by given pattern
changeNote :: [String] -> String -> [String]
changeNote [] [] = []
changeNote [] _ = []
changeNote _ [] = []
changeNote (pitch:pitches) (pattern:patterns) 
    = [pattern:[last pitch]] ++ changeNote pitches patterns


-- Extract the notes from the given list of pitches
extractNote :: [String] -> String
extractNote [] = []
extractNote [[]] = []
extractNote [x] = [head x]
extractNote (x:xs) = [head x] ++ extractNote xs
