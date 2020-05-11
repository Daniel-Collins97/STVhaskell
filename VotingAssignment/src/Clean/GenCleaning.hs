module Clean.GenCleaning where

import Debug.Trace
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)

----------------------------------------------------------
-------------------- Cleaning CSV Data -------------------
----------------------------------------------------------
firstClean :: String -> [[String]]
firstClean xs = do
  let ys = map (splitOn ",") (splitOn "\r\n" xs)
  filter (/=[]) (map (filter(/="")) ys)

----------------------------------------------------------
--------------- Custom Variables and Types ---------------
----------------------------------------------------------

type Candidate = (Char, String)
type Vote = [(Char, String)]
type Count = [(Char, Int)]

weight :: Double
weight = 1000

----------------------------------------------------------
---------------- General Cleaning Methods ----------------
----------------------------------------------------------

getAllCandidates :: String -> [Candidate]
getAllCandidates xs = zip ['A'..] (head (firstClean xs))

-- Lists all valid votes (removes any with empty strings)
getVotes :: [[String]] -> [[String]]
getVotes xs = drop 1 [drop 2 x | x <- xs, "" `notElem` x]


-- Assigns a character from the list ['A'..] to each element in each individual vote indicating which candidate each prefrence is for
pairVotes :: [[String]] -> [Vote]
pairVotes xs = [zip ['A'..] x | x <- getVotes xs]

-- Filters out any preference that has an asterisk instead of a valid vote
findStars :: Vote -> Vote
findStars = filter ((/="*") . snd)

-- Function to go through the entire list of Votes and remove asterisks
removeStars :: [Vote] -> [Vote]
removeStars = map findStars

-- Sorts the votes into ascending order (Sort by preference)
sortVotes :: [Vote] -> [Vote]
sortVotes = map (sortBy (compare `on` snd))

-- Checks each Vote to fund their first preference
checker :: Char -> Vote -> Int
checker x list = sum $ map (const 1) $ filter (== (x, "1")) list

-- Loops through the entire set of Votes to find the first preference of each voter
checkAllVotes :: Char -> [Vote] -> [Int]
checkAllVotes x = map (checker x)

-- Pairs, rmeoves asterisks and sorts the list of DirtyVotes
cleanVotes :: [[String]] -> [Vote]
cleanVotes xs = sortVotes (removeStars (pairVotes xs))

-- Calls the "changePref" function on each individual vote
resetVote :: [Vote] -> [Vote]
resetVote = map changePref

-- Changes the preference of the next choice to the top choice
changePref :: Vote -> Vote
changePref [] = []
changePref [x] = [(fst x, "1")]
changePref (x:xs) = (fst x, "1") : xs