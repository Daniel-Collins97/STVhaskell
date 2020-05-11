module Count.STVCount where

import Debug.Trace
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)

import Clean.GenCleaning

-----------------------------------------------------------------
------------------- Single Transferable Vote --------------------
-----------------------------------------------------------------

getSTVQuota :: [Vote] -> Int -> Float
getSTVQuota xs seats = fromIntegral (length xs) / fromIntegral (seats + 1) + 1

-- Checks if any of the candidates have surpassed the quota and are to be elected
checkElected :: [Float] -> Float -> Bool
checkElected xs quota
    | True `elem` [x >= quota | x <- xs] = True
    | otherwise = False

-- Elects the candidate with the highest votes (Is only called if one candidate has surpassed the quota)
electCandidate :: [Float] -> [Candidate] -> [Candidate] -> [Candidate]
electCandidate xs candidates electedCandidates = do
    let x = zip ['A'..] xs
    let (z:zs) = sortBy (flip compare `on` snd) x
    findElectedCandidate (fst z) candidates : electedCandidates

-- Takes the Char associated with the candidate and finds the candidate from the full list of candidates
findElectedCandidate :: Char -> [Candidate] -> Candidate
findElectedCandidate x (y:ys)
    | x == fst y = y
    | otherwise = findElectedCandidate x ys

-- Finds the candidate with the lowest number of votes
findLowestCandidate :: [Vote] -> String -> Float -> Char
findLowestCandidate xs pref voteWeight = do
    let x = sortBy (compare `on` snd) (filter((/=0) . snd) (zip ['A'..] (stvVoteCounter xs pref voteWeight)))
    fst (head x)

-- Removes any votes accociated with an the eliminated candidate
removeCandidateVotes :: [Vote] -> Char -> [Vote]
removeCandidateVotes xs z = map (filter ((/=z) . fst)) xs

-- Counts the number of votes each candidate has received (uses the vote weight)
stvVoteCounter :: [Vote] -> String -> Float -> [Float]
stvVoteCounter xs pref voteWeight = do
    let x = [sum (checkAllSTVVotes x pref xs) | x <- ['A' .. 'E']]
    map (* voteWeight) x

-- Loops through the entire set of Votes to find the first preference of each voter
checkAllSTVVotes :: Char -> String -> [Vote] -> [Float]
checkAllSTVVotes x pref = map (stvChecker x pref)

stvChecker :: Char -> String -> Vote -> Float
stvChecker x pref list = sum $ map (const 1) $ filter (== (x, pref)) list

-- Finds the surplus of votes from an elected candidate
getSurplus :: Float -> Float -> Float
getSurplus votes quota = votes - quota

-- Gets the new weight that should be applied to each re-distributed vote
getVoteWeight :: Float -> Float -> Float -> Float
getVoteWeight oldWeight surplus z = oldWeight * (surplus / z)

-- Checks each individual vote to ensure that there is a second vote that can be transferred
getTransferableVotesChecker :: Char -> Vote -> Bool
getTransferableVotesChecker candidate xs = (fst (head xs) == candidate) && not (null (tail xs))

getTransferableVotes :: Char -> [Vote] -> Float
getTransferableVotes candidate xs = do
    let zs = map (getTransferableVotesChecker candidate) xs
    fromIntegral (sum $ map fromEnum zs)

-- Changes the second preference of an individuals 
changeEliminatedCandidateVotes :: Vote -> Vote
changeEliminatedCandidateVotes [] = []
changeEliminatedCandidateVotes [x] = [(fst x, "1")]
changeEliminatedCandidateVotes (x:xs) 
    | snd x /= "1" = (fst x, "1") : xs
    | otherwise = x:xs

changeAllCandidateVotes :: [Vote] -> [Vote]
changeAllCandidateVotes = map changeEliminatedCandidateVotes


newWeightVotesMapper :: [Vote] -> [Vote]
newWeightVotesMapper votes = [x | x <- votes, snd (head x) /= "1"]

getSTVVoteWinner :: [Vote] -> Int -> Float -> Int -> [Float] -> [Candidate] -> [Candidate] -> [(Int, String)]
getSTVVoteWinner votes seats voteWeight pref newWeightedVotes allCandidates electedCandidates =
    if length electedCandidates == seats
        then
            zip [1..] (reverse (map snd electedCandidates))
        else do
            let quota = getSTVQuota votes seats
            let totalVotes = if not (null newWeightedVotes)
                then do
                    let x2 = stvVoteCounter votes (show pref) voteWeight
                    let x3 = zipWith (+) x2 newWeightedVotes
                    x3
                else
                    stvVoteCounter votes (show pref) voteWeight
            if checkElected totalVotes quota
                then do
                    let x = electCandidate totalVotes allCandidates electedCandidates 
                    let z = getTransferableVotes (fst (head x)) votes
                    let y = zip ['A'..] totalVotes
                    let surplus = getSurplus (maximum totalVotes) quota
                    let newVoteWeight = getVoteWeight voteWeight surplus z
                    let votes1 = filter (/=[]) (removeCandidateVotes votes (fst (head x)))
                    let newVotes = resetVote (newWeightVotesMapper votes1)
                    let newWeightTransferrableVotes = stvVoteCounter (resetVote votes1) (show pref) newVoteWeight
                    getSTVVoteWinner newVotes seats newVoteWeight 1 newWeightTransferrableVotes allCandidates x
                else do
                    let x1 = findLowestCandidate votes (show pref) voteWeight
                    let votes3 = changeAllCandidateVotes votes
                    let pref1 = 1
                    getSTVVoteWinner votes3 seats voteWeight pref1 [] allCandidates electedCandidates