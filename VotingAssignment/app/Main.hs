module Main where

import Lib
import Clean.GenCleaning
import Count.AltCount
import Count.STVCount

main :: IO ()
main = do 
    putStrLn "\nSelect the file you wish to load;\n1: Uk.csv\n>>>"
    file <- getLine
    let fileSelected = read file :: Int
    let csvFile = if fileSelected == 1
        then do
          let x = "uk.csv"
          x
        else
          error "Invalid Selection"
    csvData <- readFile csvFile

    putStrLn "Please enter the number of seats\n>>>"
    seatsInput <- getLine
    let seats =  read seatsInput :: Int

    let dirtyVotes = firstClean csvData
    let candidates = getAllCandidates csvData
    let cleanData = cleanVotes dirtyVotes
    let altVoteQuota = getAltVoteQuota cleanData
    let stvQuota = getSTVQuota cleanData seats

    putStrLn "\nSelect one;\n1: Alternative Vote Winner\n2: Single Transferrable Vote Winner\n>>>"
    selectionInput <- getLine
    let selection = read selectionInput :: Int

    if selection == 1
      then do
        print (getAltVoteWinner cleanData (voteCounter cleanData) candidates altVoteQuota)
        main
      else do
        print (getSTVVoteWinner cleanData seats 1 1 [] candidates [])
        main