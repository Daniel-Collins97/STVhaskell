import Debug.Trace
import Data.Function (on)
import Data.List

----------------------------------------------------------
--------------- Custom Variables and Types ---------------
----------------------------------------------------------

type Candidate = (Char, String)
type Vote = [(Char, String)]
type Count = [(Char, Int)]

weight :: Double
weight = 1000

candidates :: [Candidate]
candidates = getAllCandidates dirtyVotes

seats = 4



----------------------------------------------------------
---------------- General Cleaning Methods ----------------
----------------------------------------------------------

-- Takes the first entry of dirtyVotes, removes empty strings and creates a number for each viable candidate
getAllCandidates :: [[String]] -> [Candidate]
getAllCandidates xs = zip ['A'..] (drop 2 (head xs))

-- Lists all valid votes (removes any with empty strings)
getVotes :: [[String]] -> [[String]]
getVotes xs =  [drop 2 x | x <- xs, "" `notElem` x]

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
cleanVotes :: [Vote]
cleanVotes = sortVotes (removeStars (pairVotes dirtyVotes))





---------------------------------------------------------
------------------- Alternative Vote --------------------
---------------------------------------------------------

-- Finds the quota for a successfull candidate
getAlternativeQuota :: Int
getAlternativeQuota = (length cleanVotes `div` 2) + 1

getAltVoteWinner :: [Vote] -> [Int] -> String
getAltVoteWinner x y = do
    let winner = checkWinner y
    if winner
        then getWinner y
        else do
            let a | 0 `elem` y = zip ['A'..] (removeItem 0 y)
                  | otherwise = zip ['A'..] y
            let (b:bs) = sortBy (compare `on` snd) a
            getAltVoteWinner (resetVote (removeLowest (fst b) x)) (voteCounter (resetVote (removeLowest (fst b) x)))

-- Returns a list of integers indicating the amount of first preference votes each candidate recieved
voteCounter :: [Vote] -> [Int]
voteCounter xs = [sum (checkAllVotes x xs) | x <- ['A' .. 'E']]

-- Finds a candidate based on the Char passed into the function
findCandidate :: Char -> [Candidate] -> String
findCandidate x (y:ys)
    | x == fst y = snd y
    | otherwise = findCandidate x ys

-- Returns the Full name of the Winner of the Vote
getWinner :: [Int] -> String
getWinner xs = do
    let x = zip ['A'..] xs
    let (z:zs) = sortBy (flip compare `on` snd) x
    findCandidate (fst z) candidates

-- Checks if the candidate with the highest number of votes has reached the quota yet
checkWinner :: [Int] -> Bool
checkWinner xs
    | True `elem` [x >= getAlternativeQuota | x <- xs] = True
    | otherwise = False

-- Removes the candidat with the lowest number of votes from the race. Also removes their votes
removeLowest :: Char -> [Vote] -> [Vote]
removeLowest x xs = do
    removeCandidate x candidates
    zip ['A'..] candidates
    map (removeVote x) xs

-- Removes individual votes from the list of votes
removeVote :: Char -> Vote -> Vote
removeVote _ [] = []
removeVote x (y:ys) | x == fst y = removeVote x ys
                    | otherwise = y : removeVote x ys

-- Calls the "changePref" function on each individual vote
resetVote :: [Vote] -> [Vote]
resetVote = map changePref

-- Changes the preference of the next choice to the top choice
changePref :: Vote -> Vote
changePref [] = []
changePref [x] = [(fst x, "1")]
changePref (x:xs) = (fst x, "1") : xs

-- Removes an item from the list (Normally removing 0 from list after votes have been re-counted)
removeItem :: Int -> [Int] -> [Int]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Removes a candidate from the list if they have recieved the lowest number of votes in that round of counting
removeCandidate :: Char -> [Candidate] -> [Candidate]
removeCandidate _ [] = []
removeCandidate x xs = [c | c <- xs, fst c /= x]







-----------------------------------------------------------------
------------------- Single Transferable Vote --------------------
-----------------------------------------------------------------

getSTVQuote = (length cleanVotes `div` (seats + 1)) + 1








dirtyVotes :: [[String]]
dirtyVotes = [
    ["","","D. Abbott","E. Balls","A. Burbhm","D. Milliband","E. Milliband"],
    ["1","Ms D Abbott MP  ","1","2","3","4","5"],
    ["2","RtHon B W Ainsworth MP ","5","4","3","1","2"],
    ["3","RtHon D Alexander MP ","5","3","4","1","2"],
    ["4","Ms H Alexander MP ","*","*","1","2","3"],
    ["5","Ms R Ali MP     ","5","3","4","1","2"],
    ["6","Mr G Allen MP   ","*","*","*","1","2"],
    ["7","Mr D Anderson MP ","5","1","4","3","2"],
    ["8","Mr I Austin MP   ","5","1","3","2","4"],
    ["9","Mr A E Bailey MP ","*","3","4","2","1"],
    ["10","Mr W Bain MP    ","5","3","4","1","2"],
    ["11","RtHon E M Balls MP ","*","1","*","*","*"],
    ["12","Mr G R Banks MP ","*","3","4","1","2"],
    ["13","RtHon K J Barron MP ","5","4","1","3","2"],
    ["14","Mr H Bayley MP  ","5","3","4","1","2"],
    ["15","RtHon M M Beckett MP ","*","*","*","*","1"],
    ["16","Miss A Begg MP  ","5","3","4","2","1"],
    ["17","Sir S Bell MP   ","*","2","*","1","*"],
    ["18","RtHon H Benn MP ","*","*","*","2","1"],
    ["19","Mr J E Benton MP ","*","4","1","2","3"],
    ["20","Miss L Berger MP ","5","3","4","2","1"],
    ["","","","","","",""],
    ["21","Mr C Betts MP   ","*","*","1","*","2"],
    ["22","Ms R Blackman-Woods MP ","*","*","2","*","1"],
    ["23","RtHon H A Blears MP ","*","3","1","2","4"],
    ["24","Mr T Blenkinsop MP ","*","1","*","2","*"],
    ["25","Mr P Blomfield MP ","*","*","*","*","1"],
    ["26","RtHon D Blunkett MP ","*","*","1","2","*"],
    ["27","Mr B Bradshaw MP ","*","*","*","1","*"],
    ["28","Mr K Brennan MP ","5","1","4","2","3"],
    ["29","Ms L Brown MP   ","*","1","*","3","2"],
    ["30","Mr R Brown MP   ","*","3","4","1","2"],
    ["31","Mr C Bryant MP  ","5","2","3","1","4"],
    ["32","Ms K Buck MP    ","*","*","*","*","1"],
    ["33","Mr R H Burden MP ","*","3","*","1","2"],
    ["34","RtHon A Burnham MP ","5","4","1","2","3"],
    ["35","Mr L Byrne MP   ","*","2","*","1","3"],
    ["36","Mr D Cairns MP  ","*","*","*","1","*"],
    ["37","Mr A Campbell MP ","*","*","2","1","*"],
    ["38","Mr R Campbell MP ","*","*","1","*","2"],
    ["39","Mr M Cashman MEP","*","*","*","1","2"],
    ["40","Mr M Caton MP   ","2","3","*","*","1"],
    ["","","","","","",""],
    ["41","Ms J Chapman MP ","*","*","*","1","*"],
    ["42","Ms K Clark MP   ","1","2","4","5","3"],
    ["43","RtHon T Clarke MP ","3","5","2","1","4"],
    ["44","RtHon A Clwyd MP ","*","*","*","1","*"],
    ["45","Mr V Coaker MP  ","*","1","4","2","3"],
    ["46","Ms A Coffey MP  ","5","4","3","1","2"],
    ["47","Mr M Connarty MP ","5","2","1","4","3"],
    ["48","Ms R Cooper MP  ","*","2","*","1","*"],
    ["49","Ms Y Cooper MP  ","*","1","*","*","*"],
    ["50","Mr J Corbyn MP  ","1","*","*","*","2"],
    ["51","Mr D Crausby MP ","5","1","4","2","3"],
    ["52","Ms M Creagh MP  ","5","2","4","1","3"],
    ["53","Miss S Creasy MP ","*","*","*","1","2"],
    ["54","Mr J Cruddas MP ","4","2","5","1","3"],
    ["55","Mr J Cryer MP   ","5","2","4","3","1"],
    ["56","Mr A Cunningham MP ","5","3","4","1","2"],
    ["57","Mr J Cunningham MP ","5","1","4","2","3"],
    ["58","Mr T Cunningham MP ","*","1","4","3","2"],
    ["59","Ms M Curran MP  ","5","4","3","2","1"],
    ["60","Mr N Dakin MP   ","5","3","4","1","2"],
    ["","","","","","",""],
    ["61","Mr S Danczuk MP ","*","*","*","1","*"],
    ["62","RtHon A Darling MP ","*","*","*","1","*"],
    ["63","Mr W David MP   ","5","3","4","2","1"],
    ["64","Mr I Davidson MP ","5","1","4","3","2"],
    ["65","Mr G R Davies MP ","*","3","*","2","1"],
    ["66","Ms G De Piero MP ","*","*","*","1","2"],
    ["67","RtHon J Y Denham MP ","*","*","*","*","1"],
    ["68","Mr J Dobbin MP  ","*","1","*","*","2"],
    ["69","RtHon F G Dobson MP ","*","2","*","*","1"],
    ["70","Mr T Docherty MP ","*","4","1","2","3"],
    ["71","Mr B Donohoe MP ","5","3","4","1","2"],
    ["72","Mr F Doran MP   ","*","3","4","2","1"],
    ["73","Mr J Dowd MP    ","*","*","*","1","*"],
    ["74","Miss G Doyle MP ","4","3","5","1","2"],
    ["75","Mr J Dromey MP  ","*","*","*","*","1"],
    ["76","Mr M Dugher MP  ","*","1","*","*","2"],
    ["77","Ms A Eagle MP   ","5","2","4","1","3"],
    ["78","Ms M Eagle MP   ","*","*","*","*","1"],
    ["79","Mr C Efford MP   ","5","3","4","2","1"],
    ["80","Ms J Elliott MP ","*","*","2","1","3"],
    ["","","","","","",""],
    ["81 ","Ms L J Ellman MP ","*","*","2","1","*"],
    ["82","Ms N Engel MP   ","*","*","*","*","1"],
    ["83","Mr B Esterson MP ","5","4","2","3","1"],
    ["84","Mr C Evans MP   ","*","1","4","2","3"],
    ["85","Mr P Farrelly MP ","5","4","3","2","1"],
    ["86","RtHon F Field MP ","*","*","2","*","1"],
    ["87","Mr J Fitzpatrick MP ","*","*","*","1","*"],
    ["88","Mr R C D Flello MP ","*","*","1","2","3"],
    ["89","Ms C Flint MP   ","*","*","*","1","*"],
    ["90","Mr P P Flynn MP ","3","4","2","1","5"],
    ["91","Ms Y Fovargue MP ","*","*","1","2","*"],
    ["92","Dr H Francis MP ","*","2","*","*","1"],
    ["93","Mr M Gapes MP   ","5","3","2","1","4"],
    ["94","Mr B Gardiner MP ","*","*","*","1","2"],
    ["95","Ms S Gilmore MP ","4","5","3","1","2"],
    ["96","Mrs P Glass MP   ","5","2","3","1","4"],
    ["97","Ms M T Glindon MP ","5","4","2","1","3"],
    ["98","Mr R Godsiff MP ","5","4","3","2","1"],
    ["99","Mr P Goggins MP ","*","*","1","2","*"],
    ["100","Ms H Goodman MP ","*","*","*","*","1"],
    ["","","","","","",""],
    ["101","Mr T J Greatrex MP ","*","4","3","2","1"],
    ["102","Ms K Green MP   ","5","1","3","4","2"],
    ["103","Ms L R Greenwood MP ","5","3","4","2","1"],
    ["104","Ms N Griffith MP ","*","1","*","3","2"],
    ["105","Mr A J Gwynne MP ","5","1","4","3","2"],
    ["106","RtHon P Hain MP ","*","*","*","2","1"],
    ["107","The Hon D Hamilton MP ","*","2","3","4","1"],
    ["108","Mr F Hamilton MP ","*","4","3","1","2"],
    ["109","RtHon D Hanson MP ","5","4","2","1","3"],
    ["110","Mr T Harris MP  ","*","*","*","1","*"],
    ["111","Mr D Havard MP   ","*","2","*","*","1"],
    ["112","Mr J Healey MP  ","*","1","*","*","2"],
    ["113","Mr M Hendrick MP ","5","4","3","1","2"],
    ["114","Mr S Hepburn MP ","*","1","*","2","*"],
    ["115","Mr D A Heyes MP ","5","4","1","3","2"],
    ["116","Ms M Hillier MP ","*","2","3","1","*"],
    ["117","Ms J A Hilling MP ","5","4","1","2","3"],
    ["118","RtHon M E Hodge MP ","*","*","*","1","*"],
    ["119","Mrs G Hodges MP ","*","2","*","*","1"],
    ["120","Ms S Hodgson MP ","5","1","2","4","3"],
    ["","","","","","",""],
    ["121","Ms K Hoey MP    ","*","*","1","3","2"],
    ["122","Ms M Honeyball MEP","5","3","4","1","2"],
    ["123","Mr J Hood MP    ","5","2","4","3","1"],
    ["124","Mr K Hopkins MP ","1","3","4","5","2"],
    ["125","RtHon G Howarth MP ","*","2","*","1","*"],
    ["126","Mr R Howitt MEP ","*","*","*","1","2"],
    ["127","Mr L Hoyle MP   ","*","1","*","3","2"],
    ["128","Mr S Hughes MEP ","*","*","3","2","1"],
    ["129","Mr T Hunt MP    ","5","2","4","1","3"],
    ["130","Mr H Irranca-Davies MP ","*","4","2","1","3"],
    ["131","Mrs S James MP  ","*","2","*","*","1"],
    ["132","Ms C Jamieson MP ","5","4","2","3","1"],
    ["133","RtHon A A Johnson MP ","*","*","*","1","*"],
    ["134","Ms D R Johnson MP ","*","1","*","2","*"],
    ["135","Mr G Jones MP   ","*","2","3","4","1"],
    ["136","Ms H Jones MP   ","*","1","*","3","2"],
    ["137","Mr K Jones MP   ","*","*","2","1","*"],
    ["138","Ms S E Jones MP ","*","*","*","2","1"],
    ["139","RtHon T J Jowell MP ","4","5","2","1","3"],
    ["140","Mr E Joyce MP   ","5","1","3","4","2"],
    ["","","","","","",""],
    ["141","RtHonSir G B Kaufman MP","*","4","3","1","2"],
    ["142","Ms B Keeley MP   ","5","1","4","3","2"],
    ["143","Mr A Keen MP    ","5","4","1","3","2"],
    ["144","Miss E Kendall MP ","*","*","*","1","*"],
    ["145","Mr S Khan MP    ","*","*","*","*","1"],
    ["146","Mr D Lammy MP   ","2","3","5","1","4"],
    ["147","Mr I Lavery Snr MP ","*","*","2","*","1"],
    ["148","Mr M Lazarowicz MP ","*","*","*","*","1"],
    ["149","Mr C Leslie MP   ","5","1","4","2","3"],
    ["150","Mr I Lewis MP   ","*","*","*","1","*"],
    ["151","Mr A Love MP    ","5","3","4","2","1"],
    ["152","Mr I Lucas MP    ","5","2","4","3","1"],
    ["153","RtHon D MacShane MP ","*","*","*","1","*"],
    ["154","Ms F Mactaggart MP ","*","2","*","1","*"],
    ["155","Mr K Mahmood MP ","3","1","5","4","2"],
    ["156","Ms S Mahmood MP ","*","*","*","*","1"],
    ["157","Mr J Mann MP    ","*","*","*","1","2"],
    ["158","Mr G Marsden MP ","*","4","3","2","1"],
    ["159","Mr D Martin MEP ","*","*","*","1","*"],
    ["160","Ms L McAvan MEP ","*","*","*","*","1"],
    ["","","","","","",""],
    ["161","Mr S J McCabe MP ","*","1","*","3","2"],
    ["162","Mr M McCann MP  ","*","*","*","1","*"],
    ["163","Ms A McCarthy MEP","*","*","3","2","1"],
    ["164","Ms K McCarthy MP ","*","1","*","2","*"],
    ["165","Mr G McClymont MP ","5","3","4","1","2"],
    ["166","Ms S McDonagh MP ","*","*","*","1","*"],
    ["167","Mr J McDonnell MP ","1","*","*","*","*"],
    ["168","Mr P McFadden MP ","*","*","*","1","*"],
    ["169","Ms A McGovern MP ","5","4","2","1","3"],
    ["170","Mr J McGovern MP ","*","*","*","*","1"],
    ["171","Mrs A McGuire MP ","5","2","4","1","3"],
    ["172","Ms A McKechin MP ","*","2","*","*","1"],
    ["173","Mrs C McKinnell MP ","*","*","*","*","1"],
    ["174","RtHon M Meacher MP ","2","*","*","*","1"],
    ["175","Mr A Meale MP   ","5","3","4","2","1"],
    ["176","Mr I Mearns MP   ","5","2","4","1","3"],
    ["177","RtHon A Michael MP ","5","2","4","1","3"],
    ["178","RtHon D Miliband MP ","*","*","*","1","2"],
    ["179","RtHon E Miliband MP ","*","*","*","2","1"],
    ["180","Mr A P Miller MP ","*","4","1","2","3"],
    ["","","","","","",""],
    ["181","Mr A Mitchell MP ","4","3","5","2","1"],
    ["182","Mrs M Moon MP   ","*","2","*","*","1"],
    ["183","Mr C Moraes MEP ","*","*","*","1","*"],
    ["184","Ms J Morden MP   ","5","4","3","1","2"],
    ["185","Mr G Morrice MP ","*","*","*","*","1"],
    ["186","Mr G M Morris MP ","4","3","2","5","1"],
    ["187","Mr G Mudie MP   ","*","1","*","*","2"],
    ["188","Ms M Munn MP    ","*","*","*","1","*"],
    ["189","Mr J Murphy MP  ","*","*","*","1","*"],
    ["190","RtHon P Murphy MP ","*","*","*","2","1"],
    ["191","Mr I Murray MP   ","5","3","4","1","2"],
    ["192","Ms L Nandy MP   ","*","2","*","*","1"],
    ["193","Miss P Nash MP  ","*","*","*","1","*"],
    ["194","Mrs F O'Donnell MP ","5","4","3","1","2"],
    ["195","Ms C Onwurah MP ","3","*","*","2","1"],
    ["196","Ms S Osborne MP ","*","*","*","*","1"],
    ["197","Mr A Owen MP    ","*","4","2","3","1"],
    ["198","Ms T Pearce MP   ","*","1","*","3","2"],
    ["199","Mr M T Perkins MP ","5","2","4","1","3"],
    ["200","Ms B M Phillipson MP ","*","*","*","1","*"],
    ["","","","","","",""],
    ["201","Mr S Pound MP   ","*","4","2","3","1"],
    ["202","RtHon D Primarolo MP ","*","*","*","*","1"],
    ["203","Ms Y Qureshi MP ","*","*","*","1","*"],
    ["204","RtHon N Raynsford MP ","*","*","*","1","*"],
    ["205","Mr J R Reed MP   ","*","4","3","1","2"],
    ["206","Ms R J Reeves MP ","5","3","4","2","1"],
    ["207","Ms E Reynolds MP ","5","3","4","2","1"],
    ["208","Mr J N Reynolds MP ","*","*","*","1","2"],
    ["209","Ms L Riordan MP ","1","*","*","*","2"],
    ["210","Mr J Robertson MP ","5","1","4","3","2"],
    ["211","Mr G Robinson MP ","5","1","4","2","3"],
    ["212","Mr S P Rotheram MP ","*","*","1","*","2"],
    ["213","Mr F Roy MP     ","*","*","*","1","*"],
    ["214","Mr L A Roy MP   ","*","2","*","3","1"],
    ["215","Mr C Ruane MP   ","5","3","4","1","2"],
    ["216","Ms J Ruddock MP ","*","*","*","*","1"],
    ["217","Mr A Sarwar MP  ","*","*","*","1","2"],
    ["218","Ms A Seabeck MP ","*","*","*","*","1"],
    ["219","Mr V K Sharma MP ","5","3","4","1","2"],
    ["220","Mr B Sheerman MP ","*","*","*","1","*"],
    ["","","","","","",""],
    ["221","Mr J Sheridan MP ","*","*","*","*","1"],
    ["222","Mr G Shuker MP  ","*","2","*","*","1"],
    ["223","Mr B Simpson MEP","*","*","1","*","2"],
    ["224","Mr M Singh MP   ","*","3","*","2","1"],
    ["225","Mr D E Skinner MP ","*","*","*","1","*"],
    ["226","Mr P Skinner MEP","*","*","*","1","*"],
    ["227","Mr A F Slaughter MP ","*","*","*","*","1"],
    ["228","RtHon A Smith MP ","5","1","4","2","3"],
    ["229","Ms A C Smith MP ","*","*","2","1","*"],
    ["230","Mr N Smith MP   ","5","2","4","1","3"],
    ["231","Mr O Smith MP   ","5","2","4","3","1"],
    ["232","Sir P Soulsby KBE MP ","5","4","3","1","2"],
    ["233","RtHon J F Spellar MP ","*","1","4","2","3"],
    ["234","Ms C Stihler MEP","5","4","3","2","1"],
    ["235","RtHon J W Straw MP ","*","3","2","1","*"],
    ["236","Mr G Stringer MP ","*","*","2","1","*"],
    ["237","Ms G Stuart MP  ","*","*","*","1","*"],
    ["238","Mr G Sutcliffe MP ","5","3","1","2","4"],
    ["239","Mr M R Tami MP   ","5","2","3","1","4"],
    ["240","Mr G Thomas MP  ","*","*","*","1","*"],
    ["","","","","","",""],
    ["241","Ms E Thornberry MP ","*","*","*","*","1"],
    ["242","RtHon S Timms MP ","*","4","3","2","1"],
    ["243","Mr J Trickett MP ","4","1","*","3","2"],
    ["244","Mr K Turner MP   ","5","4","1","2","3"],
    ["245","Mr D Twigg MP   ","5","3","1","2","4"],
    ["246","Mr S Twigg MP   ","*","*","*","1","2"],
    ["247","Mr C Umunna MP  ","*","*","*","2","1"],
    ["248","Mr D Vaughan MEP","*","*","*","2","1"],
    ["249","RtHon K Vaz MP   ","4","3","5","1","2"],
    ["250","Ms V Vaz MP     ","2","*","*","1","3"],
    ["251","Ms J L Walley MP ","*","*","2","*","1"],
    ["252","Mr T Watson MP   ","5","1","3","4","2"],
    ["253","Mr D Watts MP   ","4","2","1","5","3"],
    ["254","Mr A Whitehead MP ","*","*","2","3","1"],
    ["255","Mr M Wicks MP   ","5","4","3","1","2"],
    ["256","Mr C Williamson MP ","5","2","4","3","1"],
    ["257","Ms G Willmott MEP","*","*","*","2","1"],
    ["258","Mr P Wilson MP  ","*","*","2","1","*"],
    ["259","Mr D Winnick MP ","*","*","*","1","*"],
    ["260","RtHon R Winterton MP ","*","*","*","*","1"],
    ["","","","","","",""],
    ["261","Mr M Wood MP    ","1","3","*","*","2"],
    ["262","Mr J Woodcock MP ","*","4","2","1","3"],
    ["263","RtHon S Woodward MP ","5","3","4","1","2"],
    ["264","Mr P Woolas MP  ","*","*","*","1","*"],
    ["265","Mr D Wright MP  ","*","1","4","2","3"],
    ["266","Mr I D Wright MP ","5","1","3","4","2"],
    [""]]