{-# LANGUAGE Safe #-}

module Attack where

import Data.List(subsequences)

import DB
import Data.Foldable (traverse_)

-- | A candidate is a guess on the @hiv@ conditions of all the patients.
type Candidate = [HIV]

-- | A candidate with (public) patient names attached.
type CandidateWithNames = [(Name, HIV)]

-- | The result of our queries as a @Double@, since we are releasing sums of @hiv@
-- conditions with noise.
type ResultQuery = Double

-- | Generates all combinations of elements in @ns@ by taking groups of size @k@.
comb :: Int -> [a] -> [[a]]
comb k ns = filter ((k ==) . length) $ subsequences ns

-- | Generates all possible combinations of indexes for a database of given @dbSize@.
indexes :: Int -> [[Index]]
indexes dbSize = concatMap (`comb` [0 .. dbSize - 1]) [1 .. dbSize]

-- | Performs the noisy sums of all combinations of indexes for a given database @db@. It calls @add@.
allSums :: DB -> Noise -> IO [ResultQuery]
allSums db noise = do
    let lendb = dbSize db
    let idxs = indexes lendb
    mapM (add db noise) idxs


-- | Given a @candidate@ and some indexes @is@, it performs the sum of the conditions (without noise).
sumIndexes :: Candidate -> [Index] -> ResultQuery
sumIndexes candidate idx = sum [candidate !! i | i <- idx]
    

-- | Given a @candidate@, it performs the sums (without noise) of all combinations of indexes.
allSumsNoNoise :: Candidate -> [ResultQuery]
allSumsNoNoise candidate =
    let lendb = length candidate
        idxs = indexes lendb
     in map (sumIndexes candidate) idxs

-- | Given a database @db@, it generates all the possible candidates.
generateCandidates :: DB -> [Candidate]
generateCandidates db = let lendb = dbSize db
                            in mapM (const [0.0, 1.0]) [1 .. lendb]
    -- putStrLn $ generateCandidates db

-- | This function will determine if there exists a non-noisy sum on the @candidate@ and a
-- corresponding noisy sum in @results@ whose distance is greater than @noiseMag@.
fit :: Noise -> [ResultQuery] -> Candidate -> Bool
fit noiseMag results candidate = 
    let candidateSums = allSumsNoNoise candidate
        paired = zip candidateSums results
     in all (\(cSum, rSum) -> abs (cSum - rSum) <= noiseMag) paired

-- | Finds candidates whose non-noisy sums "fit" the noisy ones.
findCandidates :: DB -> Noise -> IO [Candidate]
findCandidates db noise = do
    results <- allSums db noise
    let candidates = generateCandidates db
    let fittingCandidates = filter (fit noise results) candidates
    return fittingCandidates

-- | Guess the conditions of all patients in the dataset.
attack :: DB -> Noise -> IO [CandidateWithNames]
attack db noise = do
    candidates <- findCandidates db noise
    let namesList = names db
    let candidatesWithNames = map (\c -> zip namesList c) candidates
    return candidatesWithNames
