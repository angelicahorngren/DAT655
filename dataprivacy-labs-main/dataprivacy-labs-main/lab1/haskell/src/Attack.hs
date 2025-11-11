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
indexes dbSize = undefined -- TODO: Implement this function

-- | Performs the noisy sums of all combinations of indexes for a given database @db@. It calls @add@.
allSums :: DB -> Noise -> IO [ResultQuery]
allSums db noise = undefined -- TODO: Implement this function

-- | Given a @candidate@ and some indexes @is@, it performs the sum of the conditions (without noise).
sumIndexes :: Candidate -> [Index] -> ResultQuery
sumIndexes candidate idx = undefined -- TODO: Implement this function

-- | Given a @candidate@, it performs the sums (without noise) of all combinations of indexes.
allSumsNoNoise :: Candidate -> [ResultQuery]
allSumsNoNoise candidate = undefined -- TODO: Implement this function

-- | Given a database @db@, it generates all the possible candidates.
generateCandidates :: DB -> [Candidate]
generateCandidates db = undefined -- TODO: Implement this function

-- | This function will determine if there exists a non-noisy sum on the @candidate@ and a
-- corresponding noisy sum in @results@ whose distance is greater than @noiseMag@.
fit :: Noise -> [ResultQuery] -> Candidate -> Bool
fit noiseMag results candidate = undefined -- TODO: Implement this function

-- | Finds candidates whose non-noisy sums "fit" the noisy ones.
findCandidates :: DB -> Noise -> IO [Candidate]
findCandidates db noise = undefined -- TODO: Implement this function

-- | Guess the conditions of all patients in the dataset.
attack :: DB -> Noise -> IO [CandidateWithNames]
attack db noise = undefined -- TODO: Implement this function
