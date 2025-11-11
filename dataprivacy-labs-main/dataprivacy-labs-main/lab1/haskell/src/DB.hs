{-# LANGUAGE Trustworthy #-}

module DB
  ( Row(..)
  , DB(secretRows) -- `secretRows` is needed by the test-suite. Do _not_ refer to it in your code! We will grep :)
  , Name
  , HIV
  , Index
  , Noise
  , dbSize
  , names
  , add
  , loadDB
  )
where

import Data.List.Split(splitOn)
import Statistics.Distribution(genContVar)
import Statistics.Distribution.Uniform(uniformDistr)
import System.Random.MWC(createSystemRandom)

-- | Names are modeled as strings.
type Name = String

-- | HIV condition is modeled as a @Double@ for simplicity.
-- Value @0@ means the patient does not have HIV, and @1@ otherwise.
type HIV = Double

-- | A @Row@ is a register with two fields: the @name@ of the patient and their @hiv@ condition.
data Row = Row
  { name :: Name
  , hiv  :: Double
  }
  deriving Show

-- | A database is simply a list of rows of type @Row@.
-- NOTE: The @secretRows@ field is considered secret data that your (hypothetical) attack does not have access to.
-- Do _not_ refer to it in your code! We will grep :)
data DB = DB { secretRows :: [Row] }

-- | An @Index@ is a row in the database.
type Index = Int

-- | We model noise as a @Double@ value.
type Noise = Double

-- | Convert a list of strings to a Row.
toRow :: [String] -> Row
toRow [n, h] = Row { name = n, hiv = read h }
toRow _ = error "toRow: Incorrect format"

-- | Convert CSV data to a list of Rows.
toRows :: [[String]] -> [Row]
toRows csv = map toRow csv

-- | Provides the number of rows in the dataset.
dbSize :: DB -> Int
dbSize (DB rs) = length rs

-- | Provides the names of patients in the same order as they appear in the dataset.
names :: DB -> [Name]
names (DB rs) = map name rs

-- | Sample from an uniform distribution within the interval @[li,ls]@
fromUniform :: Double -> Double -> IO Double
fromUniform li ls = do
  let uni = uniformDistr li ls
  gen <- createSystemRandom
  genContVar uni gen

-- | Adding elements at the position given by the @[Index]@ list with a given noise.
add :: DB -> Noise -> [Index] -> IO Double
add (DB rs) noise is = do
  let s = sum [hiv (rs !! i) | i <- is]
  noise <- fromUniform (-noise) noise
  return (s + noise)

-- | Auxiliary function for parsing CSV files.
parseCSV :: String -> [[String]]
parseCSV = map (splitOn ",") . drop 1 . lines

-- | Load a dataset from a file.
loadDB :: FilePath -> IO DB
loadDB path = fmap (DB . toRows . parseCSV) $ readFile path
