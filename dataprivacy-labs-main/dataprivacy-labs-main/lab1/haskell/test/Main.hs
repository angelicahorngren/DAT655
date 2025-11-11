module Main(main) where

import Data.Foldable(for_)
import Data.Text.Lazy(unpack)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Text.Pretty.Simple

import DB
import Attack

hammingDistance :: Candidate -> Candidate -> Double
hammingDistance as bs = fromIntegral $ length $ filter (uncurry (/=)) $ zip as bs

mkTestCase :: String -> Noise -> TestTree
mkTestCase dbName noise =
  testCase (dbName <> "-" <> show noise) $ do
    db <- loadDB ("datasets/" <> dbName <> ".csv")
    candidates <- attack db noise

    not (null candidates) @? "Your attack must produce at least 1 candidate."

    let
      numRows = dbSize db
      actualRows = map hiv $ secretRows db

    for_ candidates $ \candidate -> do
      let
        dist = hammingDistance actualRows $ map snd candidate
        maxDist = 4 * noise

        errLength =
          "The length of the following candidate does not match the dataset's row count.\n\n"
          <> unpack (pShow candidate)

        errHamming =
          "The following candidate has a Hamming distance of " <> show dist <> ".\n"
          <> "The maximum admissible distance for noise " <> show noise <> " is " <> show maxDist <> ".\n\n"
          <> unpack (pShow candidate)

      length candidate == numRows @? errLength
      dist <= maxDist @? errHamming

-- The auto-grader includes more tests than just these.
testCases :: [(String, Noise)]
testCases =
  (,)
  <$> ["db1-small", "db1-large", "db2-small", "db2-large"]
  <*> [1, 2, 5, 10]

tests :: TestTree
tests = testGroup "rattack tests" $ map (uncurry mkTestCase) testCases

main :: IO ()
main = defaultMain tests
