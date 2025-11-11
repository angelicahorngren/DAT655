{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Examples
Description : Example usage of the Differential Privacy DSL with Adult Census Dataset

This module demonstrates how to use the various components of the DSL
with real census data, showing practical differential privacy applications.
-}
module Examples where

import AdultDataset (
    Adult (..),
    Income (..),
    Occupation (..),
    loadAdultDataset,
 )
import Core (DPResult (..), Dataset (Dataset), Epsilon)
import qualified Data.Map as Map
import Measurements (dpCount)
import PrivacyTracker (Query, runQuery)
import Transformations (
    dpFilter,
    dpGroupBy,
    dpMap,
    dpPartition,
    dpUnion,
 )

-- ============================================================================
-- Differential Privacy Examples with Adult Census Dataset
-- ============================================================================

-- ============================================================================
-- Basic Examples with Adult Census Dataset
-- ============================================================================

-- | Basic example: counting high earners with multiple epsilon values in one session
basicCountExample :: Dataset 1 Adult -> IO ()
basicCountExample adultData = do
    putStrLn "=== High Earners Count Example ==="

    let Dataset adults = adultData
        trueHighEarnerCount = length [a | a <- adults, income a == HighIncome]
    putStrLn $ "True high earners count: " ++ show trueHighEarnerCount

    -- Run all three queries in a single session with budget composition
    let epsilons = [0.1, 1.0, 5.0]
        totalBudget = sum epsilons
    results <- runQuery (multiEpsilonCountQuery adultData) totalBudget

    putStrLn "DP Count with different epsilon values (single session):"
    mapM_
        ( \(eps, result) ->
            putStrLn $ "  ε=" ++ show eps ++ ": " ++ show (dpValue result)
        )
        (zip epsilons results)

-- | Run multiple count queries with different epsilon values in one session
multiEpsilonCountQuery :: Dataset 1 Adult -> Query [DPResult]
multiEpsilonCountQuery dataset = do
    highEarners <- dpFilter (\person -> income person == HighIncome) dataset

    -- Run three count queries with different epsilon allocations
    result1 <- dpCount highEarners 0.1
    result2 <- dpCount highEarners 0.5
    result3 <- dpCount highEarners 1.0

    return [result1, result2, result3]

-- | Example showing union and stability amplification with overlapping groups
unionStabilityExample :: Dataset 1 Adult -> IO ()
unionStabilityExample adultData = do
    putStrLn "=== Union with Overlapping Groups Example ==="

    let Dataset adults = adultData
        -- Two overlapping groups: high earners and white-collar workers
        highEarnersCount = length [a | a <- adults, income a == HighIncome]
        whiteCollarCount =
            length
                [ a
                | a <- adults
                , occupation a `elem` [ExecManagerial, ProfSpecialty, TechSupport, AdmClerical]
                ]
        -- Count people in both groups (overlap)
        overlapCount = highEarnersCount + whiteCollarCount

    putStrLn $ "True high earners count: " ++ show highEarnersCount
    putStrLn $ "True white-collar workers count: " ++ show whiteCollarCount
    putStrLn $ "Expected union count: " ++ show overlapCount

    -- Run with different epsilon values in single session
    let epsilons = [0.1, 1.0, 5.0]
        totalBudget = sum epsilons
    results <- runQuery (multiEpsilonUnionQuery adultData) totalBudget

    putStrLn "DP Union count with different epsilon values (single session):"
    mapM_
        ( \(eps, result) ->
            putStrLn $ "  ε=" ++ show eps ++ ": " ++ show (dpValue result)
        )
        (zip epsilons results)

-- | Run multiple union queries with different epsilon values in one session
multiEpsilonUnionQuery :: Dataset 1 Adult -> Query [DPResult]
multiEpsilonUnionQuery dataset = do
    -- Group 1: High earners (income > 50K)
    highEarners <- dpFilter (\person -> income person == HighIncome) dataset

    -- Group 2: White-collar workers (overlaps with high earners)
    whiteCollar <-
        dpFilter
            ( \person ->
                occupation person `elem` [ExecManagerial, ProfSpecialty, TechSupport, AdmClerical]
            )
            dataset

    -- Union creates stability amplification since some people appear in both groups
    combined <- dpUnion highEarners whiteCollar

    -- Run three union count queries with different epsilon allocations
    result1 <- dpCount combined 0.1
    result2 <- dpCount combined 1.0
    result3 <- dpCount combined 5.0

    return [result1, result2, result3]

-- ============================================================================
-- GroupBy Example with Occupations
-- ============================================================================

-- | Example using groupBy to analyze occupations with high work hours
occupationAnalysisExample :: Dataset 1 Adult -> IO ()
occupationAnalysisExample adultData = do
    putStrLn "=== Occupation GroupBy Example ==="

    let Dataset adults = adultData
        -- Calculate true average hours per occupation using Map.fromListWith
        occupationGroups = Map.toList $ Map.fromListWith (++) [(occupation a, [a]) | a <- adults]
        occupationAvgs =
            [ (occ, if null people then 0 else fromIntegral (sum [hoursPerWeek p | p <- people]) / fromIntegral (length people))
            | (occ, people) <- occupationGroups
            ]
        trueHighAvgOccupations = length [(occ, avg) | (occ, avg) <- occupationAvgs, avg > 40]

    putStrLn "True average hours per occupation:"
    mapM_ (\(occ, avg) -> putStrLn $ "  " ++ show occ ++ ": " ++ show @Double avg) occupationAvgs
    putStrLn $ "True count of occupations with avg hours > 40: " ++ show trueHighAvgOccupations

    -- Run with different epsilon values in single session
    let epsilons = [1.0, 1.5, 2.0, 10]
        totalBudget = sum epsilons
    results <- runQuery (multiEpsilonOccupationQuery adultData) totalBudget

    putStrLn "DP count of occupations with avg hours > 40 (single session):"
    mapM_
        ( \(eps, result) ->
            putStrLn $ "  ε=" ++ show eps ++ ": " ++ show (dpValue result)
        )
        (zip epsilons results)

-- | Run multiple occupation analysis queries with different epsilon values in one session
multiEpsilonOccupationQuery :: Dataset 1 Adult -> Query [DPResult]
multiEpsilonOccupationQuery dataset = do
    -- Group by occupation (this operation is shared across all queries)
    occGroups <- dpGroupBy occupation dataset

    -- Calculate average hours per occupation (shared computation)
    avgHours <-
        dpMap
            ( \(_occ, people) ->
                let totalHours = sum [hoursPerWeek p | p <- people]
                    count = length people
                 in if count > 0 then fromIntegral @Int @Double totalHours / fromIntegral count else 0
            )
            occGroups

    -- Filter to occupations with high average hours (shared)
    hardWorkingOccs <- dpFilter (> 40) avgHours

    -- Run three count queries with different epsilon allocations
    result1 <- dpCount hardWorkingOccs 1.0
    result2 <- dpCount hardWorkingOccs 1.5
    result3 <- dpCount hardWorkingOccs 2.0
    result4 <- dpCount hardWorkingOccs 10

    return [result1, result2, result3, result4]

-- ============================================================================
-- Partition Example with Age Groups
-- ============================================================================

-- | Demonstrate partition with age-based analysis
partitionExample :: Dataset 1 Adult -> IO ()
partitionExample adultData = do
    putStrLn "=== Age Group Partition Example ==="

    let Dataset adults = adultData
        youngCount = length [a | a <- adults, age a < 30, income a == HighIncome]
        middleCount = length [a | a <- adults, age a >= 30 && age a < 50, income a == HighIncome]
        seniorCount = length [a | a <- adults, age a >= 50, income a == HighIncome]

    putStrLn "True high earner counts by age group:"
    putStrLn $ "  Young (<30): " ++ show youngCount
    putStrLn $ "  Middle (30-49): " ++ show middleCount
    putStrLn $ "  Senior (50+): " ++ show seniorCount

    -- Run with different epsilon values in single session
    let epsilons = [0.1, 1.0, 10.0]
        totalBudget = sum epsilons
    results <- runQuery (multiEpsilonPartitionQuery adultData) totalBudget

    putStrLn "DP high earner counts by age group with different epsilon values (single session):"
    mapM_
        ( \(eps, ageGroupResults) -> do
            putStrLn $ "  ε=" ++ show eps ++ ":"
            mapM_
                ( \(ageGroup, result) ->
                    putStrLn $ "    " ++ ageGroup ++ ": " ++ show (dpValue result)
                )
                ageGroupResults
        )
        (zip epsilons results)

-- | Run multiple partition queries with different epsilon values in one session
multiEpsilonPartitionQuery :: Dataset 1 Adult -> Query [[(String, DPResult)]]
multiEpsilonPartitionQuery dataset = do
    -- Run three partition queries with different epsilon allocations
    result1 <- ageGroupPartitionQuery dataset 0.1
    result2 <- ageGroupPartitionQuery dataset 1.0
    result3 <- ageGroupPartitionQuery dataset 10.0

    return [result1, result2, result3]

ageGroupPartitionQuery :: Dataset 1 Adult -> Epsilon -> Query [(String, DPResult)]
ageGroupPartitionQuery dataset eps = do
    dpPartition eps ageGroups classifyAge dataset countHighEarners
  where
    ageGroups = ["Young", "Middle", "Senior"]
    classifyAge person
        | age person < 30 = "Young"
        | age person < 50 = "Middle"
        | otherwise = "Senior"
    countHighEarners _ageGroup ageGroupData = do
        highEarners <- dpFilter (\person -> income person == HighIncome) ageGroupData
        dpCount highEarners eps

-- ============================================================================
-- Main Function
-- ============================================================================

-- | Run all examples with the Adult Census dataset
main :: IO ()
main = do
    putStrLn "Differential Privacy DSL - Adult Census Dataset Examples"
    putStrLn (replicate 60 '=')

    putStrLn "Loading Adult Census dataset..."
    adultData <- loadAdultDataset

    let Dataset adults = adultData
    putStrLn $ "Loaded " ++ show (length adults) ++ " records"
    putStrLn ""

    -- Run all examples with the loaded dataset
    basicCountExample adultData
    putStrLn ""
    --
    unionStabilityExample adultData
    putStrLn ""

    occupationAnalysisExample adultData
    putStrLn ""
    --
    partitionExample adultData
    putStrLn ""

    putStrLn "All examples completed!"
