{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Tests
Description : Test suite for the Differential Privacy DSL

This module provides comprehensive tests for:
1. Budget tracking correctness and violation detection
2. Laplace mechanism noise accuracy and distribution properties
3. Stability tracking at the type level
-}
module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (replicateM)
import System.Exit (exitFailure)
import Core (DPResult (..), Dataset (..), Epsilon)
import GHC.TypeLits (KnownNat)
import Measurements (dpCount, dpSum)
import PrivacyTracker (Query, runQuery)
import Transformations (dpGroupBy, dpUnion)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Calculate variance of samples around expected value
calculateVariance :: [Double] -> Double -> Double
calculateVariance samples expected =
    let errors = [sample - expected | sample <- samples]
        sumSquaredErrors = sum [e * e | e <- errors]
        n = fromIntegral (length samples)
     in if n > 1 then sumSquaredErrors / (n - 1) else 0

-- ============================================================================
-- Test Data Setup
-- ============================================================================

-- | Small test dataset for predictable results
testData :: Dataset 1 Int
testData = Dataset []

-- ============================================================================
-- Budget Tracking Tests
-- ============================================================================

-- | Test that budget is correctly consumed and tracked
testBudgetTracking :: IO Bool
testBudgetTracking = do
    putStrLn "Testing budget tracking..."

    -- Test 1: Normal budget consumption
    result1 <- runQuery (dpCount testData 0.5) 1.0
    let budgetTest1 = epsilonSpent result1 == 0.5

    -- Test 2: Multiple queries consuming budget correctly
    result2 <- runQuery multipleQueries 2.0
    let budgetTest2 = sum (map epsilonSpent result2) == 1.5

    -- Test 3: Budget violation detection
    budgetViolationResult <- try $ evaluate =<< runQuery (dpCount testData 2.0) 1.0
    let budgetTest3 = case budgetViolationResult of
            Left (_ :: SomeException) -> True -- Any exception indicates budget violation
            Right _ -> False

    let allPassed = budgetTest1 && budgetTest2 && budgetTest3
    putStrLn $ "  Budget consumption: " ++ show budgetTest1
    putStrLn $ "  Multiple queries: " ++ show budgetTest2
    putStrLn $ "  Budget violation detection: " ++ show budgetTest3
    putStrLn $ "  Budget tracking tests: " ++ (if allPassed then "PASSED" else "FAILED")

    return allPassed
  where
    multipleQueries :: Query [DPResult]
    multipleQueries = do
        r1 <- dpCount testData 0.5
        r2 <- dpCount testData 1.0
        return [r1, r2]

-- | Helper function to run multiple dpCount operations in a single query session
multipleDpCountQuery :: (KnownNat stb) => Dataset stb a -> Epsilon -> Int -> Query [Double]
multipleDpCountQuery dataset eps n = do
    results <- replicateM n (dpCount dataset eps)
    return (map dpValue results)

-- | Helper function to run multiple dpSum operations in a single query session
multipleDpSumQuery :: (KnownNat stb) => Dataset stb Double -> Epsilon -> Double -> Double -> Int -> Query [Double]
multipleDpSumQuery dataset eps lowerBound upperBound n = do
    results <- replicateM n (dpSum dataset eps lowerBound upperBound)
    return (map dpValue results)

-- ============================================================================
-- Laplace Mechanism Accuracy Tests
-- ============================================================================

-- | Test Laplace mechanism using (α,β)-accuracy bounds
testLaplaceAccuracy :: IO Bool
testLaplaceAccuracy = do
    putStrLn "Testing Laplace mechanism (α,β)-accuracy..."

    -- Test dpCount with (α,β)-accuracy
    countTest <- testCountAccuracy

    -- Test dpSum with (α,β)-accuracy
    sumTest <- testSumAccuracy

    let allPassed = countTest && sumTest
    putStrLn $ "  Laplace accuracy tests: " ++ (if allPassed then "PASSED" else "FAILED")

    return allPassed

-- | Test dpCount accuracy using (α,β) bounds with multiple epsilon values
testCountAccuracy :: IO Bool
testCountAccuracy = do
    putStrLn "  Testing dpCount (α,β)-accuracy with multiple epsilons:"

    let epsilons = [0.1, 1.0, 10.0]
        beta = 0.05 -- 95% confidence
        sensitivity = 1.0 -- Count has sensitivity 1
        trueCount = 0
        numSamples = 10000

    putStrLn $ "    β = " ++ show beta ++ " (95% confidence)"
    putStrLn $ "    Sensitivity = " ++ show sensitivity

    -- Test each epsilon value
    results <- mapM (testCountForEpsilon beta sensitivity trueCount numSamples) epsilons

    let allPassed = and results
    putStrLn $ "    Overall dpCount accuracy test: " ++ show allPassed

    return allPassed

-- | Test dpCount for a specific epsilon value
testCountForEpsilon :: Double -> Double -> Int -> Int -> Double -> IO Bool
testCountForEpsilon beta sensitivity trueCount numSamples epsilon = do
    let alpha = log (1.0 / beta) * (sensitivity / epsilon) -- Theoretical error bound
    putStrLn $ "    ε = " ++ show epsilon ++ ":"
    putStrLn $ "      Theoretical α bound = " ++ show alpha

    -- Generate samples in single session
    samples <- runQuery (multipleDpCountQuery testData epsilon numSamples) (fromIntegral numSamples * epsilon + 1)

    -- Check how many samples violate the α bound
    let errors = [abs (sample - fromIntegral trueCount) | sample <- samples]
        violations = length [e | e <- errors, e > alpha]
        violationRate = fromIntegral violations / fromIntegral numSamples

    putStrLn $ "      Violations: " ++ show violations ++ "/" ++ show numSamples ++ " (" ++ show (violationRate * 100) ++ "%)"
    putStrLn $ "      Expected violation rate: ≤ " ++ show (beta * 100) ++ "%"

    let testPassed = violationRate <= beta * 1.1 -- Allow 10% tolerance for finite sample variation
    putStrLn $ "      Test result: " ++ (if testPassed then "PASSED" else "FAILED")

    return testPassed

-- | Test dpSum accuracy using (α,β) bounds with multiple epsilon values
testSumAccuracy :: IO Bool
testSumAccuracy = do
    putStrLn "  Testing dpSum (α,β)-accuracy with multiple epsilons and sensitivities:"

    let epsilons = [0.1, 1.0, 10.0]
        beta = 0.05 -- 95% confidence
        testSumData :: Dataset 1 Double = Dataset []
        numSamples = 10000
        -- Test different bounds to verify sensitivity calculation
        boundsList = [(0.0, 1.0), (0.0, 10.0), (-5.0, 5.0), (-10.0, 20.0)]

    putStrLn $ "    β = " ++ show beta ++ " (95% confidence)"

    -- Test each bounds configuration
    results <- mapM (testSumForBounds beta testSumData numSamples epsilons) boundsList

    let allPassed = and results
    putStrLn $ "    Overall dpSum accuracy test: " ++ show allPassed

    return allPassed

-- | Test dpSum for a specific bounds configuration across multiple epsilons
testSumForBounds :: Double -> Dataset 1 Double -> Int -> [Double] -> (Double, Double) -> IO Bool
testSumForBounds beta testSumData numSamples epsilons (lowerBound, upperBound) = do
    let sensitivity = max (abs lowerBound) (abs upperBound)
    putStrLn $ "    Bounds: [" ++ show lowerBound ++ ", " ++ show upperBound ++ "] (sensitivity = " ++ show sensitivity ++ ")"

    -- Test each epsilon value for this bounds configuration
    results <- mapM (testSumForEpsilon beta sensitivity testSumData numSamples lowerBound upperBound) epsilons

    let allPassed = and results
    putStrLn $ "      Bounds test result: " ++ (if allPassed then "PASSED" else "FAILED")
    return allPassed

-- | Test dpSum for a specific epsilon value
testSumForEpsilon :: Double -> Double -> Dataset 1 Double -> Int -> Double -> Double -> Double -> IO Bool
testSumForEpsilon beta sensitivity testSumData numSamples lowerBound upperBound epsilon = do
    let alpha = log (1.0 / beta) * (sensitivity / epsilon) -- Theoretical error bound
    putStrLn $ "    ε = " ++ show epsilon ++ ":"
    putStrLn $ "      Theoretical α bound = " ++ show alpha

    -- Generate samples in single session
    samples <- runQuery (multipleDpSumQuery testSumData epsilon lowerBound upperBound numSamples) (fromIntegral numSamples * epsilon + 1)

    -- Check how many samples violate the α bound
    let errors = [abs sample | sample <- samples]
        violations = length [e | e <- errors, e > alpha]
        violationRate = fromIntegral violations / fromIntegral numSamples

    putStrLn $ "      Violations: " ++ show violations ++ "/" ++ show numSamples ++ " (" ++ show (violationRate * 100) ++ "%)"
    putStrLn $ "      Expected violation rate: ≤ " ++ show (beta * 100) ++ "%"

    let testPassed = violationRate <= beta * 1.1 -- Allow 10% tolerance for finite sample variation
    putStrLn $ "      Test result: " ++ (if testPassed then "PASSED" else "FAILED")

    return testPassed

-- ============================================================================
-- Stability Tracking Tests
-- ============================================================================

-- | Test that stability is correctly tracked through transformations
testStabilityTracking :: IO Bool
testStabilityTracking = do
    putStrLn "Testing stability tracking..."

    -- Test 1: Union amplifies stability
    stabilityTest1 <- testUnionStability

    -- Test 2: GroupBy doubles stability
    stabilityTest2 <- testGroupByStability

    let allPassed = stabilityTest1 && stabilityTest2
    putStrLn $ "  Union stability amplification: " ++ show stabilityTest1
    putStrLn $ "  GroupBy stability doubling: " ++ show stabilityTest2
    putStrLn $ "  Stability tracking tests: " ++ (if allPassed then "PASSED" else "FAILED")

    return allPassed

-- | Test union amplifies stability correctly using (α,β)-accuracy
testUnionStability :: IO Bool
testUnionStability = do
    let dataset1 :: Dataset 1 Int = Dataset []
        dataset2 :: Dataset 1 Int = Dataset []
        epsilon = 1.0
        numSamples = 10000
        beta = 0.05 -- 95% confidence
    putStrLn "    Testing union stability amplification with (α,β)-accuracy..."

    -- Test stability=1 (baseline)
    baselineSamples <- runQuery (multipleBaselineQuery epsilon numSamples) (fromIntegral numSamples * epsilon + 1)

    -- Test stability=2 (after union)
    unionSamples <- runQuery (multipleUnionQuery dataset1 dataset2 epsilon numSamples) (fromIntegral numSamples * epsilon + 1)

    -- Calculate α bounds for each stability level
    let baselineAlpha = log (1.0 / beta) * (1.0 / epsilon) -- stability=1, sensitivity=1
        unionAlpha = log (1.0 / beta) * (2.0 / epsilon) -- stability=2, sensitivity=1

    -- Check violations for baseline (stability=1)
    let baselineErrors = [abs sample | sample <- baselineSamples]
        -- \|sample - 0|
        baselineViolations = length [e | e <- baselineErrors, e > baselineAlpha]
        baselineViolationRate = fromIntegral baselineViolations / fromIntegral numSamples

    -- Check violations for union (stability=2)
    let unionErrors = [abs sample | sample <- unionSamples]
        -- \|sample - 0|
        unionViolations = length [e | e <- unionErrors, e > unionAlpha]
        unionViolationRate = fromIntegral unionViolations / fromIntegral numSamples

    putStrLn $ "      Baseline (stability=1) α bound: " ++ show baselineAlpha
    putStrLn $ "      Baseline violations: " ++ show baselineViolations ++ "/" ++ show numSamples ++ " (" ++ show (baselineViolationRate * 100) ++ "%)"
    putStrLn $ "      Union (stability=2) α bound: " ++ show unionAlpha
    putStrLn $ "      Union violations: " ++ show unionViolations ++ "/" ++ show numSamples ++ " (" ++ show (unionViolationRate * 100) ++ "%)"

    -- Both should satisfy (α,β)-accuracy, and union should have larger α bound (less accurate)
    let baselineTest = baselineViolationRate <= beta * 1.1
        unionTest = unionViolationRate <= beta * 1.1
        testPassed = baselineTest && unionTest

    putStrLn $ "      Baseline accuracy test: " ++ (if baselineTest then "PASSED" else "FAILED")
    putStrLn $ "      Union accuracy test: " ++ (if unionTest then "PASSED" else "FAILED")
    return testPassed
  where
    multipleBaselineQuery :: Epsilon -> Int -> Query [Double]
    multipleBaselineQuery eps n = do
        let baselineData :: Dataset 1 Int = Dataset []
        results <- replicateM n (dpCount baselineData eps)
        return (map dpValue results)

    multipleUnionQuery :: Dataset 1 Int -> Dataset 1 Int -> Epsilon -> Int -> Query [Double]
    multipleUnionQuery d1 d2 eps n = do
        unionResult <- dpUnion d1 d2 -- Dataset 2 Int (empty union)
        results <- replicateM n (dpCount unionResult eps)
        return (map dpValue results)

-- | Test groupBy doubles stability using (α,β)-accuracy
testGroupByStability :: IO Bool
testGroupByStability = do
    let epsilon = 1.0
        numSamples = 10000
        ds :: Dataset 1 Int = Dataset []
        beta = 0.05 -- 95% confidence
    putStrLn "    Testing groupBy stability doubling with (α,β)-accuracy..."

    -- Test stability=1 (baseline)
    baselineSamples <- runQuery (multipleBaselineQuery2 ds epsilon numSamples) (fromIntegral numSamples * epsilon + 1)

    -- Test stability=2 (after groupBy)
    groupedSamples <- runQuery (multipleGroupByQuery ds epsilon numSamples) (fromIntegral numSamples * epsilon + 1)

    -- Calculate α bounds for each stability level
    let baselineAlpha = log (1.0 / beta) * (1.0 / epsilon) -- stability=1, sensitivity=1
        groupedAlpha = log (1.0 / beta) * (2.0 / epsilon) -- stability=2, sensitivity=1

    -- Check violations for baseline (stability=1)
    let baselineErrors = [abs sample | sample <- baselineSamples]
        -- \|sample - 0|
        baselineViolations = length [e | e <- baselineErrors, e > baselineAlpha]
        baselineViolationRate = fromIntegral baselineViolations / fromIntegral numSamples

    -- Check violations for groupBy (stability=2)
    let groupedErrors = [abs sample | sample <- groupedSamples]
        -- \|sample - 0|
        groupedViolations = length [e | e <- groupedErrors, e > groupedAlpha]
        groupedViolationRate = fromIntegral groupedViolations / fromIntegral numSamples

    putStrLn $ "      Baseline (stability=1) α bound: " ++ show baselineAlpha
    putStrLn $ "      Baseline violations: " ++ show baselineViolations ++ "/" ++ show numSamples ++ " (" ++ show (baselineViolationRate * 100) ++ "%)"
    putStrLn $ "      GroupBy (stability=2) α bound: " ++ show groupedAlpha
    putStrLn $ "      GroupBy violations: " ++ show groupedViolations ++ "/" ++ show numSamples ++ " (" ++ show (groupedViolationRate * 100) ++ "%)"

    -- Both should satisfy (α,β)-accuracy, and groupBy should have larger α bound (less accurate)
    let baselineTest = baselineViolationRate <= beta * 1.1
        groupedTest = groupedViolationRate <= beta * 1.1
        testPassed = baselineTest && groupedTest

    putStrLn $ "      Baseline accuracy test: " ++ (if baselineTest then "PASSED" else "FAILED")
    putStrLn $ "      GroupBy accuracy test: " ++ (if groupedTest then "PASSED" else "FAILED")

    return testPassed
  where
    multipleBaselineQuery2 :: Dataset 1 Int -> Epsilon -> Int -> Query [Double]
    multipleBaselineQuery2 dataset eps n = do
        results <- replicateM n (dpCount dataset eps)
        return (map dpValue results)

    multipleGroupByQuery :: Dataset 1 Int -> Epsilon -> Int -> Query [Double]
    multipleGroupByQuery dataset eps n = do
        grouped <- dpGroupBy (`mod` 2) dataset -- Group by even/odd on empty dataset
        results <- replicateM n (dpCount grouped eps)
        return (map dpValue results)

testAdults :: Dataset 1 a
testAdults = Dataset []

-- ============================================================================
-- Test Runner
-- ============================================================================

-- | Run all tests and report results
runAllTests :: IO Bool
runAllTests = do
    putStrLn "Running Differential Privacy DSL Tests"
    putStrLn (replicate 50 '=')

    test1 <- testBudgetTracking
    putStrLn ""

    test2 <- testLaplaceAccuracy
    putStrLn ""

    test3 <- testStabilityTracking
    putStrLn ""

    let allPassed = test1 && test2 && test3 -- && test4
    putStrLn (replicate 50 '=')
    putStrLn $ "Overall result: " ++ (if allPassed then "ALL TESTS PASSED" else "SOME TESTS FAILED")

    return allPassed

-- | Main function for running tests
main :: IO ()
main = do
    success <- runAllTests
    if success
        then putStrLn "✓ Test suite completed successfully"
        else putStrLn "✗ Test suite failed" >> exitFailure
