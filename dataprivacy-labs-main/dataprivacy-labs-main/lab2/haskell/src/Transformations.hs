{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

{- |
Module      : DP.Transformations
Description : Data transformations for the Differential Privacy DSL

This module implements transformations that modify datasets while
tracking stability at the type level. Transformations are "free"
in terms of privacy budget but must carefully track how they affect
the stability of datasets to ensure correct noise calibration.
-}
module Transformations (
    -- * Basic transformations (stability preserving)
    dpFilter,
    dpMap,

    -- * Set operations (stability amplifying)
    dpUnion,
    dpIntersect,

    -- * Grouping operations
    dpPartition,
    dpGroupBy,
) where

import GHC.TypeLits
import Core
import PrivacyTracker
import qualified Data.Map.Strict as M
import Control.Monad (foldM, when)
import Control.Monad.State (get, put)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)


-- ============================================================================
-- Basic Transformations (Stability Preserving)
-- ============================================================================

-- | Filter dataset while preserving stability
dpFilter :: (a -> Bool) -> Dataset stb a -> Query (Dataset stb a)
dpFilter p (Dataset xs) = pure (Dataset (filter p xs))

-- | Map function over dataset while preserving stability
dpMap :: (a -> b) -> Dataset stb a -> Query (Dataset stb b)
dpMap f (Dataset xs) = pure (Dataset (map f xs))
-- ============================================================================
-- Set Operations (Stability Amplifying)
-- ============================================================================

-- | Union two datasets with stability amplification
dpUnion :: Dataset stb1 a -> Dataset stb2 a -> Query (Dataset (stb1 + stb2) a)
dpUnion (Dataset xs) (Dataset ys) = pure (Dataset (xs ++ ys))

-- | Intersect two datasets with stability amplification
dpIntersect :: (Eq a) => Dataset stb1 a -> Dataset stb2 a -> Query (Dataset (stb1 + stb2) a)
dpIntersect (Dataset xs) (Dataset ys) = pure (Dataset [x | x <- xs, x `elem` ys])
-- ============================================================================
-- Partition
-- ============================================================================
-- | Partition dataset by keys. Stability is preserved and we only charge 
-- for the maximum budget spent.
dpPartition :: (Ord k) => Epsilon -> [k] -> (a -> k) -> Dataset stb a -> (k -> Dataset stb a -> Query b) -> Query [(k, b)]
dpPartition eps keys keyFn (Dataset xs) handler = do
  let index =
        M.fromListWith (++)
          [ (keyFn x, [x]) | x <- xs ]

      getGroup k = Dataset (M.findWithDefault [] k index)

  -- Get current privacy state
  initialState <- get
  let initialUsed  = epsilonUsed initialState
      initialBudget   = epsilonBudget initialState
      budgetCap = initialUsed + eps

  when (budgetCap > initialBudget) $ -- Check that the budget fits
    liftIO $ throwIO (InsufficientBudget initialBudget budgetCap)

  -- Run handlers in a fold to accumulate max spend across partitions
  (results, maxSpent) <- foldM
    (\(acc, mx) k -> do
        -- Enter partition, reset usage baseline, cap budget
        enterState <- get
        put enterState { epsilonUsed = initialUsed, epsilonBudget = budgetCap }

        r <- handler k (getGroup k) -- Run handler

        -- Get exit state to compute spent budget
        exitState <- get
        let spent = epsilonUsed exitState - initialUsed

        put exitState { epsilonUsed = initialUsed, epsilonBudget = initialBudget } -- Reset state
        pure ((k, r) : acc, max mx spent) -- Update max spend
    )
    ([], 0.0)
    keys

  -- Update state and charge maxSpent budget
  finalState <- get
  put finalState { epsilonUsed = initialUsed + maxSpent, epsilonBudget = initialBudget }

  pure (reverse results) -- Return partition results

-- | Group dataset by key function (stability doubles)
dpGroupBy :: (Ord k) => (a -> k) -> Dataset stb a -> Query (Dataset (2 * stb) (k, [a]))
dpGroupBy keyFn (Dataset xs) = do
    -- mp :: M.Map k [a]
    let mp =
            M.fromListWith (++)
                [ (keyFn x, [x]) | x <- xs ]

    pure (Dataset (M.toList mp))