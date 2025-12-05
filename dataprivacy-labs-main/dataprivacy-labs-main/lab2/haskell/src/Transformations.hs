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
import Control.Monad (forM)
import qualified Data.Map.Strict as M

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

dpPartition :: (Ord k) => Epsilon -> [k] -> (a -> k) -> Dataset stb a -> (k -> Dataset stb a -> Query b) -> Query [(k, b)]
dpPartition _eps keys keyFn (Dataset xs) handler = do
    -- index :: M.Map k [a]
    let index =
            M.fromListWith (++)
                [ (keyFn x, [x]) | x <- xs ]

        -- same k / stb / a as in dpPartition
        getGroup k = Dataset (M.findWithDefault [] k index)

    forM keys $ \k -> do
        res <- handler k (getGroup k)
        pure (k, res)

-- | Group dataset by key function (stability doubles)
dpGroupBy :: (Ord k) => (a -> k) -> Dataset stb a -> Query (Dataset (2 * stb) (k, [a]))
dpGroupBy keyFn (Dataset xs) = do
    -- mp :: M.Map k [a]
    let mp =
            M.fromListWith (++)
                [ (keyFn x, [x]) | x <- xs ]

    pure (Dataset (M.toList mp))