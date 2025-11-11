{- |
Module      : DP.Noise
Description : Noise generation utilities for differential privacy

This module provides functions to generate various types of noise
used in differential privacy mechanisms, particularly the Laplace
mechanism which is fundamental to achieving ε-differential privacy.
-}
module Noise (
    -- * Noise generation
    laplaceNoise,
    addLaplaceNoise,
) where

import Control.Monad.IO.Class
import Statistics.Distribution (genContinuous)
import Statistics.Distribution.Laplace (laplace)
import System.Random.MWC

-- ============================================================================
-- Noise Generation
-- ============================================================================

{- | Generate noise from the Laplace distribution

The Laplace distribution with scale parameter b has PDF:
f(x) = (1/2b) * exp(-|x|/b)

This implementation uses the statistics package which provides proper
statistical distributions with well-tested sampling algorithms.
-}
laplaceNoise :: (MonadIO m) => GenIO -> Double -> m Double
laplaceNoise gen scale
    | scale <= 0 = error $ "Scale must be positive, got " ++ show scale
    | otherwise = liftIO $ genContinuous (laplace 0 scale) gen

-- \| Add calibrated Laplace noise to a value
--
-- Convenience function that combines noise scale calculation and generation.
addLaplaceNoise :: (MonadIO m) => GenIO -> Double -> Double -> Double -> m Double
addLaplaceNoise gen value sensitivity epsilon = do
    noise <- laplaceNoise gen (sensitivity / epsilon)
    return (value + noise)
