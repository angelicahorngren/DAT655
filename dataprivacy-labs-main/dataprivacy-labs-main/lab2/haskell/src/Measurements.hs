{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : DP.Measurements
Description : Differential privacy measurements

This module implements the core differentially private measurements
that consume privacy budget and add calibrated noise to query results.
These are the only operations that actually spend epsilon and provide
privacy guarantees.
-}
module Measurements (
    dpCount,
    dpSum,
    LowerBound,
    UpperBound,
) where

import GHC.TypeLits
import Core
import PrivacyTracker
import Noise

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class (liftIO)

type LowerBound = Double
type UpperBound = Double

-- | Differentially private count measurement
dpCount :: forall stb a. (KnownNat stb) => Dataset stb a -> Epsilon -> Query DPResult
dpCount (Dataset xs) eps = do
    when (eps <= 0) $       -- Validate epsilon
        liftIO $ throwIO (InvalidParameter ("Epsilon must be positive, got " ++ show eps))

    checkBudget eps         -- Spend budget
    gen <- getGenerator     -- Get RNG
    
    -- Stability from the type-level nat
    let stability   :: Double
        stability   = toDouble (natVal (Proxy :: Proxy stb))
        sensitivity :: Double
        sensitivity = stability * 1.0  -- count: each row contributes at most 1
        trueCount   :: Double
        trueCount   = fromIntegral (length xs)

    noisy <- addLaplaceNoise gen trueCount sensitivity eps -- add noise
    pure DPResult                       -- return result with noisy value and epsilon spent
        { dpValue      = noisy
        , epsilonSpent = eps
        }

-- | Differentially private sum measurement with bounded contributions
dpSum :: forall stb. (KnownNat stb) => Dataset stb Double -> Epsilon -> LowerBound -> UpperBound -> Query DPResult
dpSum (Dataset xs) eps lo hi = do
    when (eps <= 0) $ -- Validate parameters
        liftIO $ throwIO (InvalidParameter ("Epsilon must be positive, got " ++ show eps))

    when (hi <= lo) $ -- Validate bounds
        liftIO $
            throwIO
                (InvalidParameter ("Upper bound must be greater than lower bound: lo="
                    ++ show lo ++ ", hi=" ++ show hi))

    checkBudget eps -- Spend budget
    gen <- getGenerator -- Get RNG

    -- Stability from type-level nat
    let stability   :: Double
        stability   = toDouble (natVal (Proxy :: Proxy stb))

        sensitivity :: Double
        sensitivity = stability * (max (abs lo) (abs hi))

        clamp :: Double -> Double
        clamp x
            | x < lo    = lo
            | x > hi    = hi
            | otherwise = x

        trueSum :: Double
        trueSum = sum (map clamp xs)

    noisy <- addLaplaceNoise gen trueSum sensitivity eps -- add noise

    pure DPResult -- return result with noisy value and epsilon spent
        { dpValue      = noisy
        , epsilonSpent = eps
        }