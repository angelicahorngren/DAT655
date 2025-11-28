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

type LowerBound = Double
type UpperBound = Double

-- | Differentially private count measurement
dpCount :: forall stb a. (KnownNat stb) => Dataset stb a -> Epsilon -> Query DPResult
dpCount = undefined

-- | Differentially private sum measurement with bounded contributions
dpSum :: forall stb. (KnownNat stb) => Dataset stb Double -> Epsilon -> LowerBound -> UpperBound -> Query DPResult
dpSum = undefined