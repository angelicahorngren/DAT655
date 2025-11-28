{- |
Module      : DP
Description : Differential Privacy DSL - Main module

A modular implementation of a Domain Specific Language for Differential Privacy.

This module re-exports the main functionality from the sub-modules:
- Core data structures (Dataset, DPResult)
- Privacy budget tracking (Query monad, PrivacyState)
- Data transformations (filter, map, union, groupBy, etc.)
- Differentially private measurements (count, sum, mean, variance)
- Noise generation utilities (Laplace mechanism)
-}
module DP (
    -- * Core data structures
    Dataset (),
    DPResult (),
    PrivacyError (),

    -- * Privacy tracking
    PrivacyState (),
    Query (),
    runQuery,

    -- * Data transformations
    dpFilter,
    dpMap,
    dpUnion,
    dpIntersect,
    dpPartition,
    dpGroupBy,

    -- * Measurements
    dpCount,
    dpSum,

    -- * Re-exports from GHC.TypeLits for convenience
    KnownNat,
    Proxy (..),
) where

-- Core modules
import Core
import Measurements
import PrivacyTracker
import Transformations

-- Re-export useful stuff from base

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat)
