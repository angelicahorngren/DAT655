{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : DP.Core
Description : Core data structures and types for the Differential Privacy DSL

This module defines the fundamental data types used throughout the DSL:
- Dataset with type-level stability tracking
- DPResult for measurement outputs
- Exception types
- Type-level arithmetic for stability composition
-}
module Core (
    -- * Data types
    Dataset (..),
    DPResult (..),
    PrivacyError (..),
    Budget,
    Epsilon,

    -- * Utility functions
    toDouble,
) where

import GHC.TypeLits

-- ============================================================================
-- Core Data Types
-- ============================================================================

-- | Dataset with phantom type for stability tracking
newtype Dataset (stb :: Nat) a = Dataset [a]
    deriving (Show, Eq)

-- | Result of a differentially private computation
data DPResult = DPResult
    { dpValue :: Double
    , epsilonSpent :: Double
    }
    deriving (Show, Eq)

-- | Exception types for privacy operations
data PrivacyError
    = InsufficientBudget Double Double
    | InvalidParameter String
    deriving (Show, Eq)

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- | Convert Integer to Double safely
toDouble :: Integer -> Double
toDouble = fromIntegral

type Epsilon = Double
type Budget = Double

