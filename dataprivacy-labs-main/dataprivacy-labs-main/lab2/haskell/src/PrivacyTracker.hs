{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : DP.PrivacyTracker
Description : Privacy budget tracking for differential privacy sessions

This module implements the privacy accounting system using a State monad
to track epsilon consumption across multiple queries in a session, ensuring
that the total privacy budget is not exceeded.
-}
module PrivacyTracker (
    -- * Privacy state
    PrivacyState (..),
    Query (..),

    -- * Query execution
    runQuery,
    checkBudget,
    getGenerator,
) where

import Control.Monad.IO.Class
import Control.Monad.State
import System.Random.MWC
import Text.Printf
import Control.Exception (throwIO)
import Core (PrivacyError(..))

-- ============================================================================
-- Privacy State and Query Monad
-- ============================================================================

{- | Privacy state for tracking budget consumption

This tracks the privacy accounting across a session of queries.
The basic composition theorem states that if we run k mechanisms
with privacy parameters ε₁, ε₂, ..., εₖ, the total privacy cost
is ε₁ + ε₂ + ... + εₖ.
-}
data PrivacyState = PrivacyState
    { epsilonUsed :: Double
    -- ^ Total epsilon consumed so far
    , epsilonBudget :: Double
    -- ^ Total epsilon budget available
    , randomGen :: GenIO
    -- ^ MWC random number generator
    }

{- | Query monad for tracking privacy budget

This monad combines State for privacy tracking with IO for randomness.
All differentially private operations are performed within this monad
to ensure proper budget accounting.
-}
newtype Query a = Query (StateT PrivacyState IO a)
    deriving (Functor, Applicative, Monad, MonadState PrivacyState, MonadIO)

-- ============================================================================
-- Query Execution
-- ============================================================================

type Budget = Double

-- | Run a query with given privacy budget
-- This initializes the new privacy state and executes the query.
runQuery :: Query a -> Budget -> IO a
runQuery (Query q) budget = do
    gen <- createSystemRandom
    let initialState = PrivacyState {epsilonUsed = 0, epsilonBudget = budget, randomGen = gen}
    evalStateT q initialState

-- ============================================================================
-- Budget Management
-- ============================================================================

-- | Force spending epsilon budget, throwing error if insufficient
-- Reads epsilonUsed and epsilonBudget from state and updates epsilonUsed unless it would exceed budget.
checkBudget :: Double -> Query ()
checkBudget epsilon = do
    st <- get
    let newUsed = epsilonUsed st + epsilon
        total   = epsilonBudget st
    if newUsed > (total + 1e-9) -- small tolerance for floating point drift
        then liftIO $ throwIO (InsufficientBudget total newUsed)
    else put st{epsilonUsed = newUsed}

-- | Get the random generator from the query state
-- This allows noise generation functions to access the shared generator.
getGenerator :: Query GenIO
getGenerator = randomGen <$> get