"""
Main module for the Differential Privacy DSL

This module re-exports all the main classes and functions students will need.
Import from this module in your code:

    from dp import DP_DSL, create_dataset, Adult, Income, Occupation

STUDENT TODO: Once you implement the core classes, this will work automatically.
"""

# Core classes and functions
from core import Dataset, DPResult, InsufficientBudget, InvalidParameter, create_dataset

# Main DSL class
from dp_dsl import DP_DSL, create_dsl

# Adult dataset types and functions
from adult_dataset import (
    Adult, WorkClass, Education, MaritalStatus, Occupation,
    Relationship, Race, Sex, Income, load_adult_dataset
)

# Export all public classes and functions
__all__ = [
    # Core types
    'Dataset', 'DPResult', 'InsufficientBudget', 'InvalidParameter', 'create_dataset',

    # Main DSL
    'DP_DSL', 'create_dsl',

    # Adult dataset
    'Adult', 'WorkClass', 'Education', 'MaritalStatus', 'Occupation',
    'Relationship', 'Race', 'Sex', 'Income', 'load_adult_dataset'
]