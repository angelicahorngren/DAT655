"""
Core data structures and types for the Differential Privacy DSL

This module provides the foundational classes and types needed for
differential privacy operations, including the abstract Dataset interface,
result types, and custom exceptions.

STUDENT TODO: Complete the implementation of the Dataset abstract base class
and the concrete dataset implementation.
"""

from typing import TypeVar, Generic, List, Iterator, Union
from abc import ABC, abstractmethod

# Type variables for generic programming
T = TypeVar('T')
Epsilon = float  # Type alias for privacy parameter


class InsufficientBudget(Exception):
    """Raised when a DP operation would exceed the available privacy budget"""

    def __init__(self, requested: float, available: float):
        self.requested = requested
        self.available = available
        super().__init__(f"Insufficient budget: requested {requested}, available {available}")


class InvalidParameter(Exception):
    """Raised when invalid parameters are passed to DP operations"""

    def __init__(self, message: str):
        super().__init__(message)


class DPResult:
    """Result of a differentially private computation

    Contains the noisy result value and the epsilon spent for this computation.
    """

    def __init__(self, dp_value: float, epsilon_spent: float):
        self.dp_value = dp_value
        self.epsilon_spent = epsilon_spent

    def __str__(self) -> str:
        return f"DPResult(value={self.dp_value:.3f}, ε={self.epsilon_spent})"

    def __repr__(self) -> str:
        return self.__str__()


class Dataset(Generic[T], ABC):
    """Abstract base class for datasets with stability tracking

    This class provides an abstract interface for datasets that encapsulates
    the data and tracks stability (how much one individual can affect results).

    STUDENT TODO: Complete the implementation of this abstract base class.
    You need to:
    1. Implement the constructor that validates stability >= 1
    2. Implement the abstract methods
    3. Implement the __len__ and __iter__ methods for the interface

    Key concepts:
    - Stability: Integer >= 1 representing the maximum contribution of one individual
    - Data encapsulation: Internal data should not be directly accessible
    - Abstract methods: Subclasses must implement _get_data() and _create_new()
    """

    def __init__(self, data: List[T], stability: int = 1):
        """Initialize dataset with data and stability

        Args:
            data: List of data elements
            stability: Stability parameter (must be >= 1)

        Raises:
            ValueError: If stability < 1
        """
        # TODO: Implement constructor
        # - Validate that stability >= 1
        # - Store a private copy of the data
        # - Store the stability value
        pass

    @property
    def stability(self) -> int:
        """Get the stability of this dataset"""
        # TODO: Return the stability value
        pass

    @abstractmethod
    def _get_data(self) -> List[T]:
        """Get the internal data (for use by DP operations only)

        This method should only be used by DP_DSL methods, not by external code.
        """
        pass

    @abstractmethod
    def _create_new(self, new_data: List[T], new_stability: int) -> 'Dataset[T]':
        """Create a new dataset of the same type with different data/stability

        Args:
            new_data: New data for the dataset
            new_stability: New stability value

        Returns:
            New dataset instance of the same concrete type
        """
        pass

    def __len__(self) -> int:
        """Get the number of elements in the dataset"""
        # TODO: Return the length of the internal data
        pass

    def __iter__(self) -> Iterator[T]:
        """Iterate over the dataset elements"""
        # TODO: Return an iterator over the internal data
        pass


class ConcreteDataset(Dataset[T]):
    """Concrete implementation of the Dataset abstract base class

    STUDENT TODO: Complete this implementation.
    This is a simple concrete implementation that stores data in a list.
    """

    def _get_data(self) -> List[T]:
        """Get the internal data"""
        # TODO: Return the internal data
        pass

    def _create_new(self, new_data: List[T], new_stability: int) -> 'Dataset[T]':
        """Create a new ConcreteDataset with the given data and stability"""
        # TODO: Return a new ConcreteDataset instance
        pass


def create_dataset(data: List[T], stability: int = 1) -> Dataset[T]:
    """Factory function to create a new dataset

    Args:
        data: List of data elements
        stability: Stability parameter (default 1)

    Returns:
        New Dataset instance
    """
    # TODO: Create and return a ConcreteDataset instance
    pass