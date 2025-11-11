# Differential Privacy DSL - Lab Assignment

**Course**: Data Privacy
**Lab**: 2 - Implementing a Differential Privacy Domain Specific Language
**Language**: Python 3

## Overview

In this lab, you will implement a Domain Specific Language (DSL) for Differential Privacy in Python. This DSL provides type-safe differential privacy with stability tracking, privacy budget management, and differentially private measurements.

Your implementation will be based on the theoretical foundations of ε-differential privacy and will include rigorous (α,β)-accuracy testing to verify the privacy guarantees.

## Learning Objectives

By completing this lab, you will:

1. **Understand differential privacy mechanisms**: Implement the Laplace mechanism for achieving ε-differential privacy
2. **Master privacy budget composition**: Track and manage privacy budget across multiple queries
3. **Learn stability analysis**: Understand how data transformations affect sensitivity
4. **Implement type-safe abstractions**: Use abstract base classes to enforce proper encapsulation
5. **Verify privacy guarantees**: Use statistical tests to validate theoretical privacy bounds

## Assignment Structure

### What You Need to Implement

1. **Core Dataset Interface** (`core.py`)
   - Complete the abstract `Dataset` class
   - Implement `ConcreteDataset`
   - Implement the `create_dataset()` factory function

2. **DP_DSL Class** (`dp_dsl.py`)
   - Budget management methods
   - Data transformations (filter, map, union, intersect, group_by)
   - DP measurements (count, sum)
   - Utility methods (budget_status, reset_budget, __str__)

3. **Examples** (`examples.py`)
   - Complete examples are provided to show proper DSL usage
   - Run after implementing your DSL to see it in action

### What Is Provided

- **Complete Adult dataset parsing** (`adult_dataset.py`): Focus on DP concepts, not data parsing
- **Noise generation methods**: Proper Laplace mechanism implementation
- **Complete test suite**: All tests provided to guide and validate your implementation
- **(α,β)-accuracy tests**: Statistical verification of your DP guarantees
- **Complete examples**: Full working examples showing proper DSL usage
- **Skeleton code**: Method signatures and detailed TODO comments

## Getting Started

### Prerequisites

```bash
# Install required dependencies
pip install -r requirements.txt
```

### Implementation Order

We recommend implementing in this order:

1. **Start with `core.py`**:
   ```python
   # Complete the Dataset abstract base class
   # Implement ConcreteDataset
   # Implement create_dataset() factory function
   ```

2. **Implement budget management in `dp_dsl.py`**:
   ```python
   # __init__(), get_remaining_budget(), get_used_budget()
   # _consume_budget(), budget_status(), __str__()
   ```

3. **Add transformations**:
   ```python
   # filter(), map() - preserve stability
   # union(), intersect() - amplify stability
   # group_by() - doubles stability
   ```

4. **Implement measurements**:
   ```python
   # count() - uses provided noise generation
   # sum() - with bounded contributions
   ```

5. **Test your implementation**: All tests and examples are provided!

### Testing Your Implementation

```bash
# Run all tests (will fail until you implement the core classes)
python3 test_dp.py

# Run only the (α,β)-accuracy tests (verifies DP guarantees)
python3 test_dp.py --accuracy-only

# Run your completed examples
python3 examples.py
```

**Important**: The tests are complete and show exactly what your implementation should do. You'll see different types of errors as you work:

- **Initially**: `AssertionError` because methods return `None` instead of expected values
- **After basic structure**: Individual test failures showing what each method should return
- **When complete**: All tests pass, including the (α,β)-accuracy tests that verify your DP guarantees

The tests serve as both specification and validation - use them to understand what each method should do!

## Key Concepts to Understand

### 1. Stability and Sensitivity

**Stability** tracks how much one individual can affect results:
- `filter`, `map`: Preserve stability (stability stays the same)
- `union`, `intersect`: Amplify stability (add the stabilities)
- `group_by`: Doubles stability (one person can affect multiple groups)

**Effective Sensitivity** = `dataset.stability × query_sensitivity`

### 2. Privacy Budget Composition

Each DP query consumes privacy budget (epsilon). The DSL must:
- Track total budget allocated
- Track budget consumed by each query
- Prevent queries that would exceed remaining budget
- Support budget reset for multiple experiments

### 3. (α,β)-Accuracy

The provided tests verify that your implementation satisfies:
- α = log(1/β) × (sensitivity/ε)
- At most β fraction of queries have error > α
- Uses empty datasets to test pure noise properties
- Validates theoretical differential privacy guarantees

### 4. Abstract Dataset Interface

The `Dataset` class encapsulates data and prevents direct access:
- Use `dataset._get_data()` only within DP_DSL methods
- Use `len(dataset)` and `for item in dataset` for safe access
- Use `dataset._create_new()` to create transformed datasets

## Implementation Details

### Budget Management Example

```python
def _consume_budget(self, epsilon: float) -> None:
    if epsilon <= 0:
        raise InvalidParameter(f"Invalid epsilon: {epsilon}")

    if self.used_budget + epsilon > self.total_budget:
        remaining = self.get_remaining_budget()
        raise InsufficientBudget(epsilon, remaining)

    self.used_budget += epsilon
```

### Transformation Example

```python
def filter(self, predicate: Callable[[T], bool], dataset: Dataset[T]) -> Dataset[T]:
    # Get data safely using abstract interface
    data = dataset._get_data()
    filtered_data = [x for x in data if predicate(x)]

    # Create new dataset with same stability (filter preserves stability)
    return dataset._create_new(filtered_data, dataset.stability)
```

### Measurement Example

```python
def count(self, dataset: Dataset[T], epsilon: Epsilon) -> DPResult:
    # Consume budget first
    self._consume_budget(epsilon)

    # Calculate effective sensitivity
    stability = dataset.stability
    measurement_sensitivity = 1  # Count has sensitivity 1
    effective_sensitivity = stability * measurement_sensitivity

    # Get true count and add calibrated noise
    true_count = len(dataset)
    noisy_value = self._add_laplace_noise(
        value=float(true_count),
        sensitivity=effective_sensitivity,
        epsilon=epsilon
    )

    return DPResult(dp_value=noisy_value, epsilon_spent=epsilon)
```

## Files Description

### Core Implementation (Your TODO)
- **`core.py`**: Abstract Dataset interface and exceptions
- **`dp_dsl.py`**: Main DP_DSL class implementation
- **`dp.py`**: Module exports (works automatically once you implement core classes)

### Provided Files
- **`adult_dataset.py`**: Complete Adult Census dataset parsing
- **`adult.csv`**: Adult Census dataset
- **`requirements.txt`**: Python dependencies (numpy)

### Examples and Tests (Your TODO)
- **`examples.py`**: Usage examples (complete the TODOs)
- **`test_dp.py`**: Test suite with (α,β)-accuracy verification

## Grading Criteria

Your implementation will be evaluated on:

1. **Correctness** (40%): Core functionality works as specified
2. **Privacy Guarantees** (30%): Passes (α,β)-accuracy tests
3. **Stability Tracking** (20%): Transformations correctly update stability
4. **Code Quality** (10%): Clean implementation following the skeleton structure

## Advanced Features (Optional)

If you finish early, consider implementing:

1. **Partition operation**: Advanced transformation that splits budget across partitions
2. **Additional measurements**: Median, histogram, etc.
3. **Advanced stability analysis**: More sophisticated sensitivity calculations
4. **Composition theorems**: Advanced composition beyond basic composition

## Getting Help

- **Start with the TODOs**: Each file has detailed TODO comments explaining what to implement
- **Use the tests**: The test file shows you exactly what your functions should do
- **Check the examples**: The example usage shows how the DSL should work
- **Understand the theory**: Review differential privacy foundations before implementing

## Submission

Submit your completed implementation with:

1. All TODO sections completed in `core.py`, `dp_dsl.py`, and `examples.py`
2. Basic functionality tests passing
3. (α,β)-accuracy tests passing (this verifies your DP guarantees)
4. Brief writeup explaining any design decisions you made

Good luck! Remember: the goal is to understand differential privacy deeply by implementing it from first principles.

## Academic Integrity

This is an individual assignment. You may discuss concepts and approaches with classmates, but your implementation must be your own work. The skeleton code and theoretical foundations are provided to help you focus on the core differential privacy concepts.