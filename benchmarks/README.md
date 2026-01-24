# Benchmarks: Python vs R Solver Comparison

This directory contains scripts to verify the R implementation matches the verified-correct Python implementations:

1. **rswjax** - Your JAX-based implementation (faster for large problems)
2. **rsw_original** - The original implementation from Barratt et al. (paper authors)

## Setup

### Python Environment

```bash
# Create a virtual environment
python -m venv venv
source venv/bin/activate  # or venv\Scripts\activate on Windows

# Install dependencies
pip install numpy pandas scipy

# Install rswjax (JAX-based, faster)
pip install jax jaxlib ott-tools
pip install -e ../ai_context/rswjax

# Install original rsw (requires qdldl for sparse LDL factorization)
pip install qdldl
pip install -e ../ai_context/rsw_original
```

Note: If you have trouble installing qdldl, you can still compare against rswjax alone.

### Running Comparisons

1. **Generate test cases and Python results:**
   ```bash
   python generate_test_cases.py
   ```
   This creates `test_cases/` with input data and Python results.

2. **Run R solver on same inputs:**
   ```r
   source("run_r_solver.R")
   ```

3. **Compare results:**
   ```r
   source("compare_results.R")
   ```

## Test Cases

| Case | Description |
|------|-------------|
| 01_basic_equality | Simple categorical, equality loss |
| 02_least_squares | Categorical with L2/least squares loss |
| 03_mixed_losses | Combination of equality and L2 |
| 04_entropy_reg_limit | Entropy regularizer with weight limit |
| 05_zero_reg | Zero regularizer |
| 06_larger_scale | 10k samples, 5 variables |
| 07_interaction | Two-way interaction terms |
| 08_lambda_* | Same problem with different lambda values |

## What Gets Compared

- **Weights**: The final sample weights from each solver
- **Achieved values**: The weighted means (F @ w) achieved by the solution
- **Targets**: Whether the achieved values match the specified targets

A test passes if the max absolute difference between implementations is < 1e-4.
