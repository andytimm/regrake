# regrake - Regularized Survey Raking

## What This Is

An R package for regularized survey raking based on Barratt et al. (2021) "Optimal Representative Sample Weighting". The goal is to find optimal weights that make a survey sample match population targets while keeping weights "regular" (not too extreme).

## Project Structure

```
R/                      # Package source
  regrake.R             # Main user-facing function
  solver.R              # ADMM solver implementation
  parse_raking_formula.R # Formula parsing (~ exact(x) + l2(y))
  compute_target_values.R # Convert population data to targets
  construct_admm_inputs.R # Build design matrix and losses for ADMM
  losses.R              # Loss functions (equality, l2, kl, inequality)
  regularizers.R        # Regularizers (entropy, zero, kl)

tests/testthat/         # Unit and integration tests

ai_context/             # Reference materials
  rswjax/               # Your JAX-based Python implementation
  rsw_original/         # Original implementation from paper authors
  *.md                  # Design docs, TODOs, inspiration

benchmarks/             # Python-R comparison scripts (build-ignored)
  generate_test_cases.py  # Creates test inputs + runs Python solvers
  run_r_solver.R          # Runs R solver on same inputs
  compare_results.R       # Compares R vs Python results
  test_cases/             # Generated test data and results
```

## The Pipeline

```
formula → parse_raking_formula() → formula_spec
                                        ↓
population_data → compute_target_values() → target_values
                                        ↓
data + formula_spec + target_values → construct_admm_inputs() → design_matrix + losses
                                        ↓
                                    admm() → weights
```

## Current Status

**Working:**
- ADMM solver (core optimization)
- Formula parsing for `exact()`, `l2()`, and interactions
- Basic categorical variable raking
- Entropy and zero regularizers
- "Proportions" format for population targets

**Known Bugs:**
1. **Continuous variables not supported**: `process_raw_data()` skips numeric columns. Need to extend target computation to handle means for continuous variables.

## Building & Testing

```r
devtools::load_all()    # Load package
devtools::test()        # Run tests (265 pass, 1 flaky perf test)
devtools::check()       # Full R CMD check
```

## Windows Environment Note

This project is developed on Windows with Git Bash (MINGW64). Multi-line `Rscript -e` commands cause segfaults due to newline handling issues between bash and Windows R.

**Workarounds:**
- Use semicolons: `Rscript -e "x <- 1; y <- 2; print(x+y)"`
- Use multiple -e flags: `Rscript -e "x <- 1" -e "print(x)"`
- Write to a temp .R file for complex scripts

## Key Design Decisions

- Formula interface inspired by brms (composable with `+`)
- All population formats convert to "autumn" format internally (variable, level, proportion)
- Uses sparse matrices (Matrix package) for efficiency
- ADMM solver, not CVXR/other convex optimization packages

## Python Reference Implementations

Two Python implementations are available in `ai_context/`:

1. **rswjax/** - Your JAX-based implementation (faster for large problems)
2. **rsw_original/** - Original implementation from Barratt et al. (paper authors)

Key files in both:
- `solver.py` - ADMM solver
- `losses.py` - Loss functions
- `regularizers.py` - Regularizers

## Benchmarking Against Python

The `benchmarks/` directory has scripts to verify the R solver matches Python:

```bash
# 1. Set up Python environment (see benchmarks/README.md)
cd benchmarks

# 2. Generate test cases and run Python solvers
python generate_test_cases.py

# 3. Run R solver on same inputs
Rscript run_r_solver.R

# 4. Compare results
Rscript -e "source('compare_results.R'); main()"
```

The R solver currently passes all test cases (max diff < 1e-6 vs targets).

## Adding New Features

When implementing new loss functions, regularizers, or solver features:

1. **Check Python first**: Look at the implementation in `ai_context/rswjax/` or `ai_context/rsw_original/` to understand the expected behavior
2. **Add a benchmark test case**: Add a new test case to `benchmarks/generate_test_cases.py` that exercises the feature
3. **Verify against Python**: Run the benchmark comparison to ensure R matches Python output
4. **Add unit tests**: Add tests in `tests/testthat/` for the new functionality

This workflow ensures mathematical correctness by comparing against verified implementations.

## Useful Commands

```r
# Test specific file
devtools::test(filter = "solver")

# Run e2e test
devtools::test(filter = "e2e")

# Check a specific function
parse_raking_formula(~ exact(sex) + l2(age))
```
