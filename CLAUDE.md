# regrake - Regularized Survey Raking

## What This Is

An R package for regularized survey raking based on Barratt et al. (2021) "Optimal Representative Sample Weighting". The goal is to find optimal weights that make a survey sample match population targets while keeping weights "regular" (not too extreme).

## Project Structure

```
R/                      # Package source
  regrake.R             # Main user-facing function
  solver.R              # ADMM solver implementation
  parse_raking_formula.R # Formula parsing (~ rr_exact(x) + rr_l2(y))
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
- Formula parsing for `rr_exact()`, `rr_l2()`, `rr_kl()`, `rr_mean()`, `rr_var()`, `rr_quantile()`, `rr_range()`/`rr_between()`, and interactions
- Categorical and continuous variable raking
- Continuous variables auto-normalized by target for numerical stability (`normalize=TRUE` default)
- All regularizers: entropy, zero, kl, sum_squares, boolean
- Weighted least squares with `diag_weight` parameter
- **Weight bounds** via `bounds` and `bounds_method` parameters:
  - `bounds = c(0.3, 3)` means weights between 0.3x and 3x the average
  - `bounds_method = "soft"` (default): Fast, uses regularizer clipping, bounds may be slightly violated when targets conflict
  - `bounds_method = "hard"`: Strict enforcement via bounded simplex projection, targets may degrade when bounds conflict
- **Range/inequality constraints** via `rr_range()` or `rr_between()`:
  - `rr_range(sex, 0.02)` - margin mode: each level within ±2% of target
  - `rr_range(sex, c(Female=0.02, Male=0.03))` - named vector for level-specific margins
  - `rr_range(age, 40, 45)` - explicit bounds mode: mean age between 40 and 45
  - Works with categorical variables, continuous variables, and categorical interactions
- **6 population data formats:**
  - `proportions` - "autumn" style (variable, level, target columns)
  - `raw` - unit-level data (computes means & proportions)
  - `weighted` - unit-level data with weights column
  - `anesrake` - list of named numeric vectors
  - `survey` - margin/category/value columns
  - `survey_design` - survey package design objects
- `balance` field: tidy data frame comparing achieved vs target values (columns: constraint, type, variable, level, achieved, target, residual) - ready for plotting/inspection
- **Convergence tolerance** via `margin_tol` in `control`:
  - Default `margin_tol = 1e-4` scales tolerance by problem size for consistent accuracy
  - `control = list(margin_tol = 0.001)` targets ~0.1% max margin error
  - `control = list(eps_abs = 1e-6)` opts out of scaling for raw ADMM control
- R CMD check: 0 errors, 0 warnings, 1 note (unrelated nyosp_regrake folder)
- 705 tests passing

**Known Limitations:**
- Interactions with continuous variables not supported (e.g., `~ rr_mean(age):sex` will error)

**Strict Validation (added January 2025):**
- **NAs:** Always errors if raking variables contain NAs (no silent dropping)
- **Data/Target Mismatch:** Errors if data has levels with no corresponding targets
- **Zero Weights:** Errors if non-boolean regularizer produces zero weights (suggests tolerance too loose, data/target mismatch, or infeasible targets)
- **Target Sum:** Auto-normalizes targets within 5% of 1.0 (warns if 0.1-5% off, errors if >5% off)

## Building & Testing

```r
devtools::load_all()    # Load package
devtools::test()        # Run tests (705 pass)
devtools::check()       # Full R CMD check (0 errors, 0 warnings)
```

## Windows Environment Note

This project is partially developed on Windows with Git Bash (MINGW64). Multi-line `Rscript -e` commands cause segfaults due to newline handling issues between bash and Windows R.

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

For large-scale tests (100K samples), use the `--include-100k` flag:
```bash
python generate_test_cases.py --include-100k
```

## Performance Comparison

Benchmark results comparing R solver to Python implementations (January 2025):

| Scenario | n | m | R | JAX | Original | R/JAX |
|----------|---|---|-----|------|----------|-------|
| Small problems | 1K | 2-8 | ~0.15-0.20s | ~0.5-0.7s | ~0.06-0.1s | **0.3-0.4x** |
| Medium scale | 10K | 10 | ~2.2s | ~1.2s | ~6s | 1.8x |
| High constraints | 2K | 50 | ~0.5s | ~0.9s | ~0.7s | **0.5x** |
| High constraints | 2K | 99 | ~9.6s | ~13.6s | ~8.4s | **0.7x** |

**Key findings:**
- **R beats JAX on small problems** (~1K samples): No JIT warm-up overhead
- **R beats JAX on high-constraint problems**: R handles "wide" problems well
- **R beats Original at scale**: Original rsw has O(n²) scaling issues
- **JAX wins on large-n problems** (10K+): JIT compilation amortizes well

**Optimizations applied:**
- Pre-allocate f vector in ADMM loop (~25% improvement on 10K)
- Compute norms directly without vector concatenation (~3-14% improvement)
- **Consolidate mat-vec products**: Compute `F %*% w` once per iteration and index results instead of repeated `F[idx,] %*% w` calls. This is 20% faster for narrow problems, **2-4x faster for wide problems** (many constraints).

**What didn't help:**
- `as.numeric()` vs `drop(as.matrix())` (made things slower)
- Lazy convergence checking (minimal benefit after norm optimization)
- **Rcpp for projections/norms**: Explored but not worth it. The simplex projection and norm computation are dominated by R's already-optimized vectorized operations (sort, exp, sqrt). Rcpp call overhead negates algorithmic gains, and the remaining bottlenecks (Lambert W, sort) are already C underneath.

**What's not easily portable from JAX:**
- JIT compilation
- JAX-specific ops (`lax.top_k`, `stop_gradient`)

For typical survey raking problems (< 10K samples, reasonable constraint counts), the R solver is competitive with or faster than JAX.

## Performance vs IPF (autumn/anesrake)

ADMM is inherently slower than IPF for simple categorical raking:
- **Per-iteration cost**: ~6x more expensive (O(n*m) vs O(n))
- **Iteration count**: ~7x more iterations at default tolerance
- **Combined**: ~42x slower for typical problems

**Convergence tolerance scaling**: Standard ADMM convergence uses `eps_pri = sqrt(p) * eps_abs + ...` where `p = m + 2n`. This means raw `eps_abs` has different effective meaning at different problem sizes — at n=5000 with eps=1e-4, the solver can exit after 2 iterations with 7% margin error.

**Solution**: `margin_tol` (default 1e-4) scales tolerance internally:
```r
eps = margin_tol / sqrt(m + 2*n)
```
This provides consistent margin accuracy regardless of n or m. Validated across n=200..10000 and m=5..31 — achieved error is consistently ~1% of margin_tol.

## Future Priorities

**High priority (documentation & examples):**
1. **Translate NYOSPM talk to regrake** - Create a longer tutorial/presentation using this package for regularized raking (statistical audience)
2. **Write introductory vignette** - Shorter getting-started guide with real survey weighting workflow examples
3. **Improve README** - Add usage examples, installation instructions

**Medium priority (polish & fixes):**
4. **Code review pass** - Simplification, correctness verification, better documentation of internals

**Lower priority (ecosystem):**
5. **Create NEWS.md** - Track changes for releases
6. **CRAN prep** - If that's a goal

**Completed (January 2025):**
- ✅ Loss/regularizer parity with Python (BooleanRegularizer, weighted least squares)
- ✅ All 6 population data formats implemented and tested
- ✅ Continuous variable constraints (`rr_mean()`, `rr_var()`, `rr_quantile()`)
- ✅ R CMD check clean (0 errors, 0 warnings, 0 notes)
- ✅ Code formatted with air 0.8.1
- ✅ Fix `survey_design` format - now uses formula_spec instead of requiring terms component
- ✅ Simplified result structure - replaced `achieved`/`targets` lists with single `balance` data frame for easy plotting/inspection
- ✅ Weight bounds support (`bounds` + `bounds_method` parameters) with soft/hard enforcement
- ✅ Optimized bounded simplex projection using sorting-based algorithm (~1.5x faster than bisection)
- ✅ Fix target-to-design-matrix ordering bug (targets now correctly matched to alphabetically-ordered factor levels)
- ✅ Optimized ADMM loop mat-vec products (20% faster narrow, 2-4x faster wide problems)
- ✅ Explored Rcpp optimization (not worth it - see "What didn't help" above)

**Completed (February 2025):**
- ✅ Range/inequality constraints via `rr_range()` / `rr_between()` - soft bounds on weighted statistics
- ✅ `margin_tol` convergence parameter - size-invariant tolerance via `eps = margin_tol / sqrt(m + 2n)`
- ✅ Improved zero-weight error message with actionable causes
- ✅ ADMM vs IPF (autumn/anesrake) performance benchmarks

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
parse_raking_formula(~ rr_exact(sex) + rr_l2(age))

# Basic raking example
result <- regrake(
  data = sample_data,
  formula = ~ rr_exact(sex) + rr_mean(age),
  population_data = pop_targets,
  pop_type = "proportions"
)

# Range constraint example (soft bounds)
result <- regrake(
  data = sample_data,
  formula = ~ rr_range(sex, 0.02) + rr_range(age, 38, 42),  # sex ±2%, age between 38-42
  population_data = pop_targets,
  pop_type = "proportions"
)
```
