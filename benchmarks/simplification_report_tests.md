# Test Code Simplification Report

Code review of tests/testthat/ for simplification opportunities.
Generated February 2025.

## Summary

10 suggestions identified, ~362 lines removable/improvable.

---

## 1. Create tests/testthat/helper.R with shared fixtures (~100 lines saved)

Multiple test files independently create identical test data:
- sex + age sample data (n=500 or n=1000)
- Population target data frames
- Weighted proportion calculation helpers

Files affected: test-diagnostics.R, test-e2e.R, test-bounds.R

**Action:** Create helper.R with:
- `make_sample_sex_age(n = 500)` — standard sample
- `make_pop_sex_age()` — matching targets
- `weighted_props(weights, data, var)` — weighted proportion calculator

---

## 2. Remove commented-out test block in test-e2e.R (61 lines)

Lines 1-61: Entirely commented-out test that references old API (`achieved`, `solution`
fields). Dead code providing no value.

**Action:** Delete lines 1-61.

---

## 3. Remove dead function tests in test-parse-raking-formula.R (~50 lines)

Lines 387-437: Tests for `raking_term()` and `process_term()` — both dead functions.
The test at line 396 creates raking_term objects via the dead constructor and calls
process_term() which is also dead.

**Action:** Delete lines 387-437.

---

## 4. Replace expect_true(x > 0) with expect_gt(x, 0) (~40 instances)

Better failure messages. Applies to:
- `expect_true(x > 0)` → `expect_gt(x, 0)`
- `expect_true(x < y)` → `expect_lt(x, y)`
- `expect_true(x >= y)` → `expect_gte(x, y)`
- `expect_true(x <= y)` → `expect_lte(x, y)`

Spread across most test files.

**Action:** Find-and-replace across test suite.

---

## 5. Extract weighted proportion assertion for test-e2e.R (~10 instances)

Repeated pattern:
```r
weighted_sex <- tapply(result$weights, data$sex, sum)
expect_equal(unname(weighted_sex["M"]), 0.49, tolerance = 0.01)
```

**Action:** Create `expect_weighted_prop()` helper.

---

## 6. Consolidate ADMM setup blocks in test-diagnostics.R and test-solver.R

Repeated setup of similar ADMM problems across multiple test blocks.

**Action:** Extract shared setup into helper functions.

---

## 7-8. Consolidate repeated patterns in test-e2e.R

Several tests create nearly identical data+targets+formula, run regrake, then check
similar assertions.

**Action:** Extract parameterized test helper.

---

## 9. Remove redundant library(testthat) calls

testthat is auto-loaded by the test runner. Explicit calls are no-ops.

**Action:** Remove from all test files.

---

## 10. Standardize assertion patterns

Mix of `expect_equal(x, TRUE)` and `expect_true(x)`. Standardize.

**Action:** Use idiomatic testthat assertions throughout.
