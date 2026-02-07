# Source Code Simplification Report

Code review of R/ source files for simplification opportunities.
Generated February 2025.

## Summary

13 suggestions identified, ~590 lines removable.

---

## 1. Dead code in parse_raking_formula.R (~450 lines) — HIGH PRIORITY

**Lines 577-1034** contain legacy component-based interface code that is never called
by the active pipeline. The active code uses `parse_raking_formula()` which creates
`raking_term` objects directly via `create_exact_term()`, `create_constraint_term()`, etc.

Dead functions:
- `validate_inputs()` — **name collision with the real one in regrake.R!**
- `validate_variable()` — only called by dead validate_inputs
- `extract_additions()` — stub, never called
- `raking_term()` — constructor, but active code doesn't use it
- `rf()` — convenience wrapper for raking_term()
- `process_term()` — never called in active pipeline
- `print.raking_term()` — references `x$formula` which doesn't exist on active objects
- `print.raking_spec()` — no code creates "raking_spec" class objects
- `formula2str()` — only called by dead print.raking_term
- `raking_constraints` environment + `register_constraint()` + `get_constraint_handler()`
  + 3 registered handlers — unused registry system

Both `raking_term()` and `rf()` are exported in NAMESPACE but unused.

**Action:** Delete lines 577-1034, remove from NAMESPACE via devtools::document().

---

## 2. Duplicated normalization logic in compute_target_values.R (~80 lines)

Four functions contain near-identical target sum normalization blocks:
- `process_anesrake_data()`
- `process_survey_data()`
- `process_survey_design_data()`
- `compute_target_values()`

Each checks if proportions sum to ~1, warns if 0.1-5% off, errors if >5% off.

**Action:** Extract `normalize_target_sum()` helper, replace 4 instances.

---

## 3. No-op conditional in compute_target_values.R (~15 lines)

Around lines 379-394: if/else branches produce identical results.

**Action:** Remove conditional, keep single code path.

---

## 4. No-op conditional in construct_admm_inputs.R (~5 lines)

Around lines 317-321: `needed_vars` if/else with same result in both branches.

**Action:** Simplify to single expression.

---

## 5. Dead fallback in regrake.R (~5 lines)

Around lines 390-394: `original_target` fallback that can never trigger.

**Action:** Remove dead branch.

---

## 6. Consolidate continuous normalization in construct_admm_inputs.R (~25 lines)

Lines 219-298: Three sub-cases (variance, range, mean) contain identical normalization
blocks. Can restructure to apply normalization once after type-specific value computation.

**Action:** Consolidate into single normalization pass.

---

## 7. sapply() → vapply() in compute_target_values.R (2 instances)

`sapply()` returns unpredictable types. Should use `vapply()` with explicit return type.

**Action:** Replace with vapply().

---

## 8. Extract term_variable_name() helper

Variable name extraction from terms is done similarly in 3 files.

**Action:** Create small helper function.

---

## 9. Remove redundant column validation in compute_target_values.R

Column existence check that's already guaranteed by earlier validation.

**Action:** Remove redundant check.

---

## 10-13. Minor consistency improvements

- Duplicate variable validation patterns
- Unused imports
- Slightly verbose conditionals

**Action:** Address opportunistically.

---

## Notes

- **Skip modifyList()**: The current `ctrl[names(control)] <- control` pattern correctly
  preserves NULL values. `modifyList()` would remove NULL entries, breaking
  `margin_tol = NULL` opt-out behavior.
