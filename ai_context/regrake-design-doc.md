# Regularized Raking Package Design Document
(Package name TBD - seeking something clear and discoverable for survey statisticians)

## Overview
An R package for regularized raking that aims to provide a modern, intuitive interface for survey statisticians while maintaining flexibility and power. The package builds on the mathematical framework from rswjax while providing a more R-idiomatic interface inspired by packages like brms and autumn.

## Core Interface

### Main Function
The primary interface is through the main raking function:

```r
regularized_rake(
  exact(race + educ) + l2(race:educ:income),
  regularizer = "entropy",
  bounds = c(0.2, 5),
  control = list(
    lambda = 1,
    rho = 50,
    eps_abs = 1e-5
  ),
  data = survey_df,
  population = pop_df  # or targets = targets_df
)
```

### Formula Interface
- Loss specifications are combined using `+` operator
- Supported loss types include:
  - `exact()`: Exact matching (equality constraints)
  - `l2()`: L2 loss
  - `kl()`: KL divergence (for continuous variables)
- Variables can be combined using standard R formula operators (`:` for interactions)

### Data Input Types
The package supports four main ways to specify target distributions:

1. Raw population data
```r
regularized_rake(..., data = survey_df, population = pop_df)
```

2. Weighted population data
```r
regularized_rake(..., data = survey_df, population = pop_df, pop_weight = "weight_col")
```

3. Autumn-style targets
```r
regularized_rake(..., data = survey_df, targets = tibble(
  variable = c("race", "educ"),
  level = c("white", "college"),
  proportion = c(0.7, 0.3)
))
```

4. Anesrake-style targets
```r
regularized_rake(..., data = survey_df, targets = list(
  race = c(white = 0.7, black = 0.3),
  educ = c(college = 0.6, hs = 0.4)
))
```

### Continuous Variables
- Continuous variables with KL divergence loss are only supported with population data inputs
- Users needing to specify parametric distributions should pre-process into empirical samples

### Regularization
- Specified via string name: `regularizer = "entropy"`
- Common options include:
  - `"entropy"`: Maximum entropy regularization
  - `"uniform"`: Uniform weights
- Weight bounds specified separately via `bounds = c(min, max)`
- Additional parameters controlled via `control` list

### Helper Functions
```r
# Generate template for target specification
get_target_template(
  exact(race + educ) + l2(race:educ),
  data = df,
  format = c("autumn", "anesrake")
)
```

### Output Structure
By default, when used with pipe operator:
```r
df %>% 
  regularized_rake(exact(race + educ), population = pop_df) 
```
Returns original dataframe with weights appended.

When called directly with `verbose = TRUE`:
```r
fit <- regularized_rake(exact(race + educ), 
           data = df, 
           population = pop_df,
           verbose = TRUE)
```
Returns list containing:
- `weights`: The computed weights
- `fit`: Convergence metrics and achieved vs target differences
- `call`: The original call for reproducibility

## Future Enhancements
1. Additional loss functions and regularizers with flexible specification options
2. Comprehensive print/summary methods for diagnostics
3. Integration helpers for survey/srvyr packages
4. Parameter saving/loading for repeated weighting
5. Additional visualization tools for weight diagnostics
6. More helper functions for common weighting scenarios

## Design Principles
1. Prioritize intuitive interface over excessive flexibility
2. Follow R idioms and formula conventions
3. Provide smart defaults while allowing customization
4. Clear error messages and validation
5. Maintain compatibility with existing survey analysis workflows

## Implementation Notes
- Formula parsing should handle standard R operators
- Error messages should guide users to correct format based on input type
- Validation should occur early in function call
- Performance optimization should focus on large dataset handling