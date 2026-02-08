# regrake

`regrake` provides an interface for regularized raking in R. This more general 
formulation of the weighting problem, following [Barratt et al. (2021)](https://web.stanford.edu/~boyd/papers/pdf/optimal_representative_sampling.pdf)'s approach, enables more flexible functional forms in adherence to population targets, meaningful regularization, and ultimately more expressive and efficient survey weights.

## Installation

```r
remotes::install_github("andytimm/regrake")
```

## Quick start

```r
library(regrake)

set.seed(605)
n <- 500

sample_data <- data.frame(
  sex = sample(c("F", "M"), n, replace = TRUE, prob = c(0.55, 0.45)),
  age_group = sample(c("18-34", "35-54", "55+"), n, replace = TRUE,
                     prob = c(0.40, 0.35, 0.25)),
  income = rnorm(n, mean = 58000, sd = 14000)
)

# autumn-style target table: variable / level / target
pop_targets <- data.frame(
  variable = c("sex", "sex", "age_group", "age_group", "age_group", "income"),
  level = c("F", "M", "18-34", "35-54", "55+", "mean"),
  target = c(0.51, 0.49, 0.30, 0.40, 0.30, 62000)
)

fit <- regrake(
  data = sample_data,
  formula = ~ rr_exact(sex) + rr_exact(age_group) + rr_mean(income),
  population_data = pop_targets,
  pop_type = "proportions",
  regularizer = "entropy",
  bounds = c(0.3, 3)
)

fit
head(fit$weights)
fit$balance
```

## Formula interface

Constraint helpers include:

- `rr_exact()`: exact matching
- `rr_l2()`: soft least-squares matching
- `rr_kl()`: soft KL matching
- `rr_range()` / `rr_between()`: bounded matching
- `rr_mean()`: continuous mean matching
- `rr_var()`: continuous variance matching
- `rr_quantile(x, p)`: quantile matching

Interactions are supported with `:` (for example `rr_l2(sex:age_group)`).

## Population target formats

`regrake()` supports:

- `pop_type = "proportions"`: autumn-style table with `variable`, `level`, `target`
- `pop_type = "raw"`: one row per population unit
- `pop_type = "weighted"`: population microdata plus a weight column
- `pop_type = "anesrake"`: named list of numeric vectors
- `pop_type = "survey"`: margin/category/value table
- `pop_type = "survey_design"`: `survey.design` object

## Output

A fitted object contains:

- `weights`: calibrated weights (sum to sample size)
- `balance`: achieved vs target values by constraint
- `diagnostics`: convergence and weight-quality diagnostics
- `solution`: solver internals

## For more

- [Barratt et al. (2021)](https://web.stanford.edu/~boyd/papers/pdf/optimal_representative_sampling.pdf):
  overview of the underlying ADMM optimization formulation.
- [NYOSPM regrake materials](https://github.com/andytimm/nyospm_regrake_public):
  broader motivation and practical context for survey weighting.

## Status

The package is under active development and approaching a first stable release.

## License

Apache License 2.0.
