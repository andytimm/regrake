# regrake

An R implementation of optimal representative sample weighting, enabling significantly more flexible raking objectives. Built on the mathematical framework from Barratt et al. (2021) <https://web.stanford.edu/~boyd/papers/pdf/optimal_representative_sampling.pdf>.

🚧 This is a work in progress — it's not yet sufficiently developed to be usable. 🚧

## (Intended) Features
- Intuitive formula interface for specifying raking constraints
- Multiple target specification formats (raw population data, weighted data, direct proportions)
- Various loss functions and regularization methods
- Integration with common survey analysis workflows

## Installation

```r
# Install from GitHub:
remotes::install_github("andytimm/regrake")
```

## Basic Usage

```r
library(regrake)

# Example usage (coming soon)
# df %>%
#   rsw(
#     exact(race + educ) + l2(race:educ:income),
#     regularizer = "entropy",
#     bounds = c(0.2, 5),
#     data = survey_df,
#     population = pop_df
#   )
```

## Status

🚧 This package is under active development. The API is not yet stable and may change. 🚧

## Contributing

Interested in contributing? Please note that this project is released with a Code of Conduct. By contributing to this project, you agree to abide by its terms.

## License

Licensed under Apache License 2.0