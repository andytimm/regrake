# Shared test fixtures
# Automatically sourced by testthat before running tests.

#' Generate standard sex + age sample data
#' @param n Sample size
#' @param seed Random seed
make_sample_sex_age <- function(n = 500, seed = 42) {
  set.seed(seed)
  data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    age = sample(
      c("18-34", "35-54", "55+"),
      n,
      replace = TRUE,
      prob = c(0.5, 0.3, 0.2)
    )
  )
}

#' Standard sex + age population targets (autumn format)
make_pop_sex_age <- function() {
  data.frame(
    variable = c(rep("sex", 2), rep("age", 3)),
    level = c("M", "F", "18-34", "35-54", "55+"),
    proportion = c(0.49, 0.51, 0.3, 0.4, 0.3)
  )
}

#' Compute weighted proportions for a categorical variable
#' @param weights Weight vector
#' @param data Data frame
#' @param var Variable name (string)
#' @return Named numeric vector of weighted proportions
weighted_props <- function(weights, data, var) {
  w <- weights / sum(weights)
  tapply(w, data[[var]], sum)
}
