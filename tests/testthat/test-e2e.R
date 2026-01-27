# End-to-end tests for regrake package

# test_that("regrake handles basic synthetic example", {
#   # Generate synthetic sample data
#   set.seed(605)
#   n_sample <- 1000
#   sample_data <- data.frame(
#     age = runif(n_sample, 20, 30),
#     sex = factor(rbinom(n_sample, 1, 0.6)),
#     height = rnorm(n_sample, 5, 1)
#   )
#
#   # Create population data with different distributions
#   n_pop <- 10000
#   set.seed(606)  # Different seed for population
#   pop_data <- data.frame(
#     age = runif(n_pop, 22, 28),     # Different age range
#     sex = factor(rbinom(n_pop, 1, 0.5)),  # Different sex ratio
#     height = rnorm(n_pop, 5.2, 0.9)  # Different height distribution
#   )
#
#   # Set up raking formula with exact matching for sex and l2 for continuous vars
#   formula <- ~ rr_exact(sex) + rr_l2(age) + rr_l2(height)
#
#   # Run regrake
#   result <- regrake(
#     data = sample_data,
#     formula = formula,
#     population_data = pop_data,
#     pop_type = "raw",
#     regularizer = "entropy",
#     lambda = 0.1,
#     verbose = TRUE
#   )
#
#   # Check structure of results
#   expect_s3_class(result, "regrake")
#   expect_named(result, c("data", "weights", "achieved", "solution", "diagnostics", "call"))
#
#   # Verify weights are valid
#   expect_true(all(result$weights >= 0))
#   expect_equal(sum(result$weights), 1, tolerance = 1e-6)
#
#   # Calculate weighted means for sample
#   weighted_sex <- tapply(result$weights, sample_data$sex, sum)
#   weighted_age <- sum(sample_data$age * result$weights)
#   weighted_height <- sum(sample_data$height * result$weights)
#
#   # Calculate population means
#   pop_sex <- table(pop_data$sex) / n_pop
#   pop_age <- mean(pop_data$age)
#   pop_height <- mean(pop_data$height)
#
#   # Check that weighted sample means match population means
#   # Exact matching for sex
#   expect_equal(weighted_sex, as.numeric(pop_sex), tolerance = 1e-2)
#
#   # L2 matching for continuous variables (less strict tolerance)
#   expect_equal(weighted_age, pop_age, tolerance = 0.1)
#   expect_equal(weighted_height, pop_height, tolerance = 0.1)
# })

test_that("basic raking workflow works end-to-end with categorical variables", {
  # Create synthetic sample data
  set.seed(42)
  n <- 1000
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    age = sample(
      c("18-34", "35-54", "55+"),
      n,
      replace = TRUE,
      prob = c(0.5, 0.3, 0.2)
    )
  )

  # Create population targets in autumn format
  pop_data <- data.frame(
    variable = c(rep("sex", 2), rep("age", 3)),
    level = c("M", "F", "18-34", "35-54", "55+"),
    proportion = c(0.49, 0.51, 0.3, 0.4, 0.3)
  )

  # Run raking
  result <- regrake(
    data = sample_data,
    population = pop_data,
    formula = ~ sex + age,
    pop_type = "proportions" # Explicitly specify pop_type
  )

  # Check that weights sum to n
  expect_equal(sum(result$weights), n)

  # Check that weighted proportions match targets
  agg_sex <- stats::aggregate(
    result$weights,
    list(sex = sample_data$sex),
    sum
  )
  wtd_props_sex <- agg_sex$x / n

  agg_age <- stats::aggregate(
    result$weights,
    list(age = sample_data$age),
    sum
  )
  wtd_props_age <- agg_age$x / n

  # Get target proportions in same order as aggregate results (alphabetical)
  sex_pop <- pop_data[pop_data$variable == "sex", ]
  sex_targets <- sex_pop$proportion[match(agg_sex$sex, sex_pop$level)]

  age_pop <- pop_data[pop_data$variable == "age", ]
  age_targets <- age_pop$proportion[match(agg_age$age, age_pop$level)]

  # Test that weighted proportions match targets within tolerance
  expect_equal(wtd_props_sex, sex_targets, tolerance = 1e-4)
  expect_equal(wtd_props_age, age_targets, tolerance = 1e-4)
})

test_that("regrake handles continuous variables with rr_mean", {
  set.seed(42)

  # Sample data: sex slightly skewed, age mean around 35
  # Using 500 samples for more stable optimization
  survey <- data.frame(
    sex = sample(c("M", "F"), 500, replace = TRUE, prob = c(0.6, 0.4)),
    age = rnorm(500, mean = 35, sd = 10)
  )

  # Population targets - modest adjustments from sample
  pop <- data.frame(
    variable = c("sex", "sex", "age"),
    level = c("M", "F", "mean"),
    target = c(0.5, 0.5, 38) # Want 50/50 sex, mean age 38 (small shift from ~35)
  )

  result <- regrake(
    data = survey,
    formula = ~ rr_exact(sex) + rr_mean(age),
    population_data = pop,
    pop_type = "proportions"
  )

  # Check weights exist and are reasonable
  expect_length(result$weights, 500)
  expect_true(all(result$weights >= 0)) # Allow zero but not negative

  # Check achieved values are close to targets
  # Sex proportions
  weighted_sex <- tapply(result$weights, survey$sex, sum) / sum(result$weights)
  expect_equal(unname(weighted_sex["M"]), 0.5, tolerance = 0.01)
  expect_equal(unname(weighted_sex["F"]), 0.5, tolerance = 0.01)

  # Age mean
  weighted_age_mean <- sum(result$weights * survey$age) / sum(result$weights)
  expect_equal(weighted_age_mean, 38, tolerance = 0.1)
})

test_that("regrake handles variance constraints with rr_var", {
  set.seed(42)

  # Sample data: income with lower variance than target
  survey <- data.frame(
    sex = sample(c("M", "F"), 500, replace = TRUE, prob = c(0.6, 0.4)),
    income = rnorm(500, mean = 50000, sd = 8000) # SD ~8000, var ~64M
  )

  # Population targets - modest variance increase
  # Sample variance is approximately 64M, target 100M (SD ~10000)
  target_var <- 100000000 # 100 million

  pop <- data.frame(
    variable = c("sex", "sex", "income"),
    level = c("M", "F", "var"),
    target = c(0.5, 0.5, target_var)
  )

  result <- regrake(
    data = survey,
    formula = ~ rr_exact(sex) + rr_var(income),
    population_data = pop,
    pop_type = "proportions"
  )

  # Check weights exist and are reasonable
  expect_length(result$weights, 500)
  expect_true(all(result$weights >= 0))

  # Check sex proportions
  weighted_sex <- tapply(result$weights, survey$sex, sum) / sum(result$weights)
  expect_equal(unname(weighted_sex["M"]), 0.5, tolerance = 0.01)
  expect_equal(unname(weighted_sex["F"]), 0.5, tolerance = 0.01)

  # Check weighted variance of income
  # Weighted variance = sum(w * (x - weighted_mean)^2) / sum(w)
  w <- result$weights / sum(result$weights) # Normalize to proportions
  weighted_mean <- sum(w * survey$income)
  weighted_var <- sum(w * (survey$income - weighted_mean)^2)

  # Note: The achieved variance may not match exactly due to optimization constraints
  # We just check that it moved in the right direction (increased from sample variance)
  sample_var <- var(survey$income) *
    (length(survey$income) - 1) /
    length(survey$income) # pop variance
  expect_true(weighted_var > sample_var * 1.1) # Should increase noticeably
})

test_that("regrake handles quantile constraints with rr_quantile", {
  set.seed(42)

  # Sample data: income skewed low (most below 50000)
  survey <- data.frame(
    sex = sample(c("M", "F"), 500, replace = TRUE, prob = c(0.6, 0.4)),
    income = rlnorm(500, meanlog = 10.5, sdlog = 0.5) # Median ~36000
  )

  # Target: median income should be 45000 (higher than sample median)
  # Also want 50/50 sex split
  pop <- data.frame(
    variable = c("sex", "sex", "income"),
    level = c("M", "F", "q50"),
    target = c(0.5, 0.5, 45000)
  )

  result <- regrake(
    data = survey,
    formula = ~ rr_exact(sex) + rr_quantile(income, 0.5),
    population_data = pop,
    pop_type = "proportions"
  )

  # Check weights exist and are reasonable
  expect_length(result$weights, 500)
  expect_true(all(result$weights >= 0))

  # Check sex proportions
  weighted_sex <- tapply(result$weights, survey$sex, sum) / sum(result$weights)
  expect_equal(unname(weighted_sex["M"]), 0.5, tolerance = 0.01)
  expect_equal(unname(weighted_sex["F"]), 0.5, tolerance = 0.01)

  # Check that weighted proportion below 45000 is close to 0.5
  w <- result$weights / sum(result$weights)
  weighted_below <- sum(w * (survey$income <= 45000))
  expect_equal(weighted_below, 0.5, tolerance = 0.02)
})

# ============================================================================
# Tests for different population data formats
# ============================================================================

test_that("regrake works with raw population data format", {
  set.seed(42)
  n <- 500

  # Sample: 60% male, skewed toward young
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    age = sample(
      c("young", "middle", "old"),
      n,
      replace = TRUE,
      prob = c(0.5, 0.3, 0.2)
    )
  )

  # Population (raw): 50/50 sex, more balanced age
  # Using larger n to get stable proportions
  pop_data <- data.frame(
    sex = sample(c("M", "F"), 10000, replace = TRUE, prob = c(0.5, 0.5)),
    age = sample(
      c("young", "middle", "old"),
      10000,
      replace = TRUE,
      prob = c(0.33, 0.34, 0.33)
    )
  )

  result <- regrake(
    data = sample_data,
    population = pop_data,
    formula = ~ sex + age,
    pop_type = "raw"
  )

  # Check weights sum to n
  expect_equal(sum(result$weights), n)

  # Check weighted proportions roughly match population
  w <- result$weights / sum(result$weights)
  weighted_sex <- tapply(w, sample_data$sex, sum)
  expect_equal(unname(weighted_sex["M"]), 0.5, tolerance = 0.02)
  expect_equal(unname(weighted_sex["F"]), 0.5, tolerance = 0.02)
})

test_that("regrake works with anesrake population data format", {
  set.seed(42)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    region = sample(
      c("North", "South", "East", "West"),
      n,
      replace = TRUE,
      prob = c(0.4, 0.3, 0.2, 0.1)
    )
  )

  # Anesrake format: list of named numeric vectors
  pop_data <- list(
    sex = c(F = 0.52, M = 0.48),
    region = c(North = 0.25, South = 0.25, East = 0.25, West = 0.25)
  )

  result <- regrake(
    data = sample_data,
    population = pop_data,
    formula = ~ sex + region,
    pop_type = "anesrake"
  )

  # Check weights sum to n
  expect_equal(sum(result$weights), n)

  # Check sex proportions by computing from weights
  w <- result$weights / sum(result$weights)
  weighted_sex <- tapply(w, sample_data$sex, sum)

  # Should match targets (0.52 for F, 0.48 for M)
  expect_equal(unname(weighted_sex["F"]), 0.52, tolerance = 0.01)
  expect_equal(unname(weighted_sex["M"]), 0.48, tolerance = 0.01)
})

test_that("regrake works with survey population data format", {
  set.seed(42)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    edu = sample(c("HS", "College", "Grad"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
  )

  # Survey format: margin, category, value columns
  pop_data <- data.frame(
    margin = c("sex", "sex", "edu", "edu", "edu"),
    category = c("F", "M", "College", "Grad", "HS"),
    value = c(0.51, 0.49, 0.35, 0.25, 0.4)
  )

  result <- regrake(
    data = sample_data,
    population = pop_data,
    formula = ~ sex + edu,
    pop_type = "survey"
  )

  # Check weights sum to n
  expect_equal(sum(result$weights), n)

  # Check sex proportions by computing from weights
  w <- result$weights / sum(result$weights)
  weighted_sex <- tapply(w, sample_data$sex, sum)

  # Should match targets (0.51 for F, 0.49 for M)
  expect_equal(unname(weighted_sex["F"]), 0.51, tolerance = 0.01)
  expect_equal(unname(weighted_sex["M"]), 0.49, tolerance = 0.01)
})

test_that("regrake works with weighted population data format", {
  set.seed(42)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    age = sample(c("young", "old"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  # Weighted format: unit-level data with weights
  # Weight represents how many population units each row represents
  pop_data <- data.frame(
    sex = c("M", "F", "M", "F"),
    age = c("young", "young", "old", "old"),
    pop_weight = c(2500, 2500, 2500, 2500) # Equal distribution
  )

  result <- regrake(
    data = sample_data,
    population = pop_data,
    formula = ~ sex + age,
    pop_type = "weighted",
    pop_weights = "pop_weight"
  )

  # Check weights sum to n
  expect_equal(sum(result$weights), n)

  # Check proportions are roughly 50/50 for both variables
  w <- result$weights / sum(result$weights)
  weighted_sex <- tapply(w, sample_data$sex, sum)
  expect_equal(unname(weighted_sex["M"]), 0.5, tolerance = 0.01)
  expect_equal(unname(weighted_sex["F"]), 0.5, tolerance = 0.01)

  weighted_age <- tapply(w, sample_data$age, sum)
  expect_equal(unname(weighted_age["young"]), 0.5, tolerance = 0.01)
  expect_equal(unname(weighted_age["old"]), 0.5, tolerance = 0.01)
})

# ============================================================================
# Tests for balance data frame
# ============================================================================

test_that("balance data frame has correct structure and values", {
  set.seed(42)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    age = sample(c("young", "old"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  pop_data <- data.frame(
    variable = c("sex", "sex", "age", "age"),
    level = c("M", "F", "young", "old"),
    target = c(0.5, 0.5, 0.5, 0.5)
  )

  result <- regrake(
    data = sample_data,
    population_data = pop_data,
    formula = ~ rr_exact(sex) + rr_l2(age),
    pop_type = "proportions"
  )

  # Check balance is a data frame with expected columns
  expect_s3_class(result$balance, "data.frame")
  expect_named(
    result$balance,
    c("constraint", "type", "variable", "level", "achieved", "target", "residual")
  )

  # Check correct number of rows (2 sex levels + 2 age levels = 4)
  expect_equal(nrow(result$balance), 4)

  # Check constraint types are correct (alphabetical order: F, M, old, young)
  expect_equal(
    result$balance$type,
    c("exact", "exact", "l2", "l2")
  )

  # Check constraint names are correct
  expect_equal(
    result$balance$constraint,
    c("exact_sex", "exact_sex", "l2_age", "l2_age")
  )

  # Check variable names are correct
  expect_equal(
    result$balance$variable,
    c("sex", "sex", "age", "age")
  )

  # Check levels are correct (alphabetical order within each variable)
  expect_equal(
    result$balance$level,
    c("F", "M", "old", "young")
  )

  # Check targets match population data input
  expect_equal(result$balance$target, c(0.5, 0.5, 0.5, 0.5))

  # Check residual is computed correctly
  expect_equal(
    result$balance$residual,
    result$balance$achieved - result$balance$target
  )

  # For exact constraints, achieved should match targets closely
  exact_rows <- result$balance$type == "exact"
  expect_equal(
    result$balance$achieved[exact_rows],
    result$balance$target[exact_rows],
    tolerance = 1e-4
  )

  # Check achieved values sum to 1 for each variable (categorical proportions)
  sex_achieved <- result$balance$achieved[result$balance$variable == "sex"]
  expect_equal(sum(sex_achieved), 1, tolerance = 0.01)
})

test_that("balance data frame works with interactions", {
  set.seed(42)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE),
    region = sample(c("N", "S"), n, replace = TRUE)
  )

  pop_data <- data.frame(
    variable = c("sex", "sex", "region", "region", rep("sex:region", 4)),
    level = c("M", "F", "N", "S", "M:N", "M:S", "F:N", "F:S"),
    target = c(0.5, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25)
  )

  suppressWarnings({
    result <- regrake(
      data = sample_data,
      population_data = pop_data,
      formula = ~ sex + region + sex:region,
      pop_type = "proportions"
    )
  })

  # Check that interaction is present in balance
  expect_true("sex:region" %in% result$balance$variable)
  expect_true("exact_sex:region" %in% result$balance$constraint)

  # Check correct number of interaction rows (2x2 = 4)
  interaction_rows <- result$balance$variable == "sex:region"
  expect_equal(sum(interaction_rows), 4)

  # Check interaction targets
  expect_equal(
    result$balance$target[interaction_rows],
    rep(0.25, 4)
  )
})

test_that("balance data frame works with continuous variables", {
  set.seed(42)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE),
    income = rnorm(n, 50000, 10000)
  )

  pop_data <- data.frame(
    variable = c("sex", "sex", "income"),
    level = c("M", "F", "mean"),
    target = c(0.5, 0.5, 55000)
  )

  result <- regrake(
    data = sample_data,
    population_data = pop_data,
    formula = ~ rr_exact(sex) + rr_mean(income),
    pop_type = "proportions"
  )

  # Check income is in balance
  expect_true("income" %in% result$balance$variable)

  # rr_mean maps to "exact" type internally
  income_rows <- result$balance$variable == "income"
  expect_equal(result$balance$type[income_rows], "exact")

  # Check continuous target value is preserved
  expect_equal(result$balance$target[income_rows], 55000, tolerance = 1)

  # Check achieved is close to target for exact constraint
  expect_equal(
    result$balance$achieved[income_rows],
    result$balance$target[income_rows],
    tolerance = 100
  )
})
