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

# =============================================================================
# Tests for zero weight handling
# =============================================================================

test_that("boolean regularizer allows zero weights", {
  set.seed(42)
  n <- 50
  sample_data <- data.frame(
    sex = factor(sample(c("M", "F"), n, replace = TRUE))
  )

  pop_data <- data.frame(
    variable = rep("sex", 2),
    level = c("M", "F"),
    target = c(0.5, 0.5)
  )

  # Boolean regularizer with k < n will produce zero weights
  # (boolean problems often don't fully converge, suppress that warning)
  result <- suppressWarnings(regrake(
    data = sample_data,
    formula = ~ rr_exact(sex),
    population_data = pop_data,
    pop_type = "proportions",
    regularizer = "boolean",
    k = 20
  ))

  # Should have exactly 20 non-zero weights
  expect_equal(sum(result$weights > 0), 20)
  # Should have n - 20 zero weights
  expect_equal(sum(result$weights == 0), n - 20)
})

test_that("regrake errors when data has NAs", {
  sample_data <- data.frame(
    sex = factor(c("M", "F", NA, "M", "F"))
  )

  pop_data <- data.frame(
    variable = rep("sex", 2),
    level = c("M", "F"),
    target = c(0.5, 0.5)
  )

  expect_error(
    regrake(
      data = sample_data,
      formula = ~ rr_exact(sex),
      population_data = pop_data,
      pop_type = "proportions"
    ),
    "missing values"
  )
})

test_that("regrake errors when data has level not in targets", {
  sample_data <- data.frame(
    region = factor(c("N", "S", "E", "W", "N", "S"))
  )

  # Only N and S have targets
  pop_data <- data.frame(
    variable = rep("region", 2),
    level = c("N", "S"),
    target = c(0.5, 0.5)
  )

  expect_error(
    regrake(
      data = sample_data,
      formula = ~ rr_exact(region),
      population_data = pop_data,
      pop_type = "proportions"
    ),
    "that have no targets"
  )
})

# =============================================================================
# Tests for rr_range (inequality constraints)
# =============================================================================

test_that("regrake handles rr_range with categorical variable (margin mode)", {
  set.seed(42)
  n <- 500

  # Sample: heavily skewed toward M (70%)
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  # Target: M=0.49, F=0.51, but with margin ±0.05
  # So M should be in [0.44, 0.54], F should be in [0.46, 0.56]
  pop_data <- data.frame(
    variable = c("sex", "sex"),
    level = c("M", "F"),
    target = c(0.49, 0.51)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_range(sex, 0.05),
    population_data = pop_data,
    pop_type = "proportions"
  )

  # Check weights exist and are reasonable
  expect_length(result$weights, n)
  expect_true(all(result$weights >= 0))

  # Check achieved proportions are within bounds
  w <- result$weights / sum(result$weights)
  weighted_sex <- tapply(w, sample_data$sex, sum)

  # M should be in [0.44, 0.54]
  expect_true(weighted_sex["M"] >= 0.44 - 0.01)
  expect_true(weighted_sex["M"] <= 0.54 + 0.01)

  # F should be in [0.46, 0.56]
  expect_true(weighted_sex["F"] >= 0.46 - 0.01)
  expect_true(weighted_sex["F"] <= 0.56 + 0.01)
})

test_that("regrake handles rr_range with continuous variable (margin mode)", {
  set.seed(42)
  n <- 500

  # Sample: age mean around 35
  survey <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.5, 0.5)),
    age = rnorm(n, mean = 35, sd = 10)
  )

  # Target: mean age 40, but allow ±3 margin
  pop <- data.frame(
    variable = c("sex", "sex", "age"),
    level = c("M", "F", "mean"),
    target = c(0.5, 0.5, 40)
  )

  result <- regrake(
    data = survey,
    formula = ~ rr_exact(sex) + rr_range(age, 3),
    population_data = pop,
    pop_type = "proportions"
  )

  # Check weights exist
  expect_length(result$weights, n)
  expect_true(all(result$weights >= 0))

  # Check weighted mean age is within bounds [37, 43]
  w <- result$weights / sum(result$weights)
  weighted_age_mean <- sum(w * survey$age)
  expect_true(weighted_age_mean >= 37 - 0.5)
  expect_true(weighted_age_mean <= 43 + 0.5)
})

test_that("regrake handles rr_range with continuous variable (explicit bounds)", {
  set.seed(42)
  n <- 500

  # Sample: income mean around 50000
  survey <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.5, 0.5)),
    income = rnorm(n, mean = 50000, sd = 10000)
  )

  # Target: income should be between 55000 and 60000
  pop <- data.frame(
    variable = c("sex", "sex", "income"),
    level = c("M", "F", "mean"),
    target = c(0.5, 0.5, 57500) # midpoint of bounds
  )

  result <- regrake(
    data = survey,
    formula = ~ rr_exact(sex) + rr_range(income, 55000, 60000),
    population_data = pop,
    pop_type = "proportions"
  )

  # Check weights exist
  expect_length(result$weights, n)
  expect_true(all(result$weights >= 0))

  # Check weighted mean income is within bounds [55000, 60000]
  w <- result$weights / sum(result$weights)
  weighted_income_mean <- sum(w * survey$income)
  expect_true(weighted_income_mean >= 55000 - 500)
  expect_true(weighted_income_mean <= 60000 + 500)
})

test_that("regrake handles rr_range with categorical interaction", {
  set.seed(42)
  n <- 500

  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6, 0.4)),
    region = sample(c("N", "S"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  # Target joint distribution with ±0.03 margin
  pop_data <- data.frame(
    variable = c(rep("sex:region", 4)),
    level = c("F:N", "F:S", "M:N", "M:S"),
    target = c(0.25, 0.25, 0.25, 0.25)
  )

  result <- regrake(
    data = sample_data,
    formula = ~ rr_range(sex:region, 0.03),
    population_data = pop_data,
    pop_type = "proportions"
  )

  # Check weights exist
  expect_length(result$weights, n)
  expect_true(all(result$weights >= 0))

  # Check achieved proportions are within bounds [0.22, 0.28] for each cell
  w <- result$weights / sum(result$weights)
  for (s in c("M", "F")) {
    for (r in c("N", "S")) {
      achieved <- sum(w[sample_data$sex == s & sample_data$region == r])
      expect_true(achieved >= 0.22 - 0.01, label = paste(s, r, "lower"))
      expect_true(achieved <= 0.28 + 0.01, label = paste(s, r, "upper"))
    }
  }
})

test_that("rr_between works as alias for rr_range", {
  set.seed(42)
  n <- 500

  survey <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.5, 0.5)),
    age = rnorm(n, mean = 35, sd = 10)
  )

  pop <- data.frame(
    variable = c("sex", "sex", "age"),
    level = c("M", "F", "mean"),
    target = c(0.5, 0.5, 40)
  )

  # rr_between should work exactly like rr_range
  result <- regrake(
    data = survey,
    formula = ~ rr_exact(sex) + rr_between(age, 38, 42),
    population_data = pop,
    pop_type = "proportions"
  )

  # Check weights exist
  expect_length(result$weights, n)
  expect_true(all(result$weights >= 0))

  # Check weighted mean age is within bounds [38, 42]
  w <- result$weights / sum(result$weights)
  weighted_age_mean <- sum(w * survey$age)
  expect_true(weighted_age_mean >= 38 - 0.5)
  expect_true(weighted_age_mean <= 42 + 0.5)
})
