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
    age = sample(c("18-34", "35-54", "55+"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
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
    pop_type = "proportions"  # Explicitly specify pop_type
  )

  # Check that weights sum to n
  expect_equal(sum(result$weights), n)

  # Check that weighted proportions match targets
  wtd_props_sex <- stats::aggregate(
    result$weights,
    list(sex = sample_data$sex),
    sum
  )$x / n

  wtd_props_age <- stats::aggregate(
    result$weights,
    list(age = sample_data$age),
    sum
  )$x / n

  # Get target proportions in same order as weighted props
  sex_targets <- pop_data$proportion[pop_data$variable == "sex"]
  age_targets <- pop_data$proportion[pop_data$variable == "age"]

  # Test that weighted proportions match targets within tolerance
  expect_equal(wtd_props_sex, sex_targets, tolerance = 1e-4)
  expect_equal(wtd_props_age, age_targets, tolerance = 1e-4)
})