test_that("regrake supports KL regularizer with explicit prior", {
  sample_data <- make_sample_sex_age(n = 300, seed = 123)
  pop_data <- make_pop_sex_age()
  prior <- stats::runif(nrow(sample_data))
  prior <- prior / sum(prior)

  result <- regrake(
    data = sample_data,
    formula = ~ rr_exact(sex) + rr_exact(age),
    population_data = pop_data,
    pop_type = "proportions",
    regularizer = "kl",
    prior = prior
  )

  expect_length(result$weights, nrow(sample_data))
  expect_true(all(result$weights > 0))
  expect_equal(sum(result$weights), nrow(sample_data), tolerance = 1e-6)
  expect_equal(result$prior, prior, tolerance = 1e-12)
})

test_that("regrake errors for KL regularizer without prior", {
  sample_data <- make_sample_sex_age(n = 200, seed = 456)
  pop_data <- make_pop_sex_age()

  expect_error(
    regrake(
      data = sample_data,
      formula = ~ rr_exact(sex) + rr_exact(age),
      population_data = pop_data,
      pop_type = "proportions",
      regularizer = "kl"
    ),
    "prior must be provided when regularizer = 'kl'"
  )
})

test_that("regrake normalizes prior that does not sum to 1", {
  sample_data <- make_sample_sex_age(n = 250, seed = 789)
  pop_data <- make_pop_sex_age()
  prior <- rep(2, nrow(sample_data))

  expect_warning(
    result <- regrake(
      data = sample_data,
      formula = ~ rr_exact(sex) + rr_exact(age),
      population_data = pop_data,
      pop_type = "proportions",
      regularizer = "kl",
      prior = prior
    ),
    "prior does not sum to 1; normalizing internally"
  )

  expect_equal(sum(result$prior), 1, tolerance = 1e-12)
  expect_true(all(result$prior > 0))
})

test_that("regrake validates prior values", {
  sample_data <- make_sample_sex_age(n = 150, seed = 111)
  pop_data <- make_pop_sex_age()

  expect_error(
    regrake(
      data = sample_data,
      formula = ~ rr_exact(sex) + rr_exact(age),
      population_data = pop_data,
      pop_type = "proportions",
      regularizer = "kl",
      prior = rep(1, nrow(sample_data) - 1)
    ),
    "length equal to nrow\\(data\\)"
  )

  bad_prior <- rep(1, nrow(sample_data))
  bad_prior[1] <- 0
  expect_error(
    regrake(
      data = sample_data,
      formula = ~ rr_exact(sex) + rr_exact(age),
      population_data = pop_data,
      pop_type = "proportions",
      regularizer = "kl",
      prior = bad_prior
    ),
    "strictly positive"
  )
})

test_that("prior is ignored for non-KL regularizers", {
  sample_data <- make_sample_sex_age(n = 200, seed = 222)
  pop_data <- make_pop_sex_age()
  prior <- rep(1 / nrow(sample_data), nrow(sample_data))

  expect_warning(
    result <- regrake(
      data = sample_data,
      formula = ~ rr_exact(sex) + rr_exact(age),
      population_data = pop_data,
      pop_type = "proportions",
      regularizer = "entropy",
      prior = prior
    ),
    "Argument `prior` is ignored unless regularizer = 'kl'"
  )

  expect_null(result$prior)
})
