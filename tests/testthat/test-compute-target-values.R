test_that("compute_target_values handles joint distributions correctly", {
  # Create test data with both marginal and joint distributions
  pop_data <- tibble::tribble(
    ~variable,    ~level,         ~proportion,
    "race",       "white",        0.6,
    "race",       "nonwhite",     0.4,
    "age",        "18-34",        0.3,
    "age",        "35-54",        0.4,
    "age",        "55+",          0.3,
    "race:age",   "white:18-34",  0.15,
    "race:age",   "white:35-54",  0.25,
    "race:age",   "white:55+",    0.20,
    "race:age",   "nonwhite:18-34", 0.15,
    "race:age",   "nonwhite:35-54", 0.15,
    "race:age",   "nonwhite:55+",   0.10
  )

  # Test formula with main effects and interaction
  formula <- ~ race + age + l2(race:age)

  # Expect warning about overlapping variables
  expect_warning(
    formula_spec <- parse_raking_formula(formula),
    "Variables in l2\\(race:age\\) also appear as main effects"
  )

  # Compute target values
  targets <- compute_target_values(pop_data, formula_spec)

  # Check main effects
  expect_equal(targets[[1]]$type, "exact")
  expect_equal(targets[[1]]$values, c(white = 0.6, nonwhite = 0.4))
  expect_equal(targets[[2]]$type, "exact")
  expect_equal(targets[[2]]$values, c("18-34" = 0.3, "35-54" = 0.4, "55+" = 0.3))

  # Check interaction uses provided joint distribution
  expect_equal(targets[[3]]$type, "l2")
  expect_equal(targets[[3]]$values, c(
    "white:18-34" = 0.15,
    "white:35-54" = 0.25,
    "white:55+" = 0.20,
    "nonwhite:18-34" = 0.15,
    "nonwhite:35-54" = 0.15,
    "nonwhite:55+" = 0.10
  ))

  # Test warning when joint distribution not provided
  pop_data_no_joint <- pop_data[pop_data$variable != "race:age", ]
  expect_warning(
    targets_no_joint <- compute_target_values(pop_data_no_joint, formula_spec),
    "Joint distribution for race:age not found in population data"
  )

  # Check independence assumption used correctly
  joint_props <- targets_no_joint[[3]]$values
  # Get the actual values using the full names
  white_young <- joint_props["race:white:age:18-34"]
  nonwhite_old <- joint_props["race:nonwhite:age:55+"]

  expect_equal(unname(white_young), 0.6 * 0.3)
  expect_equal(unname(nonwhite_old), 0.4 * 0.3)

  # Test error for missing variables
  pop_data_missing <- pop_data[pop_data$variable != "age", ]
  expect_error(
    compute_target_values(pop_data_missing, formula_spec),
    "Missing target values for variables: age"
  )
})