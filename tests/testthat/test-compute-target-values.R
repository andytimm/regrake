test_that("compute_target_values handles basic validation", {
  # Missing required columns
  pop_data <- data.frame(
    variable = "race",
    level = "white"
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~race)),
    "must contain columns: variable, level, target"
  )

  # Targets not summing to 1
  pop_data <- data.frame(
    variable = rep("race", 2),
    level = c("white", "black"),
    target = c(0.7, 0.7), # Sums to 1.4
    stringsAsFactors = FALSE
  )

  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~race)),
    "too far from 1.0 to auto-normalize"
  )
})

test_that("compute_target_values handles joint distributions correctly", {
  # Create test data in autumn format
  pop_data <- data.frame(
    variable = c(
      rep("race", 2),
      rep("age", 2),
      rep("race:age", 4) # 2x2 joint distribution
    ),
    level = c(
      "white",
      "black", # race levels
      "young",
      "old", # age levels
      # Joint levels
      "white:young",
      "white:old",
      "black:young",
      "black:old"
    ),
    target = c(
      0.6,
      0.4, # race marginals
      0.3,
      0.7, # age marginals
      0.2,
      0.4, # white x age groups
      0.1,
      0.3 # black x age groups
    )
  )

  # Create formula specification with warning
  expect_warning(
    formula_spec <- parse_raking_formula(~ race + age + race:age),
    "Variables in rr_exact\\(race:age\\) also appear as main effects"
  )

  # Compute target values
  result <- compute_target_values(pop_data, formula_spec)

  # Test structure
  expect_type(result, "list")
  expect_named(result, c("targets", "variables"))
  expect_named(result$targets, c("exact_race", "exact_age", "exact_race:age"))

  # Test that main effect targets match
  expect_equal(
    unname(result$targets$exact_race),
    c(0.6, 0.4)
  )
  expect_equal(
    unname(result$targets$exact_age),
    c(0.3, 0.7)
  )

  # Test that joint distribution targets match and sum to 1
  expect_equal(
    length(result$targets$`exact_race:age`),
    4 # 2x2 joint distribution
  )
  expect_equal(
    sum(result$targets$`exact_race:age`),
    1
  )

  # Test missing joint distribution warning
  pop_data_no_joint <- pop_data[pop_data$variable != "race:age", ]
  expect_warning(
    expect_error(
      compute_target_values(
        pop_data_no_joint,
        parse_raking_formula(~ race + age + race:age)
      ),
      "Missing target values for variable: race:age"
    ),
    "Variables in rr_exact\\(race:age\\) also appear as main effects"
  )
})

test_that("autumn format handles edge cases", {
  # Empty levels - should error because missing required columns
  pop_data <- data.frame(
    variable = "empty_var",
    level = "level1"
    # Missing target column
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~empty_var)),
    "must contain columns: variable, level, target"
  )

  # Small targets
  pop_data <- data.frame(
    variable = rep("small_var", 3),
    level = c("a", "b", "c"),
    target = c(0.999999, 0.000001, 0) # Very small but valid targets
  )
  result <- compute_target_values(pop_data, parse_raking_formula(~small_var))
  expect_equal(sum(result$targets$exact_small_var), 1)

  # Non-standard names
  pop_data <- data.frame(
    variable = rep(c("with space", "with.dot", "with/slash"), each = 1),
    level = c("level 1", "level.2", "level/3"),
    target = c(1, 1, 1)
  )
  expect_error(
    compute_target_values(
      pop_data,
      parse_raking_formula(~ `with space` + `with.dot` + `with/slash`)
    ),
    NA # Should not error
  )
})

test_that("autumn format validates input correctly", {
  # Targets not summing to 1
  pop_data <- data.frame(
    variable = rep("var", 2),
    level = c("a", "b"),
    target = c(0.7, 0.7) # Sums to 1.4
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~var)),
    "too far from 1.0 to auto-normalize"
  )

  # Duplicate variable-level combinations
  pop_data <- data.frame(
    variable = rep("var", 3),
    level = c("a", "b", "b"), # Duplicate level
    target = c(0.5, 0.25, 0.25)
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~var)),
    "Duplicate variable-level combination"
  )

  # Wrong column types
  pop_data <- data.frame(
    variable = 1, # Should be character
    level = "a",
    target = "0.5" # Should be numeric
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~var)),
    "Invalid column types"
  )
})

test_that("process_raw_data computes means for continuous variables", {
  # Raw data with both categorical and continuous variables
  raw_data <- data.frame(
    sex = c("M", "F", "M", "F", "M"),
    age = c(25, 35, 45, 30, 40),
    income = c(50000, 75000, 45000, 80000, 55000)
  )

  # Use process_pop_data with raw type (which calls process_raw_data)
  result <- process_pop_data(raw_data, "raw", NULL)

  # Should have rows for categorical levels AND continuous means
  expect_true("sex" %in% result$variable)
  expect_true("age" %in% result$variable)
  expect_true("income" %in% result$variable)

  # Categorical (sex) should have proportions summing to 1
  sex_rows <- result[result$variable == "sex", ]
  expect_equal(nrow(sex_rows), 2) # M and F
  expect_equal(sum(sex_rows$target), 1)

  # Continuous (age, income) should have single row with level = "mean"
  age_rows <- result[result$variable == "age", ]
  expect_equal(nrow(age_rows), 1)
  expect_equal(age_rows$level, "mean")
  expect_equal(age_rows$target, mean(raw_data$age))

  income_rows <- result[result$variable == "income", ]
  expect_equal(nrow(income_rows), 1)
  expect_equal(income_rows$level, "mean")
  expect_equal(income_rows$target, mean(raw_data$income))
})

test_that("validation skips sums-to-1 check for continuous variables", {
  # Population data with continuous variable (target doesn't sum to 1)
  pop_data <- data.frame(
    variable = c("sex", "sex", "age"),
    level = c("M", "F", "mean"),
    target = c(0.49, 0.51, 35.5) # age target is a mean, not a proportion
  )

  formula_spec <- parse_raking_formula(~ sex + age)

  # Should not error - continuous variables should be exempt from sums-to-1 validation
  expect_error(
    compute_target_values(pop_data, formula_spec),
    NA
  )
})

test_that("validation still catches categorical variables not summing to 1", {
  # Population data with categorical variable that doesn't sum to 1
  pop_data <- data.frame(
    variable = c("sex", "sex", "age"),
    level = c("M", "F", "mean"),
    target = c(0.4, 0.4, 35.5) # sex sums to 0.8, not 1
  )

  formula_spec <- parse_raking_formula(~ sex + age)

  # Should error because sex targets don't sum to 1
  expect_error(
    compute_target_values(pop_data, formula_spec),
    "too far from 1.0 to auto-normalize"
  )
})

test_that("autumn format handles complex interactions", {
  # N-way interactions
  pop_data <- data.frame(
    variable = c(
      rep("a", 2),
      rep("b", 2),
      rep("c", 2), # Main effects
      rep("a:b:c", 8) # 3-way interaction (2x2x2)
    ),
    level = c(
      "a1",
      "a2", # a levels
      "b1",
      "b2", # b levels
      "c1",
      "c2", # c levels
      # 3-way interaction levels
      "a1:b1:c1",
      "a1:b1:c2",
      "a1:b2:c1",
      "a1:b2:c2",
      "a2:b1:c1",
      "a2:b1:c2",
      "a2:b2:c1",
      "a2:b2:c2"
    ),
    target = c(
      0.6,
      0.4, # a targets
      0.3,
      0.7, # b targets
      0.5,
      0.5, # c targets
      # 3-way targets
      0.1,
      0.1,
      0.2,
      0.2,
      0.1,
      0.1,
      0.1,
      0.1
    )
  )

  expect_warning(
    formula_spec <- parse_raking_formula(~ a + b + c + a:b:c),
    "Variables in rr_exact\\(a:b:c\\) also appear as main effects"
  )
  result <- compute_target_values(pop_data, formula_spec)
  expect_named(
    result$targets,
    c("exact_a", "exact_b", "exact_c", "exact_a:b:c")
  )
  expect_equal(length(result$targets$`exact_a:b:c`), 8) # All combinations

  # Missing joint distribution
  pop_data_no_joint <- pop_data[pop_data$variable != "a:b:c", ]
  expect_warning(
    expect_error(
      compute_target_values(
        pop_data_no_joint,
        parse_raking_formula(~ a + b + c + a:b:c)
      ),
      "Missing target values for variable: a:b:c"
    ),
    "Variables in rr_exact\\(a:b:c\\) also appear as main effects"
  )

  # Overlapping variables
  pop_data <- data.frame(
    variable = c(
      rep("x", 2),
      rep("y", 2), # Main effects
      rep("x:y", 4), # 2-way interaction
      rep("x:y:z", 8) # 3-way interaction including x:y
    ),
    level = c(
      "x1",
      "x2", # x levels
      "y1",
      "y2", # y levels
      "x1:y1",
      "x1:y2",
      "x2:y1",
      "x2:y2", # x:y levels
      paste0("x", rep(1:2, each = 4), ":y", rep(1:2, each = 2), ":z", 1:2) # x:y:z levels
    ),
    target = c(
      0.5,
      0.5, # x targets
      0.5,
      0.5, # y targets
      0.25,
      0.25,
      0.25,
      0.25, # x:y targets
      rep(0.125, 8) # x:y:z targets
    )
  )

  # Test that we get both overlapping variables warnings
  expect_warning(
    expect_warning(
      formula_spec <- parse_raking_formula(~ x + y + x:y + x:y:z),
      "Variables in rr_exact\\(x:y:z\\) also appear as main effects"
    ),
    "Variables in rr_exact\\(x:y\\) also appear as main effects"
  )

  result <- compute_target_values(pop_data, formula_spec)
  expect_named(
    result$targets,
    c("exact_x", "exact_y", "exact_x:y", "exact_x:y:z")
  )
})

# ============================================================================
# Tests for process_weighted_data
# ============================================================================

test_that("process_weighted_data computes weighted proportions for categorical", {
  weighted_data <- data.frame(
    sex = c("M", "F", "M", "F"),
    wt = c(100, 200, 50, 150) # Total weight = 500
  )

  result <- process_pop_data(weighted_data, "weighted", "wt")

  # Expected: M = 150/500 = 0.3, F = 350/500 = 0.7
  sex_rows <- result[result$variable == "sex", ]
  expect_equal(nrow(sex_rows), 2)

  m_target <- sex_rows$target[sex_rows$level == "M"]
  f_target <- sex_rows$target[sex_rows$level == "F"]

  expect_equal(m_target, 0.3)
  expect_equal(f_target, 0.7)
  expect_equal(sum(sex_rows$target), 1)
})

test_that("process_weighted_data computes weighted means for continuous", {
  weighted_data <- data.frame(
    age = c(20, 30, 40, 50),
    wt = c(1, 2, 3, 4) # Total weight = 10
  )

  result <- process_pop_data(weighted_data, "weighted", "wt")

  # Expected: weighted mean = (20*1 + 30*2 + 40*3 + 50*4) / 10 = 400/10 = 40
  age_rows <- result[result$variable == "age", ]
  expect_equal(nrow(age_rows), 1)
  expect_equal(age_rows$level, "mean")
  expect_equal(age_rows$target, 40)
})

test_that("process_weighted_data handles mixed variables", {
  weighted_data <- data.frame(
    sex = c("M", "F", "M"),
    age = c(25, 35, 45),
    wt = c(2, 3, 5) # Total weight = 10
  )

  result <- process_pop_data(weighted_data, "weighted", "wt")

  # Sex: M = 7/10 = 0.7, F = 3/10 = 0.3
  sex_rows <- result[result$variable == "sex", ]
  expect_equal(sum(sex_rows$target), 1)

  # Age: weighted mean = (25*2 + 35*3 + 45*5) / 10 = 380/10 = 38
  age_rows <- result[result$variable == "age", ]
  expect_equal(age_rows$target, 38)
})

test_that("process_weighted_data errors on missing weight column", {
  data <- data.frame(sex = c("M", "F"))
  # Format detection sees this as "raw" since weight column is missing
  expect_error(
    process_pop_data(data, "weighted", "wt"),
    "appears to be in 'raw' format"
  )
})

# ============================================================================
# Tests for process_anesrake_data
# ============================================================================

test_that("process_anesrake_data converts named vectors to autumn format", {
  anesrake_data <- list(
    sex = c(M = 0.49, F = 0.51),
    age = c("18-34" = 0.3, "35-54" = 0.4, "55+" = 0.3)
  )

  result <- process_pop_data(anesrake_data, "anesrake", NULL)

  # Check structure
  expect_true(all(c("variable", "level", "target") %in% names(result)))

  # Check sex
  sex_rows <- result[result$variable == "sex", ]
  expect_equal(nrow(sex_rows), 2)
  expect_equal(sum(sex_rows$target), 1)
  expect_equal(sex_rows$target[sex_rows$level == "M"], 0.49)
  expect_equal(sex_rows$target[sex_rows$level == "F"], 0.51)

  # Check age
  age_rows <- result[result$variable == "age", ]
  expect_equal(nrow(age_rows), 3)
  expect_equal(sum(age_rows$target), 1)
})

test_that("process_anesrake_data errors on non-numeric vectors", {
  bad_data <- list(
    sex = c(M = "0.49", F = "0.51") # Character, not numeric
  )

  # Format detection sees this as "raw" since vectors aren't numeric
  expect_error(
    process_pop_data(bad_data, "anesrake", NULL),
    "appears to be in 'raw' format"
  )
})

test_that("process_anesrake_data errors when targets don't sum to 1", {
  bad_data <- list(
    sex = c(M = 0.4, F = 0.4) # Sums to 0.8
  )

  expect_error(
    process_pop_data(bad_data, "anesrake", NULL),
    "too far from 1.0 to auto-normalize"
  )
})

# ============================================================================
# Tests for process_survey_data
# ============================================================================

test_that("process_survey_data converts margin format to autumn format", {
  survey_data <- data.frame(
    margin = c("sex", "sex", "age", "age", "age"),
    category = c("M", "F", "18-34", "35-54", "55+"),
    value = c(0.49, 0.51, 0.3, 0.4, 0.3)
  )

  result <- process_pop_data(survey_data, "survey", NULL)

  # Check structure
  expect_true(all(c("variable", "level", "target") %in% names(result)))

  # Check sex
  sex_rows <- result[result$variable == "sex", ]
  expect_equal(nrow(sex_rows), 2)
  expect_equal(sum(sex_rows$target), 1)

  # Check age
  age_rows <- result[result$variable == "age", ]
  expect_equal(nrow(age_rows), 3)
  expect_equal(sum(age_rows$target), 1)
})

test_that("process_survey_data handles interactions in margin", {
  survey_data <- data.frame(
    margin = c(rep("sex:age", 6)),
    category = c("M:young", "M:old", "M:middle", "F:young", "F:old", "F:middle"),
    value = c(0.15, 0.2, 0.14, 0.17, 0.19, 0.15)
  )

  result <- process_pop_data(survey_data, "survey", NULL)

  # Check interaction is preserved
  expect_equal(unique(result$variable), "sex:age")
  expect_equal(nrow(result), 6)
  expect_equal(sum(result$target), 1)
})

test_that("process_survey_data errors when targets don't sum to 1", {
  bad_data <- data.frame(
    margin = c("sex", "sex"),
    category = c("M", "F"),
    value = c(0.4, 0.4) # Sums to 0.8
  )

  expect_error(
    process_pop_data(bad_data, "survey", NULL),
    "too far from 1.0 to auto-normalize"
  )
})

test_that("process_survey_data errors on missing columns", {
  bad_data <- data.frame(
    margin = "sex",
    category = "M"
    # Missing value column
  )

  # Format detection sees this as "raw" since value column is missing
  expect_error(
    process_pop_data(bad_data, "survey", NULL),
    "appears to be in 'raw' format"
  )
})

# ============================================================================
# Tests for process_survey_design_data
# ============================================================================

test_that("process_survey_design_data works with categorical variables", {
  # Create sample data with known proportions
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F", "M")),
    age = factor(c("young", "old", "young", "old", "young")),
    prob = rep(0.2, 5) # Equal probability sample
  )

  # Create survey design
  design <- survey::svydesign(ids = ~1, probs = ~prob, data = data)

  # Create formula_spec
  formula_spec <- parse_raking_formula(~ sex + age)

  # Process
  result <- process_survey_design_data(design, formula_spec)

  # Verify structure
  expect_true(all(c("variable", "level", "target") %in% names(result)))

  # Verify sex proportions (3 M, 2 F -> 0.6, 0.4)
  sex_rows <- result[result$variable == "sex", ]
  expect_equal(nrow(sex_rows), 2)
  expect_equal(sum(sex_rows$target), 1)
  expect_equal(sex_rows$target[sex_rows$level == "M"], 0.6)
  expect_equal(sex_rows$target[sex_rows$level == "F"], 0.4)

  # Verify age proportions (3 young, 2 old -> 0.6, 0.4)
  age_rows <- result[result$variable == "age", ]
  expect_equal(nrow(age_rows), 2)
  expect_equal(sum(age_rows$target), 1)
  expect_equal(age_rows$target[age_rows$level == "young"], 0.6)
  expect_equal(age_rows$target[age_rows$level == "old"], 0.4)
})

test_that("process_survey_design_data works with continuous variables", {
  data <- data.frame(
    income = c(50000, 60000, 70000, 80000),
    prob = rep(0.25, 4)
  )

  design <- survey::svydesign(ids = ~1, probs = ~prob, data = data)
  formula_spec <- parse_raking_formula(~ rr_mean(income))

  result <- process_survey_design_data(design, formula_spec)

  # Should have weighted mean
  expect_equal(nrow(result), 1)
  expect_equal(result$variable, "income")
  expect_equal(result$level, "mean")
  expect_equal(result$target, 65000) # (50+60+70+80)/4 = 65
})

test_that("process_survey_design_data works with weighted samples", {
  # Unequal weights scenario
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F")),
    prob = c(0.1, 0.4, 0.1, 0.4) # M oversampled (lower prob = higher weight)
  )

  design <- survey::svydesign(ids = ~1, probs = ~prob, data = data)
  formula_spec <- parse_raking_formula(~ sex)

  result <- process_survey_design_data(design, formula_spec)

  # Weights: 1/0.1=10, 1/0.4=2.5, 1/0.1=10, 1/0.4=2.5
  # M total weight: 10+10=20, F total weight: 2.5+2.5=5
  # M proportion: 20/25=0.8, F proportion: 5/25=0.2
  sex_rows <- result[result$variable == "sex", ]
  expect_equal(sex_rows$target[sex_rows$level == "M"], 0.8)
  expect_equal(sex_rows$target[sex_rows$level == "F"], 0.2)
})

test_that("process_survey_design_data works with interactions", {
  data <- data.frame(
    sex = factor(c("M", "F", "M", "F")),
    age = factor(c("young", "young", "old", "old")),
    prob = rep(0.25, 4)
  )

  design <- survey::svydesign(ids = ~1, probs = ~prob, data = data)

  suppressWarnings({
    formula_spec <- parse_raking_formula(~ sex + age + sex:age)
  })

  result <- process_survey_design_data(design, formula_spec)

  # Should have sex:age joint distribution
  joint_rows <- result[result$variable == "sex:age", ]
  expect_equal(nrow(joint_rows), 4) # 2x2
  expect_equal(sum(joint_rows$target), 1)
  # Each combination appears once with equal weight -> 0.25 each
  expect_true(all(abs(joint_rows$target - 0.25) < 1e-10))
})

test_that("process_survey_design_data requires formula_spec", {
  data <- data.frame(x = c(1, 2, 3), prob = rep(0.33, 3))
  design <- survey::svydesign(ids = ~1, probs = ~prob, data = data)

  expect_error(
    process_survey_design_data(design, NULL),
    "formula_spec is required"
  )
})

test_that("process_survey_design_data validates design object", {
  formula_spec <- parse_raking_formula(~ sex)

  expect_error(
    process_survey_design_data(data.frame(sex = c("M", "F")), formula_spec),
    "Expected a survey.design object"
  )
})

test_that("process_survey_design_data errors on missing variables", {
  data <- data.frame(
    sex = factor(c("M", "F")),
    prob = rep(0.5, 2)
  )

  design <- survey::svydesign(ids = ~1, probs = ~prob, data = data)
  formula_spec <- parse_raking_formula(~ sex + age) # age doesn't exist

  expect_error(
    process_survey_design_data(design, formula_spec),
    "Variable\\(s\\) not found in survey design: age"
  )
})

test_that("regrake works end-to-end with survey_design format", {
  set.seed(42)
  n <- 500

  # Sample data (biased toward M and N)
  sample_data <- data.frame(
    sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.65, 0.35)),
    region = sample(c("N", "S"), n, replace = TRUE, prob = c(0.7, 0.3))
  )

  # Population survey design (50/50 splits)
  pop_n <- 1000
  pop_data <- data.frame(
    sex = rep(c("M", "F"), each = pop_n / 2),
    region = rep(c("N", "S"), pop_n / 2),
    prob = rep(1 / pop_n, pop_n)
  )
  pop_design <- survey::svydesign(ids = ~1, probs = ~prob, data = pop_data)

  result <- regrake(
    data = sample_data,
    population_data = pop_design,
    formula = ~ sex + region,
    pop_type = "survey_design"
  )

  expect_equal(sum(result$weights), n)

  # Check proportions are close to 50/50
  w <- result$weights / sum(result$weights)
  weighted_sex <- tapply(w, sample_data$sex, sum)
  expect_equal(unname(weighted_sex["M"]), 0.5, tolerance = 0.01)
  expect_equal(unname(weighted_sex["F"]), 0.5, tolerance = 0.01)

  weighted_region <- tapply(w, sample_data$region, sum)
  expect_equal(unname(weighted_region["N"]), 0.5, tolerance = 0.01)
  expect_equal(unname(weighted_region["S"]), 0.5, tolerance = 0.01)
})

# =============================================================================
# Tests for target sum normalization (tiered tolerance)
# =============================================================================

test_that("targets silently normalized for floating point artifacts", {
  # 1/3 + 1/3 + 1/3 doesn't sum to exactly 1 due to floating point
  pop_data <- data.frame(
    variable = rep("race", 3),
    level = c("A", "B", "C"),
    target = c(1 / 3, 1 / 3, 1 / 3)
  )

  formula_spec <- parse_raking_formula(~ rr_exact(race))

  # Should not warn or error
  expect_silent(compute_target_values(pop_data, formula_spec))

  # Targets should be normalized
  result <- compute_target_values(pop_data, formula_spec)
  target_sum <- sum(result$targets[[1]])
  expect_equal(target_sum, 1.0)
})

test_that("targets warn and normalize for small deviations (within 5%)", {
  # Sum = 1.01 (1% deviation)
  pop_data <- data.frame(
    variable = rep("sex", 2),
    level = c("M", "F"),
    target = c(0.505, 0.505) # Sum = 1.01
  )

  formula_spec <- parse_raking_formula(~ rr_exact(sex))

  # Should warn but not error
  expect_warning(
    compute_target_values(pop_data, formula_spec),
    "normalizing to 1.0"
  )

  # Targets should still be normalized
  result <- suppressWarnings(compute_target_values(pop_data, formula_spec))
  target_sum <- sum(result$targets[[1]])
  expect_equal(target_sum, 1.0)
})

test_that("targets error for large deviations (beyond 5%)", {
  # Sum = 0.8 (20% deviation)
  pop_data <- data.frame(
    variable = rep("sex", 2),
    level = c("M", "F"),
    target = c(0.4, 0.4) # Sum = 0.8
  )

  formula_spec <- parse_raking_formula(~ rr_exact(sex))

  expect_error(
    compute_target_values(pop_data, formula_spec),
    "too far from 1.0"
  )
})

test_that("targets error for large positive deviations", {
  # Sum = 1.2 (20% deviation)
  pop_data <- data.frame(
    variable = rep("sex", 2),
    level = c("M", "F"),
    target = c(0.6, 0.6) # Sum = 1.2
  )

  formula_spec <- parse_raking_formula(~ rr_exact(sex))

  expect_error(
    compute_target_values(pop_data, formula_spec),
    "too far from 1.0"
  )
})

test_that("continuous variables are not affected by sum normalization", {
  # Continuous targets don't need to sum to 1
  pop_data <- data.frame(
    variable = c("sex", "sex", "age"),
    level = c("M", "F", "mean"),
    target = c(0.5, 0.5, 35.5)
  )

  formula_spec <- parse_raking_formula(~ rr_exact(sex) + rr_mean(age))

  # Should not error - continuous age doesn't need sum validation
  expect_silent(compute_target_values(pop_data, formula_spec))
})

test_that("anesrake format normalizes targets silently", {
  # Floating point sum
  data <- list(
    race = c(A = 1 / 3, B = 1 / 3, C = 1 / 3)
  )

  # Should not warn (within 1e-3 tolerance)
  expect_silent(process_pop_data(data, "anesrake", NULL))
})

test_that("survey format normalizes targets silently", {
  # Floating point sum
  data <- data.frame(
    margin = rep("race", 3),
    category = c("A", "B", "C"),
    value = c(1 / 3, 1 / 3, 1 / 3)
  )

  # Should not warn (within 1e-3 tolerance)
  expect_silent(process_pop_data(data, "survey", NULL))
})
