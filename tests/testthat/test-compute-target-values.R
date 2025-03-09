test_that("compute_target_values handles basic validation", {
  # Missing required columns
  pop_data <- data.frame(
    variable = "race",
    level = "white"
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~ race)),
    "must contain columns: variable, level, proportion"
  )

  # Proportions not summing to 1
  pop_data <- data.frame(
    variable = rep("race", 2),
    level = c("white", "black"),
    proportion = c(0.7, 0.7),  # Sums to 1.4
    stringsAsFactors = FALSE
  )

  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~ race)),
    "Proportions for variable 'race' do not sum to 1"
  )
})

test_that("compute_target_values handles joint distributions correctly", {
  # Create test data in autumn format
  pop_data <- data.frame(
    variable = c(
      rep("race", 2),
      rep("age", 2),
      rep("race:age", 4)  # 2x2 joint distribution
    ),
    level = c(
      "white", "black",    # race levels
      "young", "old",      # age levels
      # Joint levels
      "white:young", "white:old",
      "black:young", "black:old"
    ),
    proportion = c(
      0.6, 0.4,    # race marginals
      0.3, 0.7,    # age marginals
      0.2, 0.4,    # white x age groups
      0.1, 0.3     # black x age groups
    )
  )

  # Create formula specification with warning
  expect_warning(
    formula_spec <- parse_raking_formula(~ race + age + race:age),
    "Variables in exact\\(race:age\\) also appear as main effects"
  )

  # Compute target values
  result <- compute_target_values(pop_data, formula_spec)

  # Test structure
  expect_type(result, "list")
  expect_named(result, c("targets", "variables"))
  expect_named(result$targets, c("exact_race", "exact_age", "exact_race:age"))

  # Test that main effect proportions match
  expect_equal(
    unname(result$targets$exact_race),
    c(0.6, 0.4)
  )
  expect_equal(
    unname(result$targets$exact_age),
    c(0.3, 0.7)
  )

  # Test that joint distribution proportions match and sum to 1
  expect_equal(
    length(result$targets$`exact_race:age`),
    4  # 2x2 joint distribution
  )
  expect_equal(
    sum(result$targets$`exact_race:age`),
    1
  )

  # Test missing joint distribution warning
  pop_data_no_joint <- pop_data[pop_data$variable != "race:age", ]
  expect_warning(
    expect_error(
      compute_target_values(pop_data_no_joint, parse_raking_formula(~ race + age + race:age)),
      "Missing target values for variable: race:age"
    ),
    "Variables in exact\\(race:age\\) also appear as main effects"
  )
})

test_that("autumn format handles edge cases", {
  # Empty levels - should error because missing required columns
  pop_data <- data.frame(
    variable = "empty_var",
    level = "level1"
    # Missing proportion column
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~ empty_var)),
    "must contain columns: variable, level, proportion"
  )

  # Small proportions
  pop_data <- data.frame(
    variable = rep("small_var", 3),
    level = c("a", "b", "c"),
    proportion = c(0.999999, 0.000001, 0)  # Very small but valid proportions
  )
  result <- compute_target_values(pop_data, parse_raking_formula(~ small_var))
  expect_equal(sum(result$targets$exact_small_var), 1)

  # Non-standard names
  pop_data <- data.frame(
    variable = rep(c("with space", "with.dot", "with/slash"), each = 1),
    level = c("level 1", "level.2", "level/3"),
    proportion = c(1, 1, 1)
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~ `with space` + `with.dot` + `with/slash`)),
    NA  # Should not error
  )
})

test_that("autumn format validates input correctly", {
  # Proportions not summing to 1
  pop_data <- data.frame(
    variable = rep("var", 2),
    level = c("a", "b"),
    proportion = c(0.7, 0.7)  # Sums to 1.4
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~ var)),
    "Proportions for variable 'var' do not sum to 1"
  )

  # Duplicate variable-level combinations
  pop_data <- data.frame(
    variable = rep("var", 3),
    level = c("a", "b", "b"),  # Duplicate level
    proportion = c(0.5, 0.25, 0.25)
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~ var)),
    "Duplicate variable-level combination"
  )

  # Wrong column types
  pop_data <- data.frame(
    variable = 1,  # Should be character
    level = "a",
    proportion = "0.5"  # Should be numeric
  )
  expect_error(
    compute_target_values(pop_data, parse_raking_formula(~ var)),
    "Invalid column types"
  )
})

test_that("autumn format handles complex interactions", {
  # N-way interactions
  pop_data <- data.frame(
    variable = c(
      rep("a", 2), rep("b", 2), rep("c", 2),  # Main effects
      rep("a:b:c", 8)  # 3-way interaction (2x2x2)
    ),
    level = c(
      "a1", "a2",  # a levels
      "b1", "b2",  # b levels
      "c1", "c2",  # c levels
      # 3-way interaction levels
      "a1:b1:c1", "a1:b1:c2", "a1:b2:c1", "a1:b2:c2",
      "a2:b1:c1", "a2:b1:c2", "a2:b2:c1", "a2:b2:c2"
    ),
    proportion = c(
      0.6, 0.4,  # a proportions
      0.3, 0.7,  # b proportions
      0.5, 0.5,  # c proportions
      # 3-way proportions
      0.1, 0.1, 0.2, 0.2, 0.1, 0.1, 0.1, 0.1
    )
  )

  expect_warning(
    formula_spec <- parse_raking_formula(~ a + b + c + a:b:c),
    "Variables in exact\\(a:b:c\\) also appear as main effects"
  )
  result <- compute_target_values(pop_data, formula_spec)
  expect_named(result$targets, c("exact_a", "exact_b", "exact_c", "exact_a:b:c"))
  expect_equal(length(result$targets$`exact_a:b:c`), 8)  # All combinations

  # Missing joint distribution
  pop_data_no_joint <- pop_data[pop_data$variable != "a:b:c", ]
  expect_warning(
    expect_error(
      compute_target_values(pop_data_no_joint, parse_raking_formula(~ a + b + c + a:b:c)),
      "Missing target values for variable: a:b:c"
    ),
    "Variables in exact\\(a:b:c\\) also appear as main effects"
  )

  # Overlapping variables
  pop_data <- data.frame(
    variable = c(
      rep("x", 2), rep("y", 2),  # Main effects
      rep("x:y", 4),  # 2-way interaction
      rep("x:y:z", 8)  # 3-way interaction including x:y
    ),
    level = c(
      "x1", "x2",  # x levels
      "y1", "y2",  # y levels
      "x1:y1", "x1:y2", "x2:y1", "x2:y2",  # x:y levels
      paste0("x", rep(1:2, each=4), ":y", rep(1:2, each=2), ":z", 1:2)  # x:y:z levels
    ),
    proportion = c(
      0.5, 0.5,  # x proportions
      0.5, 0.5,  # y proportions
      0.25, 0.25, 0.25, 0.25,  # x:y proportions
      rep(0.125, 8)  # x:y:z proportions
    )
  )

  # Test that we get both overlapping variables warnings
  expect_warning(
    expect_warning(
      formula_spec <- parse_raking_formula(~ x + y + x:y + x:y:z),
      "Variables in exact\\(x:y:z\\) also appear as main effects"
    ),
    "Variables in exact\\(x:y\\) also appear as main effects"
  )

  result <- compute_target_values(pop_data, formula_spec)
  expect_named(result$targets, c("exact_x", "exact_y", "exact_x:y", "exact_x:y:z"))
})