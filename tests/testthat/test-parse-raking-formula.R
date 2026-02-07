test_that("basic formula parsing works", {
  # Test single variable
  f1 <- parse_raking_formula(~age)
  expect_equal(length(f1$terms), 1)
  expect_equal(f1$terms[[1]]$type, "exact")
  expect_equal(f1$terms[[1]]$variables, "age")
  expect_equal(f1$variables, "age")

  # Test multiple variables
  f2 <- parse_raking_formula(~ race + age)
  expect_equal(length(f2$terms), 2)
  expect_equal(f2$terms[[1]]$type, "exact")
  expect_equal(f2$terms[[2]]$type, "exact")
  expect_equal(sort(f2$variables), c("age", "race"))
})

test_that("constraint functions are parsed correctly", {
  # Test l2 constraint
  f1 <- parse_raking_formula(~ rr_l2(age))
  expect_equal(f1$terms[[1]]$type, "l2")
  expect_equal(f1$terms[[1]]$variables, "age")

  # Test kl constraint
  f2 <- parse_raking_formula(~ rr_kl(income))
  expect_equal(f2$terms[[1]]$type, "kl")
  expect_equal(f2$terms[[1]]$variables, "income")

  # Test exact constraint
  f3 <- parse_raking_formula(~ rr_exact(education))
  expect_equal(f3$terms[[1]]$type, "exact")
  expect_equal(f3$terms[[1]]$variables, "education")

  # Test mean constraint (for continuous variables)
  f4 <- parse_raking_formula(~ rr_mean(income))
  expect_equal(f4$terms[[1]]$type, "exact") # rr_mean maps to exact internally
  expect_equal(f4$terms[[1]]$variables, "income")
})

test_that("rr_mean() works for continuous variables", {
  # rr_mean should work like rr_exact but is specifically for continuous variables
  # It maps to "exact" type internally

  # Single continuous variable
  f1 <- parse_raking_formula(~ rr_mean(age))
  expect_equal(f1$terms[[1]]$type, "exact")
  expect_equal(f1$terms[[1]]$variables, "age")
  expect_null(f1$terms[[1]]$interaction)

  # Multiple continuous variables
  f2 <- parse_raking_formula(~ rr_mean(age) + rr_mean(income))
  expect_equal(length(f2$terms), 2)
  expect_equal(f2$terms[[1]]$type, "exact")
  expect_equal(f2$terms[[2]]$type, "exact")

  # Mix of categorical (rr_exact) and continuous (rr_mean)
  f3 <- parse_raking_formula(~ rr_exact(sex) + rr_mean(age))
  expect_equal(length(f3$terms), 2)
  expect_equal(f3$terms[[1]]$type, "exact")
  expect_equal(f3$terms[[1]]$variables, "sex")
  expect_equal(f3$terms[[2]]$type, "exact")
  expect_equal(f3$terms[[2]]$variables, "age")
})

test_that("rr_var() parses correctly for variance constraints", {
  # rr_var keeps its own type "var" for variance matching

  # Single continuous variable
  f1 <- parse_raking_formula(~ rr_var(age))
  expect_equal(f1$terms[[1]]$type, "var")
  expect_equal(f1$terms[[1]]$variables, "age")
  expect_null(f1$terms[[1]]$interaction)

  # Combined with mean constraint
  f2 <- parse_raking_formula(~ rr_mean(age) + rr_var(age))
  expect_equal(length(f2$terms), 2)
  expect_equal(f2$terms[[1]]$type, "exact") # rr_mean maps to exact
  expect_equal(f2$terms[[2]]$type, "var")

  # Mix of categorical, mean, and variance
  f3 <- parse_raking_formula(~ rr_exact(sex) + rr_mean(age) + rr_var(income))
  expect_equal(length(f3$terms), 3)
  expect_equal(f3$terms[[1]]$type, "exact")
  expect_equal(f3$terms[[1]]$variables, "sex")
  expect_equal(f3$terms[[2]]$type, "exact")
  expect_equal(f3$terms[[2]]$variables, "age")
  expect_equal(f3$terms[[3]]$type, "var")
  expect_equal(f3$terms[[3]]$variables, "income")
})

test_that("rr_quantile() parses correctly for quantile constraints", {
  # rr_quantile requires two arguments: variable and probability

  # Positional argument
  f1 <- parse_raking_formula(~ rr_quantile(income, 0.5))
  expect_equal(f1$terms[[1]]$type, "quantile")
  expect_equal(f1$terms[[1]]$variables, "income")
  expect_equal(f1$terms[[1]]$params$p, 0.5)

  # Named argument
  f2 <- parse_raking_formula(~ rr_quantile(age, p = 0.25))
  expect_equal(f2$terms[[1]]$type, "quantile")
  expect_equal(f2$terms[[1]]$variables, "age")
  expect_equal(f2$terms[[1]]$params$p, 0.25)

  # Combined with other constraints
  f3 <- parse_raking_formula(
    ~ rr_mean(age) + rr_quantile(age, 0.5) + rr_var(age)
  )
  expect_equal(length(f3$terms), 3)
  expect_equal(f3$terms[[1]]$type, "exact") # rr_mean
  expect_equal(f3$terms[[2]]$type, "quantile")
  expect_equal(f3$terms[[2]]$params$p, 0.5)
  expect_equal(f3$terms[[3]]$type, "var")

  # Errors on missing probability
  expect_error(
    parse_raking_formula(~ rr_quantile(income)),
    "rr_quantile requires two arguments"
  )

  # Errors on invalid probability
  expect_error(
    parse_raking_formula(~ rr_quantile(income, 0)),
    "probability p must be between 0 and 1"
  )
  expect_error(
    parse_raking_formula(~ rr_quantile(income, 1)),
    "probability p must be between 0 and 1"
  )
})

test_that("rr_range() parses correctly for range/inequality constraints", {
  # Margin mode with single numeric (positional)
  f1 <- parse_raking_formula(~ rr_range(age, 0.02))
  expect_equal(f1$terms[[1]]$type, "range")
  expect_equal(f1$terms[[1]]$variables, "age")
  expect_equal(f1$terms[[1]]$params$mode, "margin")
  expect_equal(f1$terms[[1]]$params$margin, 0.02)
  expect_null(f1$terms[[1]]$interaction)

  # Margin mode with named argument
  f2 <- parse_raking_formula(~ rr_range(income, margin = 1000))
  expect_equal(f2$terms[[1]]$type, "range")
  expect_equal(f2$terms[[1]]$variables, "income")
  expect_equal(f2$terms[[1]]$params$mode, "margin")
  expect_equal(f2$terms[[1]]$params$margin, 1000)

  # Explicit bounds mode (positional)
  f3 <- parse_raking_formula(~ rr_range(age, 40, 45))
  expect_equal(f3$terms[[1]]$type, "range")
  expect_equal(f3$terms[[1]]$variables, "age")
  expect_equal(f3$terms[[1]]$params$mode, "bounds")
  expect_equal(f3$terms[[1]]$params$lower, 40)
  expect_equal(f3$terms[[1]]$params$upper, 45)

  # Explicit bounds mode (named)
  f4 <- parse_raking_formula(~ rr_range(income, lower = 45000, upper = 55000))
  expect_equal(f4$terms[[1]]$type, "range")
  expect_equal(f4$terms[[1]]$params$mode, "bounds")
  expect_equal(f4$terms[[1]]$params$lower, 45000)
  expect_equal(f4$terms[[1]]$params$upper, 55000)

  # Named vector margin for level-specific constraints
  f5 <- parse_raking_formula(~ rr_range(sex, c(Female = 0.02, Male = 0.03)))
  expect_equal(f5$terms[[1]]$type, "range")
  expect_equal(f5$terms[[1]]$params$mode, "margin")
  expect_equal(f5$terms[[1]]$params$margin, c(Female = 0.02, Male = 0.03))

  # rr_between is an alias
  f6 <- parse_raking_formula(~ rr_between(age, 40, 45))
  expect_equal(f6$terms[[1]]$type, "range")
  expect_equal(f6$terms[[1]]$params$mode, "bounds")
  expect_equal(f6$terms[[1]]$params$lower, 40)
  expect_equal(f6$terms[[1]]$params$upper, 45)

  # Interaction with margin
  f7 <- parse_raking_formula(~ rr_range(sex:region, 0.02))
  expect_equal(f7$terms[[1]]$type, "range")
  expect_equal(sort(f7$terms[[1]]$variables), c("region", "sex"))
  expect_equal(length(f7$terms[[1]]$interaction), 2)
  expect_equal(f7$terms[[1]]$params$mode, "margin")
  expect_equal(f7$terms[[1]]$params$margin, 0.02)

  # Combined with other constraint types
  f8 <- parse_raking_formula(~ rr_exact(sex) + rr_range(age, 40, 45))
  expect_equal(length(f8$terms), 2)
  expect_equal(f8$terms[[1]]$type, "exact")
  expect_equal(f8$terms[[2]]$type, "range")

  # Errors on missing arguments
  expect_error(
    parse_raking_formula(~ rr_range(age)),
    "rr_range requires at least 2 arguments"
  )

  # Errors on invalid margin (must be positive)
  expect_error(
    parse_raking_formula(~ rr_range(age, -0.02)),
    "margin must be a positive number"
  )

  # Errors on lower >= upper
  expect_error(
    parse_raking_formula(~ rr_range(age, 45, 40)),
    "lower must be less than upper"
  )
  expect_error(
    parse_raking_formula(~ rr_range(age, 40, 40)),
    "lower must be less than upper"
  )
})

test_that("interactions are handled correctly", {
  # Simple interaction
  f1 <- parse_raking_formula(~ race:age)
  expect_equal(f1$terms[[1]]$type, "exact")
  expect_equal(sort(f1$terms[[1]]$variables), c("age", "race"))
  expect_equal(length(f1$terms[[1]]$interaction), 2)

  # Interaction within constraint
  f2 <- parse_raking_formula(~ rr_l2(race:age))
  expect_equal(f2$terms[[1]]$type, "l2")
  expect_equal(sort(f2$terms[[1]]$variables), c("age", "race"))
  expect_equal(length(f2$terms[[1]]$interaction), 2)

  # Multiple interactions
  f3 <- parse_raking_formula(~ race:age + education:income)
  expect_equal(length(f3$terms), 2)
  expect_equal(length(f3$interactions), 2)
})

test_that("complex formulas are parsed correctly", {
  f <- parse_raking_formula(
    ~ race + rr_l2(age:education) + rr_kl(income) + state:region
  )

  expect_equal(length(f$terms), 4)
  expect_equal(f$terms[[1]]$type, "exact")
  expect_equal(f$terms[[2]]$type, "l2")
  expect_equal(f$terms[[3]]$type, "kl")
  expect_equal(f$terms[[4]]$type, "exact")

  expect_equal(
    sort(f$variables),
    sort(c("race", "age", "education", "income", "state", "region"))
  )

  expect_equal(length(f$interactions), 2) # age:education and state:region
})

test_that("term IDs are unique", {
  # Now expect a warning for overlapping variables
  expect_warning(
    f <- parse_raking_formula(~ race + age + race:age),
    "Variables in rr_exact\\(race:age\\) also appear as main effects. Using exact constraints for main effects and rr_exact constraint for the interaction term"
  )
  ids <- vapply(f$terms, function(t) t$term_id, character(1))
  expect_equal(length(unique(ids)), 3)
})

test_that("error conditions and edge cases are handled appropriately", {
  # Not a formula
  expect_error(
    parse_raking_formula("race + age"),
    "'formula' must be a formula"
  )

  # Formula with only intercept
  expect_error(parse_raking_formula(~1), "Empty formula")

  # Invalid constraint type (should default to exact)
  expect_warning(
    f <- parse_raking_formula(~ unknown(age)),
    "Unknown function 'unknown', defaulting to exact constraint"
  )
  expect_equal(f$terms[[1]]$type, "exact")
  expect_equal(f$terms[[1]]$variables, "age")

  # Nested functions
  f2 <- parse_raking_formula(~ rr_l2(rr_exact(age)))
  expect_equal(f2$terms[[1]]$type, "l2")
  expect_equal(f2$terms[[1]]$variables, "age")

  # Duplicate terms with same type are allowed
  f3 <- parse_raking_formula(~ age + age)
  expect_equal(length(f3$terms), 2)
  expect_equal(length(f3$variables), 1)
})

test_that("print method works correctly", {
  f <- parse_raking_formula(~ race + rr_l2(age:education) + rr_kl(income))
  expect_output(print(f), "Raking Formula Specification:")
  expect_output(print(f), "Variables:")
  expect_output(print(f), "Terms:")
})

test_that("formula environment is preserved", {
  age <- "not_the_variable" # This should not affect the parsing
  f <- parse_raking_formula(~ age + education)
  expect_equal(f$terms[[1]]$variables, "age")
})

test_that("mixed constraints with overlapping variables work correctly", {
  expect_warning(
    {
      f <- parse_raking_formula(~ age + race + rr_l2(age:race))
      f
    },
    "Variables in rr_l2\\(age:race\\) also appear as main effects. Using exact constraints for main effects and rr_l2 constraint for the interaction term"
  )

  # Should have 3 terms
  expect_equal(length(f$terms), 3)

  # First two terms should be exact constraints
  expect_equal(f$terms[[1]]$type, "exact")
  expect_equal(f$terms[[1]]$variables, "age")
  expect_equal(f$terms[[2]]$type, "exact")
  expect_equal(f$terms[[2]]$variables, "race")

  # Last term should be l2 constraint with interaction
  expect_equal(f$terms[[3]]$type, "l2")
  expect_equal(sort(f$terms[[3]]$variables), c("age", "race"))
  expect_true(!is.null(f$terms[[3]]$interaction))

  # Variables should include both without duplicates
  expect_equal(sort(f$variables), c("age", "race"))
})

test_that("overlapping constraints are handled appropriately", {
  # Pure duplicates should error
  expect_error(
    parse_raking_formula(~ age + rr_l2(age)),
    "Variable 'age' appears multiple times with different constraints"
  )
  expect_error(
    parse_raking_formula(~ race:age + rr_l2(race:age)),
    "Interaction 'race:age' appears multiple times with different constraints"
  )

  # Partial overlaps should warn but use the appropriate versions
  expect_warning(
    f <- parse_raking_formula(~ age + race + rr_l2(age:race)),
    "Variables in rr_l2\\(age:race\\) also appear as main effects. Using exact constraints for main effects and rr_l2 constraint for the interaction term"
  )

  # Check that we kept the earlier main effects
  expect_equal(length(f$terms), 3)
  expect_equal(f$terms[[1]]$type, "exact")
  expect_equal(f$terms[[1]]$variables, "age")
  expect_equal(f$terms[[2]]$type, "exact")
  expect_equal(f$terms[[2]]$variables, "race")

  # And the later interaction
  expect_equal(f$terms[[3]]$type, "l2")
  expect_equal(sort(f$terms[[3]]$variables), c("age", "race"))
  expect_true(!is.null(f$terms[[3]]$interaction))
})

test_that("n-way interactions are handled correctly", {
  # 3-way interaction
  f1 <- parse_raking_formula(~ a:b:c)
  expect_equal(f1$terms[[1]]$type, "exact")
  expect_equal(sort(f1$terms[[1]]$variables), c("a", "b", "c"))
  expect_equal(length(f1$terms[[1]]$interaction), 3)

  # 3-way interaction with constraint
  f2 <- parse_raking_formula(~ rr_l2(a:b:c))
  expect_equal(f2$terms[[1]]$type, "l2")
  expect_equal(sort(f2$terms[[1]]$variables), c("a", "b", "c"))
  expect_equal(length(f2$terms[[1]]$interaction), 3)

  # 4-way interaction
  f3 <- parse_raking_formula(~ a:b:c:d)
  expect_equal(f3$terms[[1]]$type, "exact")
  expect_equal(sort(f3$terms[[1]]$variables), c("a", "b", "c", "d"))
  expect_equal(length(f3$terms[[1]]$interaction), 4)

  # Mixed n-way interactions
  f4 <- parse_raking_formula(~ a:b:c + d:e + f)
  expect_equal(length(f4$terms), 3)
  expect_equal(length(f4$terms[[1]]$interaction), 3) # 3-way
  expect_equal(length(f4$terms[[2]]$interaction), 2) # 2-way
  expect_null(f4$terms[[3]]$interaction) # main effect
})
