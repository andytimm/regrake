library(testthat)

test_that("setup works", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  lambda <- 1
  lower <- -0.3
  upper <- 0.3

  expect_length(f, m)
  expect_length(fdes, m)
})

test_that("equality loss matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  lambda <- 1

  # Direct loss calculation
  direct_loss <- sum(equality_loss(f, fdes))
  
  # Optimization solution using CVXR
  library(CVXR)
  x <- Variable(m)
  objective <- Minimize(sum(abs(x - fdes)))
  problem <- Problem(objective)
  result <- CVXR::solve(problem)
  opt_x <- as.vector(result$getValue(x))
  
  expect_equal(opt_x, fdes, tolerance = 1e-5)
  expect_equal(sum(equality_loss(opt_x, fdes)), 0, tolerance = 1e-5)
})

test_that("l2 loss matches optimization solution", {
  set.seed(605)
  m <- 10
  f <- rnorm(m)
  fdes <- rnorm(m)
  lambda <- 1

  # Direct loss calculation
  direct_loss <- sum(l2_loss(f, fdes))
  
  # Optimization solution using CVXR
  library(CVXR)
  x <- Variable(m)
  objective <- Minimize(sum((x - fdes)^2))  # Squared error for L2
  problem <- Problem(objective)
  result <- CVXR::solve(problem)
  opt_x <- as.vector(result$getValue(x))
  
  expect_equal(opt_x, fdes, tolerance = 1e-5)
  expect_equal(sum(l2_loss(opt_x, fdes)), 0, tolerance = 1e-5)
})

test_that("kl loss matches optimization solution", {
  set.seed(605)
  m <- 10
  # Use positive values for KL divergence
  f <- abs(rnorm(m)) + 0.1  
  fdes <- abs(rnorm(m)) + 0.1
  lambda <- 1

  # Direct loss calculation
  direct_loss <- sum(kl_loss(f, fdes))
  
  # Optimization solution using CVXR
  library(CVXR)
  x <- Variable(m)
  # Rewrite objective to be DCP compliant
  objective <- Minimize(sum(-entr(x) - x * log(fdes) - x + fdes))
  constraints <- list(x >= 0)
  problem <- Problem(objective, constraints)
  result <- CVXR::solve(problem)
  opt_x <- as.vector(result$getValue(x))
  
  expect_equal(opt_x, fdes, tolerance = 1e-5)
  expect_equal(sum(kl_loss(opt_x, fdes)), 0, tolerance = 1e-5)
}) 

test_that("loss functions handle zero-length inputs", {
  x <- numeric(0)
  target <- numeric(0)
  
  expect_equal(length(equality_loss(x, target)), 0)
  expect_equal(length(l2_loss(x, target)), 0)
  expect_equal(length(kl_loss(x, target)), 0)
})

test_that("loss functions handle mismatched lengths", {
  x <- c(0.1, 0.2)
  target <- 0.1
  
  expect_error(equality_loss(x, target))
  expect_error(l2_loss(x, target))
  expect_error(kl_loss(x, target))
})

test_that("kl_loss handles edge cases correctly", {
  # Test non-positive values
  expect_equal(kl_loss(0, 1), Inf)
  expect_equal(kl_loss(1, 0), Inf)
  expect_equal(kl_loss(0, 0), Inf)
  
  # Test vectors with mixed valid/invalid values
  x <- c(0.1, 0, 0.3)
  target <- c(0.2, 0.1, 0)
  result <- kl_loss(x, target)
  
  expect_equal(length(result), 3)
  expect_true(is.infinite(result[2]))  # x = 0
  expect_true(is.infinite(result[3]))  # target = 0
  expect_false(is.infinite(result[1])) # both positive
})

test_that("loss functions handle NA/NaN values", {
  x <- c(0.1, NA, NaN)
  target <- c(0.2, 0.3, 0.4)
  
  expect_true(any(is.na(equality_loss(x, target))))
  expect_true(any(is.na(l2_loss(x, target))))
  expect_true(any(is.na(kl_loss(x, target))))
}) 
