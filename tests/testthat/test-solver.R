test_that("projection_simplex maintains sum constraint", {
  set.seed(605)
  n <- 1000
  v <- rnorm(n)
  z <- 1.5  # test non-unit sum constraint

  result <- projection_simplex(v, z)

  expect_equal(sum(result), z, tolerance = 1e-6)
  expect_true(all(result >= 0))
  expect_equal(length(result), length(v))
})

test_that("projection_simplex handles edge cases", {
  # All negative values - should put weight on least negative
  v1 <- c(-1, -2, -3)
  expect_equal(projection_simplex(v1), c(1, 0, 0))

  # Already a probability vector
  v2 <- c(0.3, 0.5, 0.2)
  expect_equal(projection_simplex(v2), v2)

  # Single very large value
  v3 <- c(100, 0.1, 0.1)
  result3 <- projection_simplex(v3)
  expect_true(result3[1] > result3[2])
  expect_equal(sum(result3), 1)
})