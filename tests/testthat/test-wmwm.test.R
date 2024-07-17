test_that("bounds of WMW test Statistic without ties", {
  X <- c(6.2, 3.5, NA, 7.6, 9.2)
  Y <- c(0.2, 1.3, -0.5, -1.7)

  RES <- wmwm.test(X,Y, ties = FALSE)
  # minimum possible WMW test statistic
  expect_equal(RES$bounds.statistic[1], 16)
  # maximum possible WMW test statistic
  expect_equal(RES$bounds.statistic[2], 20)
})

test_that("check ties are not considered", {
  X <- c(6.2, 3.5, NA, 7.6, 9.2)
  Y <- c(0.2, 1.3, -0.5, -1.7)
  RES <- wmwm.test(X,Y, ties = FALSE)
  expect_equal(RES$ties.method, FALSE)
})

test_that("bounds of WMW test Statistic with ties
          without specifiying lower.boundary and upper.boundary", {
  X <- c(6, 9, NA, 7, 9)
  Y <- c(0, 1, 0, -1)

  RES <- suppressWarnings(wmwm.test(X,Y, ties = TRUE))
  # minimum possible WMW test statistic
  expect_equal(RES$bounds.statistic[1], 16)
  # maximum possible WMW test statistic
  expect_equal(RES$bounds.statistic[2], 20)
})

test_that("bounds of WMW test Statistic with ties
           specifiying lower.boundary and upper.boundary", {
  X <- c(6, 9, NA, 7, 9)
  Y <- c(0, 1, 0, -1)

  RES <- suppressWarnings(wmwm.test(X,Y, ties = TRUE, lower.boundary = -1, upper.boundary = 9))
  # minimum possible WMW test statistic
  expect_equal(RES$bounds.statistic[1], 16.5)
  # maximum possible WMW test statistic
  expect_equal(RES$bounds.statistic[2], 20)
})

test_that("check ties are considered", {
  X <- c(6, 9, NA, 7, 9)
  Y <- c(0, 1, 0, -1)
  RES <- suppressWarnings(wmwm.test(X,Y, ties = TRUE))
  expect_equal(RES$ties.method, TRUE)
})
