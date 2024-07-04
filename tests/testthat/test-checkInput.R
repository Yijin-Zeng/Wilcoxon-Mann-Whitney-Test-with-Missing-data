test_that("check number of valid inputs", {
  X <- 'a'
  Y <- c(0,1)
  # Remove all infinite
  X <- X[is.finite(X) | is.na(X)]
  Y <- Y[is.finite(Y) | is.na(Y)]
  expect_error(checkInput(X,Y),
               "not enough valid samples in 'X' or 'Y'")
})


test_that("check non-numerical inputs", {
  X <- TRUE
  Y <- c(0,1)
  # Remove all infinite
  X <- X[is.finite(X) | is.na(X)]
  Y <- Y[is.finite(Y) | is.na(Y)]
  expect_error(checkInput(X,Y),
               "observed samples must be numeric")
})
