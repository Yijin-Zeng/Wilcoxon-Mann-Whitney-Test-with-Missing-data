test_that("check inputs", {
  # Test random inputs, X and Y should be numeric vectors of data
  # with potential missing, i.e. NA

  X <- 'a'
  Y <- c(0,1)
  expect_error(wmwm.test(X,Y),
               "not enough finite or missing samples in 'X' or 'Y'")

  X <- TRUE
  Y <- c(0,1)
  expect_error(wmwm.test(X,Y),
               "observed samples must be numeric")

  X <- c(NA, NA)
  Y <- c(NA, NA)
  expect_warning(wmwm.test(X,Y),
                 "either 'X' or 'Y' does not contain any observed sample")

})


test_that("check ties", {
  # If observed samples are tied, then ties will automatically and must be TRUE
  # If observed samples are distinct numbers, then ties could be TRUE. However,
  # if no missing data is presented and all samples are distinct numbers, ties must
  # be FALSE

  X <- c(1,1,2,5)
  Y <- c(NA,NA, 2, 3, 5)
  expect_warning(wmwm.test(X,Y,ties = FALSE, exact = FALSE),
                 "observed samples are tied, you may want to specify ties = TRUE")


  X <- rnorm(10,0,1)
  Y <- rnorm(10,0,1)
  expect_warning(wmwm.test(X,Y,ties = TRUE),
                 "all samples are observed distinct numbers, ties can only be FALSE")


  X <- c(1,1,2,5)
  Y <- c(2,3,5)
  expect_warning(wmwm.test(X,Y,ties = FALSE),
                 "all samples are observed with tied observations, ties can only be TRUE")
})
