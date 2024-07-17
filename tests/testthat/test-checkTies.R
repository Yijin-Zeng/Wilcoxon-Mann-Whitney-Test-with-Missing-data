test_that("expect warning when observed samples are tied but ties are
          specified to FALSE", {
            X <- c(1,1,2,5)
            Y <- c(NA,NA, 2, 3, 5)
            expect_warning(checkTies(X,Y,ties = FALSE),
                           "observed samples are tied, you may want to specify ties = TRUE")

          })

test_that("expect warning when all samples are observed as unique numbers
          but ties are specified to TRUE", {
            X <- rnorm(10,0,1)
            Y <- rnorm(10,0,1)
            expect_warning(checkTies(X,Y,ties = TRUE),
                           "all samples are observed distinct numbers, ties can only be FALSE")
          })


test_that("expected warnings when all samples are observed and
          tied samples exist, but ties are specified to FALSE", {
            X <- c(1,1,2,5)
            Y <- c(2,3,5)
            warnings <- capture_warnings(checkTies(X,Y,ties = FALSE))
            expect_equal(warnings[1], "observed samples are tied, you may want to specify ties = TRUE")
            expect_equal(warnings[2], "all samples are observed with tied observations, ties can only be TRUE")
          })
