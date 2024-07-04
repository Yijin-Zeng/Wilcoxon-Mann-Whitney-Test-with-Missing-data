test_that("boundsWMWStatistic without ties", {
  expect_equal(boundsWMWStatistic(X = c(0.1, 1.1, 1.5), Y = c(2.1, 3.5),
                                  ties = FALSE), c(0, 0))

  expect_equal(boundsWMWStatistic(X = c(0.1, 1.1, NA), Y = c(2.1, 3.5),
                                  ties = FALSE), c(0, 2))
})

test_that('boundsWMWStatistic with ties', {
  expect_equal(boundsWMWStatistic(X = c(1, 1, NA), Y = c(2.1, 3.5),
                                  ties = TRUE, lower.boundary = 1,
                                  upper.boundary = 3.5), c(0, 1.5))

  expect_equal(boundsWMWStatistic(X = c(1, 1, NA), Y = c(2.1, 3.5),
                                  ties = TRUE, lower.boundary = 0,
                                  upper.boundary = 4), c(0, 2))
})
