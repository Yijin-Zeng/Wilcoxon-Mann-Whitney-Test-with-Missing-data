test_that("boundsSumRanks works without ties", {

  expect_equal(boundsSumRank(X = c(0.1, 1.1, 1.5), Y = c(2.1, 3.5),
                             ties = FALSE), c(6, 6))

  expect_equal(boundsSumRank(X = c(0.1, 1.1, NA), Y = c(2.1, 3.5),
                             ties = FALSE), c(6, 8))
})

test_that("boundsSumRanks works with ties", {
  expect_equal(boundsSumRank(X = c(1, 1, NA), Y = c(2.1, 3.5),
                             ties = TRUE, lower.boundary = 1,
                             upper.boundary = 3.5), c(6, 7.5))

  expect_equal(boundsSumRank(X = c(1, 1, NA), Y = c(2.1, 3.5),
                             ties = TRUE, lower.boundary = 0,
                             upper.boundary = 4), c(6, 8))
})
