# generate samples (with ties) for testing
set.seed(1)
accuary <- 5 # compare the first 5 digits of test and expect results
n <- 50
m <- 50
X <- rpois(n,1)
Y <- rpois(m,1)

#################### testing: no missing data case ###########################
test_that("no missing data case: produce the same test statistic", {
  # While no missing data is presented, the boundsPValueWithTies is expected
  # to produce exactly the same result as stats::wilcox.test().
  resTestTwo <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                     lower.boundary = -Inf,
                                     upper.boundary = Inf,
                                     exact = FALSE, correct = TRUE)
  resExpcTwo <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                   exact = FALSE)
  ### both lower and upper bounds equal to resExpcTwo
  expect_equal(round(resTestTwo[1],accuary),
               round(resExpcTwo$statistic[[1]],accuary))
  expect_equal(round(resTestTwo[2],accuary),
               round(resExpcTwo$statistic[[1]],accuary))
})


test_that("no missing data case: produce the same p-value
          (alternative = two.sided, correct = TRUE)", {
  resTestTwo <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                     lower.boundary = -Inf,
                                     upper.boundary = Inf,
                                     exact = FALSE, correct = TRUE)
  resExpcTwo <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                   exact = FALSE)
  ### two sided (correct = TRUE)
  expect_equal(round(resTestTwo[3],accuary),
               round(resExpcTwo$p.value[[1]],accuary))
  expect_equal(round(resTestTwo[4],accuary),
               round(resExpcTwo$p.value[[1]],accuary))
})


test_that("no missing data case: produce the same p-value
          (alternative = two.sided, correct = FALSE)", {
            ### two sided (correct = FALSE)
  resTestTwo <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                     lower.boundary = -Inf,
                                     upper.boundary = Inf,
                                     exact = FALSE, correct = FALSE)
  resExpcTwo <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                   exact = FALSE, correct = FALSE)

  expect_equal(round(resTestTwo[3],accuary),
               round(resExpcTwo$p.value[[1]],accuary))
  expect_equal(round(resTestTwo[4],accuary),
               round(resExpcTwo$p.value[[1]],accuary))
})


test_that("no missing data case: produce the same p-value
          (alternative = less, correct = TRUE)", {
 ## less (correct = TRUE)
  resTestLess <- boundsPValueWithTies(X,Y, alternative = 'less',
                                      lower.boundary = -Inf,
                                      upper.boundary = Inf,
                                      exact = FALSE, correct = TRUE)
  resExpcLess <- stats::wilcox.test(X,Y, alternative = 'less',
                                    exact = FALSE)
  expect_equal(round(resTestLess[3],accuary),
               round(resExpcLess$p.value[[1]],accuary))
  expect_equal(round(resTestLess[4],accuary),
               round(resExpcLess$p.value[[1]],accuary))
})


test_that("no missing data case: produce the same p-value
          (alternative = less, correct = FALSE)", {
            ## less (correct = FALSE)
  resTestLess <- boundsPValueWithTies(X,Y, alternative = 'less',
                                      lower.boundary = -Inf,
                                      upper.boundary = Inf,
                                      exact = FALSE, correct = FALSE)
  resExpcLess <- stats::wilcox.test(X,Y, alternative = 'less',
                                    exact = FALSE, correct = FALSE)
  expect_equal(round(resTestLess[3],accuary),
               round(resExpcLess$p.value[[1]],accuary))
  expect_equal(round(resTestLess[4],accuary),
               round(resExpcLess$p.value[[1]],accuary))
})


test_that("no missing data case: produce the same p-value
          (alternative = greater, correct = TRUE)", {
  ## greater (correct = TRUE)
  resTestGreat <- boundsPValueWithTies(X,Y, alternative = 'greater',
                                       lower.boundary = -Inf,
                                       upper.boundary = Inf,
                                       exact = FALSE, correct = TRUE)
  resExpcGreat <- stats::wilcox.test(X,Y, alternative = 'greater',
                                     exact = FALSE)
  expect_equal(round(resTestGreat[3],accuary),
               round(resExpcGreat$p.value[[1]],accuary))
  expect_equal(round(resTestGreat[4],accuary),
               round(resExpcGreat$p.value[[1]],accuary))
})


test_that("no missing data case: produce the same p-value
          (alternative = greater, correct = FALSE)", {

  ## greater (correct = FALSE)
  resTestGreat <- boundsPValueWithTies(X,Y, alternative = 'greater',
                                       lower.boundary = -Inf,
                                       upper.boundary = Inf,
                                       exact = FALSE, correct = FALSE)
  resExpcGreat <- stats::wilcox.test(X,Y, alternative = 'greater',
                                     exact = FALSE, correct = FALSE)
  expect_equal(round(resTestGreat[3],accuary),
               round(resExpcGreat$p.value[[1]],accuary))
  expect_equal(round(resTestGreat[4],accuary),
               round(resExpcGreat$p.value[[1]],accuary))
})


############################ testing: missing data case ######################
## Special imputation one: result minimum possible WMW test statistic
## impute each missing sample in X as 0 (potentially smallest value)
## impute each missing sample in Y as a value **larger** than all
## oberved samples
ImputedXOne <- X
ImputedXOne[is.na(ImputedXOne)] <- 0
ImputedYOne <- Y
ImputedYOne[is.na(ImputedYOne)] <- max(c(X[!is.na(X)], Y[!is.na(Y)])) + 1
## Special imputation two: result maximum possible WMW test statistic
## impute each missing sample in X as a value **larger** than all
## oberved samples
## impute each missing sample in Y as 0 (potentially smallest value)
ImputedXTwo <- X
ImputedXTwo[is.na(ImputedXTwo)] <- max(c(X[!is.na(X)], Y[!is.na(Y)])) + 1
ImputedYTwo <- Y
ImputedYTwo[is.na(ImputedYTwo)] <- 0

## compute test results of the two special imputations
resExpcTwoOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                    alternative = 'two.sided',
                                    exact = FALSE)

resExpcTwoTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                    alternative = 'two.sided',
                                    exact = FALSE)

test_that("missing data case: check the bounds of test statistic
          (lower.boundary = -Inf)", {
  # If one specifies lower.boundary = -Inf
  resTestTwoBroader <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                            lower.boundary = -Inf,
                                            upper.boundary = Inf,
                                            exact = FALSE, correct = TRUE)
  # broader bounds should bound the special cases
  expect_lte(round(resTestTwoBroader[1],accuary),
             round(resExpcTwoOne$statistic[[1]],accuary))

  expect_gte(round(resTestTwoBroader[2],accuary),
             round(resExpcTwoTwo$statistic[[1]],accuary))
})

test_that("missing data case: check the bounds of test statistic
          (lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])))", {
  # If one specifies lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)]))
  resTestTwoTighter <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                            lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])),
                                            upper.boundary = Inf,
                                            exact = FALSE, correct = TRUE)
  # tighter bounds should be equal to the special cases
  expect_equal(round(resTestTwoTighter[1],accuary),
               round(resExpcTwoOne$statistic[[1]],accuary))

  expect_equal(round(resTestTwoTighter[2],accuary),
               round(resExpcTwoTwo$statistic[[1]],accuary))
})


test_that("missing data case: check the bounds of p-value
          (alternative = two.sided, lower.boundary = -Inf)", {
  # If one specifies lower.boundary = -Inf
  resTestTwoBroader <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                            lower.boundary = -Inf,
                                            upper.boundary = Inf,
                                            exact = FALSE, correct = TRUE)
  ### two sided (correct = TRUE)
  expected_p1 <- resExpcTwoOne$p.value[[1]]
  expected_p2 <- resExpcTwoTwo$p.value[[1]]
  expected_minp <- min(expected_p1, expected_p2)

  if( ((resExpcTwoOne$statistic[[1]] - n*m/2) *
       (resExpcTwoTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expected_p1, expected_p2)
  }

  # broader bounds should bound tighter bounds
  expect_lte(round(resTestTwoBroader[3],accuary),
             round(expected_minp, accuary))
  expect_gte(round(resTestTwoBroader[4],accuary),
             round(expected_maxp, accuary))

})

test_that("missing data case: check the bounds of p-value
          (alternative = two.sided,
          lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])))", {
  # If one specifies lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)]))
  resTestTwoTighter <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                            lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])),
                                            upper.boundary = Inf,
                                            exact = FALSE, correct = TRUE)

  expected_p1 <- resExpcTwoOne$p.value[[1]]
  expected_p2 <- resExpcTwoTwo$p.value[[1]]
  expected_minp <- min(expected_p1, expected_p2)

  if( ((resExpcTwoOne$statistic[[1]] - n*m/2) *
       (resExpcTwoTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expected_p1, expected_p2)
  }

  # tighter bounds should bound special cases
  expect_lte(round(resTestTwoTighter[3],accuary),
             round(expected_minp,accuary))
  expect_gte(round(resTestTwoTighter[4],accuary),
             round(expected_maxp,accuary))
})
