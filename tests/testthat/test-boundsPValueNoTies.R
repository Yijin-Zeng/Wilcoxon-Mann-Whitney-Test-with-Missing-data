## generate samples (without ties) for testing
set.seed(1)
accuary <- 5 # compare the first 5 digits of test and expect results
n <- 50
m <- 50
X <- rnorm(n,0,1)
Y <- rnorm(m,0,1)

#################### testing: no missing data case ###########################
test_that("no missing data case: produce the same test statistic", {
  # While no missing data is presented, the boundsPValueNoTies is expected
  # to produce exactly the same result as stats::wilcox.test().
  resTestTwoExact <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = TRUE, correct = TRUE)
  resExpcTwoExact <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                        exact = TRUE)

  expect_equal(round(resTestTwoExact[1],accuary),
               round(resExpcTwoExact$statistic[[1]],accuary))

  expect_equal(round(resTestTwoExact[2],accuary),
               round(resExpcTwoExact$statistic[[1]],accuary))

})

test_that("no missing data case: produce the same p-value
          (alternative = two.sided, exact = TRUE)", {
    # While no missing data is presented, the boundsPValueNoTies is expected
    # to produce exactly the same result as stats::wilcox.test().
    resTestTwoExact <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                          exact = TRUE, correct = TRUE)
    resExpcTwoExact <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                          exact = TRUE)

    ### two sided; exact
    expect_equal(round(resTestTwoExact[3],accuary),
                 round(resExpcTwoExact$p.value[[1]],accuary))
    expect_equal(round(resTestTwoExact[4],accuary),
                 round(resExpcTwoExact$p.value[[1]],accuary))
  })


test_that("no missing data case: produce the same p-value
          (alternative = two.sided, correct = TRUE)", {
  # While no missing data is presented, the boundsPValueNoTies is expected
  # to produce exactly the same result as stats::wilcox.test().
  resTestTwoExact <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = TRUE, correct = TRUE)
  resExpcTwoExact <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                        exact = TRUE)

  ### two sided; approximation (correct = TRUE)
  resTestTwoAppro <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = TRUE)
  resExpcTwoAppro <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                        exact = FALSE)
  expect_equal(round(resTestTwoAppro[3],accuary),
               round(resExpcTwoAppro$p.value[[1]],accuary))
  expect_equal(round(resTestTwoAppro[4],accuary),
               round(resExpcTwoAppro$p.value[[1]],accuary))
})

test_that("no missing data case: produce the same p-value
          (alternative = two.sided, correct = FALSE)", {

  ### two sided; approximation (correct = FALSE)
  resTestTwoAppro <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = FALSE)
  resExpcTwoAppro <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = FALSE)
  expect_equal(round(resTestTwoAppro[3],accuary),
               round(resExpcTwoAppro$p.value[[1]],accuary))
  expect_equal(round(resTestTwoAppro[4],accuary),
               round(resExpcTwoAppro$p.value[[1]],accuary))
})

test_that("no missing data case: produce the same p-value
          (alternative = less, exact = TRUE)", {
            ## less; exact
  resTestLessExact <- boundsPValueNoTies(X,Y, alternative = 'less',
                                         exact = TRUE, correct = TRUE)
  resExpcLessExact <- stats::wilcox.test(X,Y, alternative = 'less',
                                         exact = TRUE)
  expect_equal(round(resTestLessExact[3],accuary),
               round(resExpcLessExact$p.value[[1]],accuary))
  expect_equal(round(resTestLessExact[4],accuary),
               round(resExpcLessExact$p.value[[1]],accuary))
          })


test_that("no missing data case: produce the same p-value
          (alternative = greater, exact = TRUE)", {

  ## greater; exact
  resTestGreatExact <- boundsPValueNoTies(X,Y, alternative = 'greater',
                                          exact = TRUE, correct = TRUE)
  resExpcGreatExact <- stats::wilcox.test(X,Y, alternative = 'greater',
                                          exact = TRUE)
  expect_equal(round(resTestGreatExact[3],accuary),
               round(resExpcGreatExact$p.value[[1]],accuary))
  expect_equal(round(resTestGreatExact[4],accuary),
               round(resExpcGreatExact$p.value[[1]],accuary))

          })

############################ testing: missing data case ######################
## Special imputation one: result minimum possible WMW test statistic
## impute each missing sample in X as a value **smaller** than all
## oberved samples
## impute each missing sample in Y as a value **larger** than all
## oberved samples
ImputedXOne <- X
ImputedXOne[is.na(ImputedXOne)] <- min(c(X[!is.na(X)], Y[!is.na(Y)])) - abs(rnorm(10,0,1))
ImputedYOne <- Y
ImputedYOne[is.na(ImputedYOne)] <- max(c(X[!is.na(X)], Y[!is.na(Y)])) + abs(rnorm(5,0,1))

## Special imputation two: result maximum possible WMW test statistic
## impute each missing sample in X as a value **larger** than all oberved samples
## impute each missing sample in Y as a value **smaller** than all oberved samples
ImputedXTwo <- X
ImputedXTwo[is.na(ImputedXTwo)] <- max(c(X[!is.na(X)], Y[!is.na(Y)])) + abs(rnorm(10,0,1))
ImputedYTwo <- Y
ImputedYTwo[is.na(ImputedYTwo)] <- min(c(X[!is.na(X)], Y[!is.na(Y)])) - abs(rnorm(5,0,1))


## compute test results of the two special imputations
resExpcTwoExactOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                         alternative = 'two.sided',
                                         exact = TRUE)
resExpcTwoExactTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                         alternative = 'two.sided',
                                         exact = TRUE)

test_that("missing data case: check the bounds of test statistics", {

  ###############################Test Statistic#################################

  # Check the bounds of the WMW test statistic, which is not depended on
  # alternative or exact
  resTestTwoExact <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = TRUE, correct = TRUE)

  expect_equal(round(resTestTwoExact[1],accuary),
               round(resExpcTwoExactOne$statistic[[1]],accuary))

  expect_equal(round(resTestTwoExact[2],accuary),
               round(resExpcTwoExactTwo$statistic[[1]],accuary))
})

test_that("missing data case: check the bounds of p-value
          (exact = TRUE)", {
  resTestTwoExact <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = TRUE, correct = TRUE)

  ### two sided; exact
  expected_p1 <- resExpcTwoExactOne$p.value[[1]]
  expected_p2 <- resExpcTwoExactTwo$p.value[[1]]
  expected_minp <- min(expected_p1, expected_p2)

  if( ((resExpcTwoExactOne$statistic[[1]] - n*m/2) *
       (resExpcTwoExactTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expected_p1, expected_p2)
  }

  expect_equal(resTestTwoExact[3], expected_minp)
  expect_equal(resTestTwoExact[4], expected_maxp)
})


test_that("missing data case: check the bounds of p-value
          (exact = FALSE)", {

  ### two sided; approximation (correct = TRUE)
  resTestTwoAppro <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = TRUE)
  resExpcTwoApproOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                           alternative = 'two.sided',
                                           exact = FALSE)
  resExpcTwoApproTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                           alternative = 'two.sided',
                                           exact = FALSE)

  expected_p1 <- resExpcTwoApproOne$p.value[[1]]
  expected_p2 <- resExpcTwoApproTwo$p.value[[1]]
  expected_minp <- min(expected_p1, expected_p2)

  if( ((resExpcTwoApproOne$statistic[[1]] - n*m/2) *
       (resExpcTwoApproTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expected_p1, expected_p2)
  }

  expect_equal(resTestTwoAppro[3], expected_minp)
  expect_equal(resTestTwoAppro[4], expected_maxp)
})

