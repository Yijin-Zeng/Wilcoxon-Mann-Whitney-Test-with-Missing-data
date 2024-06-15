test_that("no missing data case", {
  # While no missing data is presented, the boundsPValueWithTies is expected
  # to produce exactly the same result as stats::wilcox.test().

  set.seed(1)
  accuary <- 5 # compare the first 5 digits of test and expect results

  # samples used for testing
  n <- 50
  m <- 50
  X <- rpois(n,1)
  Y <- rpois(m,1)

  ###############################Test Statistic#################################

  # Check the bounds of the WMW test statistic, which is not depended on
  # alternative or exact
  resTestTwo <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                     lower.boundary = -Inf,
                                     upper.boundary = Inf,
                                     exact = FALSE, correct = TRUE)
  resExpcTwo <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                   exact = FALSE)

  expect_equal(round(resTestTwo[1],accuary),
               round(resExpcTwo$statistic[[1]],accuary))

  expect_equal(round(resTestTwo[2],accuary),
               round(resExpcTwo$statistic[[1]],accuary))


  ###################################p-value####################################

  ### two sided (correct = TRUE)
  expect_equal(round(resTestTwo[3],accuary),
               round(resExpcTwo$p.value[[1]],accuary))
  expect_equal(round(resTestTwo[4],accuary),
               round(resExpcTwo$p.value[[1]],accuary))

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


test_that("missing data case", {

  set.seed(1)
  accuary <- 5 # compare the first 5 digits of test and expect results

  # Samples used for testing
  n <- 50
  m <- 50
  X <- rpois(n,1)
  Y <- rpois(m,1)
  X[1:10] <- NA
  Y[1:5] <- NA

  ## Special imputation one
  ## impute each missing sample in X as 0 (potentially smallest value)
  ## impute each missing sample in Y as a value **larger** than all oberved samples
  ImputedXOne <- X
  ImputedXOne[is.na(ImputedXOne)] <- 0
  ImputedYOne <- Y
  ImputedYOne[is.na(ImputedYOne)] <- max(c(X[!is.na(X)], Y[!is.na(Y)])) + 1

  ## Special imputation two
  ## impute each missing sample in X as a value **larger** than all oberved samples
  ## impute each missing sample in Y as 0 (potentially smallest value)
  ImputedXTwo <- X
  ImputedXTwo[is.na(ImputedXTwo)] <- max(c(X[!is.na(X)], Y[!is.na(Y)])) + 1
  ImputedYTwo <- Y
  ImputedYTwo[is.na(ImputedYTwo)] <- 0


  ###############################Test Statistic#################################

  # Check the bounds of the WMW test statistic, which is not depended on
  # alternative or exact


  # If one specifies lower.boundary = -Inf
  resTestTwoBroader <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                            lower.boundary = -Inf,
                                            upper.boundary = Inf,
                                            exact = FALSE, correct = TRUE)

  # If one specifies lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)]))
  resTestTwoTighter <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                            lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])),
                                            upper.boundary = 'larger',
                                            exact = FALSE, correct = TRUE)

  resExpcTwoOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                      alternative = 'two.sided',
                                      exact = FALSE)

  resExpcTwoTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                      alternative = 'two.sided',
                                      exact = FALSE)

  # broader bounds should bound the special cases
  expect_lte(round(resTestTwoBroader[1],accuary),
             round(resExpcTwoOne$statistic[[1]],accuary))

  expect_gte(round(resTestTwoBroader[2],accuary),
             round(resExpcTwoTwo$statistic[[1]],accuary))

  # tighter bounds should be equal to the special cases
  expect_equal(round(resTestTwoTighter[1],accuary),
               round(resExpcTwoOne$statistic[[1]],accuary))

  expect_equal(round(resTestTwoTighter[2],accuary),
               round(resExpcTwoTwo$statistic[[1]],accuary))

  ###################################p-value####################################

  ### two sided (correct = TRUE)
  expcted_p1 <- resExpcTwoOne$p.value[[1]]
  expected_p2 <- resExpcTwoTwo$p.value[[1]]
  expected_minp <- min(expcted_p1, expected_p2)

  if( ((resExpcTwoOne$statistic[[1]] - n*m/2) *
       (resExpcTwoTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expcted_p1, expcted_p2)
  }

  # tighter bounds should bound special cases
  expect_lte(round(resTestTwoTighter[3],accuary),
             round(expected_minp,accuary))
  expect_gte(round(resTestTwoTighter[4],accuary),
             round(expected_maxp,accuary))

  # broader bounds should bound tighter bounds
  expect_lte(round(resTestTwoBroader[3],accuary),
             round(resTestTwoTighter[3],accuary))
  expect_gte(round(resTestTwoBroader[4],accuary),
             round(resTestTwoTighter[4],accuary))

  ### two sided (correct = FALSE)
  resTestTwoBroader <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                            lower.boundary = -Inf,
                                            upper.boundary = Inf,
                                            exact = FALSE, correct = FALSE)

  resTestTwoTighter <- boundsPValueWithTies(X,Y, alternative = 'two.sided',
                                            lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])),
                                            upper.boundary = 'larger',
                                            exact = FALSE, correct = FALSE)

  resExpcTwoOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                      alternative = 'two.sided',
                                      exact = FALSE, correct = FALSE)

  resExpcTwoTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                      alternative = 'two.sided',
                                      exact = FALSE, correct = FALSE)

  expcted_p1 <- resExpcTwoOne$p.value[[1]]
  expected_p2 <- resExpcTwoTwo$p.value[[1]]
  expected_minp <- min(expcted_p1, expected_p2)

  if( ((resExpcTwoOne$statistic[[1]] - n*m/2) *
       (resExpcTwoTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expcted_p1, expcted_p2)
  }

  # tighter bounds should bound special cases
  expect_lte(round(resTestTwoTighter[3],accuary),
             round(expected_minp,accuary))
  expect_gte(round(resTestTwoTighter[4],accuary),
             round(expected_maxp,accuary))

  # broader bounds should bound tighter bounds
  expect_lte(round(resTestTwoBroader[3],accuary),
             round(resTestTwoTighter[3],accuary))
  expect_gte(round(resTestTwoBroader[4],accuary),
             round(resTestTwoTighter[4],accuary))



  ## less (correct = TRUE)
  resTestLessBroader <- boundsPValueWithTies(X,Y, alternative = 'less',
                                             lower.boundary = -Inf,
                                             upper.boundary = Inf,
                                             exact = FALSE, correct = TRUE)
  resTestLessTighter <- boundsPValueWithTies(X,Y, alternative = 'less',
                                             lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])),
                                             upper.boundary = Inf,
                                             exact = FALSE, correct = TRUE)

  resExpcLessOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                       alternative = 'less',
                                       exact = FALSE)

  resExpcLessTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                       alternative = 'less',
                                       exact = FALSE)

  expected_minp <- resExpcLessOne$p.value[[1]]
  expected_maxp <- resExpcLessTwo$p.value[[1]]


  # tighter bounds should bound special cases
  expect_lte(round(resTestLessTighter[3],accuary),
             round(expected_minp,accuary))
  expect_gte(round(resTestLessTighter[4],accuary),
             round(expected_maxp,accuary))

  # broader bounds should bound tighter bounds
  expect_lte(round(resTestLessBroader[3],accuary),
             round(resTestLessTighter[3],accuary))
  expect_gte(round(resTestLessBroader[4],accuary),
             round(resTestLessTighter[4],accuary))

  ## less (correct = FALSE)
  resTestLessBroader <- boundsPValueWithTies(X,Y, alternative = 'less',
                                             lower.boundary = -Inf,
                                             upper.boundary = Inf,
                                             exact = FALSE, correct = FALSE)
  resTestLessTighter <- boundsPValueWithTies(X,Y, alternative = 'less',
                                             lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])),
                                             upper.boundary = Inf,
                                             exact = FALSE, correct = FALSE)

  resExpcLessOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                       alternative = 'less',
                                       exact = FALSE, correct = FALSE)

  resExpcLessTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                       alternative = 'less',
                                       exact = FALSE, correct = FALSE)

  expected_minp <- resExpcLessOne$p.value[[1]]
  expected_maxp <- resExpcLessTwo$p.value[[1]]


  # tighter bounds should bound special cases
  expect_lte(round(resTestLessTighter[3],accuary),
             round(expected_minp,accuary))
  expect_gte(round(resTestLessTighter[4],accuary),
             round(expected_maxp,accuary))

  # broader bounds should bound tighter bounds
  expect_lte(round(resTestLessBroader[3],accuary),
             round(resTestLessTighter[3],accuary))
  expect_gte(round(resTestLessBroader[4],accuary),
             round(resTestLessTighter[4],accuary))

  ## greater (correct = FALSE)
  resTestGreatBroader <- boundsPValueWithTies(X,Y, alternative = 'greater',
                                              lower.boundary = -Inf,
                                              upper.boundary = Inf,
                                              exact = FALSE, correct = FALSE)
  resTestGreatTighter <- boundsPValueWithTies(X,Y, alternative = 'greater',
                                              lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])),
                                              upper.boundary = Inf,
                                              exact = FALSE, correct = FALSE)

  resExpcGreatOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                        alternative = 'greater',
                                        exact = FALSE, correct = FALSE)

  resExpcGreatTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                        alternative = 'greater',
                                        exact = FALSE, correct = FALSE)

  expected_minp <- resExpcGreatTwo$p.value[[1]]
  expected_maxp <- resExpcGreatOne$p.value[[1]]

  # tighter bounds should bound special cases
  expect_lte(round(resTestGreatTighter[3],accuary),
             round(expected_minp,accuary))
  expect_gte(round(resTestGreatTighter[4],accuary),
             round(expected_maxp,accuary))

  # broader bounds should bound tighter bounds
  expect_lte(round(resTestGreatBroader[3],accuary),
             round(resTestGreatTighter[3],accuary))
  expect_gte(round(resTestGreatBroader[4],accuary),
             round(resTestGreatTighter[4],accuary))

  ## greater (correct = TRUE)
  resTestGreatBroader <- boundsPValueWithTies(X,Y, alternative = 'greater',
                                              lower.boundary = -Inf,
                                              upper.boundary = Inf,
                                              exact = FALSE, correct = TRUE)
  resTestGreatTighter <- boundsPValueWithTies(X,Y, alternative = 'greater',
                                              lower.boundary = min(c(X[!is.na(X)], Y[!is.na(Y)])),
                                              upper.boundary = Inf,
                                              exact = FALSE, correct = TRUE)

  resExpcGreatOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                        alternative = 'greater',
                                        exact = FALSE)

  resExpcGreatTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                        alternative = 'greater',
                                        exact = FALSE)

  expected_minp <- resExpcGreatTwo$p.value[[1]]
  expected_maxp <- resExpcGreatOne$p.value[[1]]

  # tighter bounds should bound special cases
  expect_lte(round(resTestGreatTighter[3],accuary),
             round(expected_minp,accuary))
  expect_gte(round(resTestGreatTighter[4],accuary),
             round(expected_maxp,accuary))

  # broader bounds should bound tighter bounds
  expect_lte(round(resTestGreatBroader[3],accuary),
             round(resTestGreatTighter[3],accuary))
  expect_gte(round(resTestGreatBroader[4],accuary),
             round(resTestGreatTighter[4],accuary))
})

