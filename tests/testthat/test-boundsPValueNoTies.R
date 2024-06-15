test_that("no missing data case", {
  # While no missing data is presented, the boundsPValueNoTies is expected
  # to produce exactly the same result as stats::wilcox.test().
  set.seed(1)
  accuary <- 5 # compare the first 5 digits of test and expect results

  # samples used for testing
  n <- 50
  m <- 50
  X <- rnorm(n,0,1)
  Y <- rnorm(m,0,1)

  ###############################Test Statistic#################################

  # Check the bounds of the WMW test statistic, which is not depended on
  # alternative or exact
  resTestTwoExact <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = TRUE, correct = TRUE)
  resExpcTwoExact <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                        exact = TRUE)

  expect_equal(round(resTestTwoExact[1],accuary),
               round(resExpcTwoExact$statistic[[1]],accuary))

  expect_equal(round(resTestTwoExact[2],accuary),
               round(resExpcTwoExact$statistic[[1]],accuary))


  ###################################p-value####################################

  ### two sided; exact
  expect_equal(round(resTestTwoExact[3],accuary),
               round(resExpcTwoExact$p.value[[1]],accuary))
  expect_equal(round(resTestTwoExact[4],accuary),
               round(resExpcTwoExact$p.value[[1]],accuary))

  ### two sided; approximation (correct = TRUE)
  resTestTwoAppro <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = TRUE)
  resExpcTwoAppro <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                        exact = FALSE)
  expect_equal(round(resTestTwoAppro[3],accuary),
               round(resExpcTwoAppro$p.value[[1]],accuary))
  expect_equal(round(resTestTwoAppro[4],accuary),
               round(resExpcTwoAppro$p.value[[1]],accuary))

  ### two sided; approximation (correct = FALSE)
  resTestTwoAppro <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = FALSE)
  resExpcTwoAppro <- stats::wilcox.test(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = FALSE)
  expect_equal(round(resTestTwoAppro[3],accuary),
               round(resExpcTwoAppro$p.value[[1]],accuary))
  expect_equal(round(resTestTwoAppro[4],accuary),
               round(resExpcTwoAppro$p.value[[1]],accuary))

  ## less; exact
  resTestLessExact <- boundsPValueNoTies(X,Y, alternative = 'less',
                                         exact = TRUE, correct = TRUE)
  resExpcLessExact <- stats::wilcox.test(X,Y, alternative = 'less',
                                         exact = TRUE)
  expect_equal(round(resTestLessExact[3],accuary),
               round(resExpcLessExact$p.value[[1]],accuary))
  expect_equal(round(resTestLessExact[4],accuary),
               round(resExpcLessExact$p.value[[1]],accuary))

  ## less; approximation (correct = TRUE)
  resTestLessAppro <- boundsPValueNoTies(X,Y, alternative = 'less',
                                         exact = FALSE, correct = TRUE)
  resExpcLessAppro <- stats::wilcox.test(X,Y, alternative = 'less',
                                         exact = FALSE)
  expect_equal(round(resTestLessAppro[3],accuary),
               round(resExpcLessAppro$p.value[[1]],accuary))
  expect_equal(round(resTestLessAppro[4],accuary),
               round(resExpcLessAppro$p.value[[1]],accuary))
  ## less; approximation (correct = FALSE)
  resTestLessAppro <- boundsPValueNoTies(X,Y, alternative = 'less',
                                         exact = FALSE, correct = FALSE)
  resExpcLessAppro <- stats::wilcox.test(X,Y, alternative = 'less',
                                         exact = FALSE, correct = FALSE)
  expect_equal(round(resTestLessAppro[3],accuary),
               round(resExpcLessAppro$p.value[[1]],accuary))
  expect_equal(round(resTestLessAppro[4],accuary),
               round(resExpcLessAppro$p.value[[1]],accuary))

  ## greater; exact
  resTestGreatExact <- boundsPValueNoTies(X,Y, alternative = 'greater',
                                          exact = TRUE, correct = TRUE)
  resExpcGreatExact <- stats::wilcox.test(X,Y, alternative = 'greater',
                                          exact = TRUE)
  expect_equal(round(resTestGreatExact[3],accuary),
               round(resExpcGreatExact$p.value[[1]],accuary))
  expect_equal(round(resTestGreatExact[4],accuary),
               round(resExpcGreatExact$p.value[[1]],accuary))

  ## greater; approximation (correct = TRUE)
  resTestGreatAppro <- boundsPValueNoTies(X,Y, alternative = 'greater',
                                          exact = FALSE, correct = TRUE)
  resExpcGreatAppro <- stats::wilcox.test(X,Y, alternative = 'greater',
                                          exact = FALSE)
  expect_equal(round(resTestGreatAppro[3],accuary),
               round(resExpcGreatAppro$p.value[[1]],accuary))
  expect_equal(round(resTestGreatAppro[4],accuary),
               round(resExpcGreatAppro$p.value[[1]],accuary))


  ## greater; approximation (correct = FALSE)
  resTestGreatAppro <- boundsPValueNoTies(X,Y, alternative = 'greater',
                                          exact = FALSE, correct = FALSE)
  resExpcGreatAppro <- stats::wilcox.test(X,Y, alternative = 'greater',
                                          exact = FALSE, correct = FALSE)
  expect_equal(round(resTestGreatAppro[3],accuary),
               round(resExpcGreatAppro$p.value[[1]],accuary))
  expect_equal(round(resTestGreatAppro[4],accuary),
               round(resExpcGreatAppro$p.value[[1]],accuary))
})


test_that("missing data case", {
  # While missing data is presented, the boundsPValueNoTies is expected
  # to produce the same result as stats::wilcox.test
  # after special cases of imputation.
  set.seed(1)
  accuary <- 5 # compare the first 5 digits of test and expect results

  # Sample used for testing
  n <- 50
  m <- 50
  X <- rnorm(n,0,1)
  Y <- rnorm(m,0,1)
  X[1:10] <- NA
  Y[1:5] <- NA

  ## Special imputation one
  ## impute each missing sample in X as a value **smaller** than all oberved samples
  ## impute each missing sample in Y as a value **larger** than all oberved samples
  ImputedXOne <- X
  ImputedXOne[is.na(ImputedXOne)] <- min(c(X[!is.na(X)], Y[!is.na(Y)])) - abs(rnorm(10,0,1))
  ImputedYOne <- Y
  ImputedYOne[is.na(ImputedYOne)] <- max(c(X[!is.na(X)], Y[!is.na(Y)])) + abs(rnorm(5,0,1))

  ## Special imputation two
  ## impute each missing sample in X as a value **larger** than all oberved samples
  ## impute each missing sample in Y as a value **smaller** than all oberved samples
  ImputedXTwo <- X
  ImputedXTwo[is.na(ImputedXTwo)] <- max(c(X[!is.na(X)], Y[!is.na(Y)])) + abs(rnorm(10,0,1))
  ImputedYTwo <- Y
  ImputedYTwo[is.na(ImputedYTwo)] <- min(c(X[!is.na(X)], Y[!is.na(Y)])) - abs(rnorm(5,0,1))


  ###############################Test Statistic#################################

  # Check the bounds of the WMW test statistic, which is not depended on
  # alternative or exact
  resTestTwoExact <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = TRUE, correct = TRUE)
  resExpcTwoExactOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                           alternative = 'two.sided',
                                           exact = TRUE)
  resExpcTwoExactTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                           alternative = 'two.sided',
                                           exact = TRUE)

  expect_equal(round(resTestTwoExact[1],accuary),
               round(resExpcTwoExactOne$statistic[[1]],accuary))

  expect_equal(round(resTestTwoExact[2],accuary),
               round(resExpcTwoExactTwo$statistic[[1]],accuary))

  #############################p-value(two sided)##############################

  ### two sided; exact
  expcted_p1 <- resExpcTwoExactOne$p.value[[1]]
  expected_p2 <- resExpcTwoExactTwo$p.value[[1]]
  expected_minp <- min(expcted_p1, expected_p2)

  if( ((resExpcTwoExactOne$statistic[[1]] - n*m/2) *
       (resExpcTwoExactTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expcted_p1, expcted_p2)
  }

  expect_equal(resTestTwoExact[3], expected_minp)
  expect_equal(resTestTwoExact[4], expected_maxp)

  ### two sided; approximation (correct = TRUE)
  resTestTwoAppro <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = TRUE)
  resExpcTwoApproOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                           alternative = 'two.sided',
                                           exact = FALSE)
  resExpcTwoApproTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                           alternative = 'two.sided',
                                           exact = FALSE)

  expcted_p1 <- resExpcTwoApproOne$p.value[[1]]
  expected_p2 <- resExpcTwoApproTwo$p.value[[1]]
  expected_minp <- min(expcted_p1, expected_p2)

  if( ((resExpcTwoApproOne$statistic[[1]] - n*m/2) *
       (resExpcTwoApproTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expcted_p1, expcted_p2)
  }

  expect_equal(resTestTwoAppro[3], expected_minp)
  expect_equal(resTestTwoAppro[4], expected_maxp)

  ### two sided; approximation (correct = FALSE)
  resTestTwoAppro <- boundsPValueNoTies(X,Y, alternative = 'two.sided',
                                        exact = FALSE, correct = FALSE)
  resExpcTwoApproOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                           alternative = 'two.sided',
                                           exact = FALSE, correct = FALSE)
  resExpcTwoApproTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                           alternative = 'two.sided',
                                           exact = FALSE, correct = FALSE)

  expcted_p1 <- resExpcTwoApproOne$p.value[[1]]
  expected_p2 <- resExpcTwoApproTwo$p.value[[1]]
  expected_minp <- min(expcted_p1, expected_p2)

  if( ((resExpcTwoApproOne$statistic[[1]] - n*m/2) *
       (resExpcTwoApproTwo$statistic[[1]] - n*m/2)) < 0){
    expected_maxp <- 1
  }else{
    expected_maxp <- max(expcted_p1, expcted_p2)
  }

  expect_equal(resTestTwoAppro[3], expected_minp)
  expect_equal(resTestTwoAppro[4], expected_maxp)

  ### less; exact
  resTestLessExact <- boundsPValueNoTies(X,Y, alternative = 'less',
                                         exact = TRUE, correct = TRUE)
  resExpcLessExactOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                            alternative = 'less',
                                            exact = TRUE)
  resExpcLessExactTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                            alternative = 'less',
                                            exact = TRUE)

  expected_minp <- resExpcLessExactOne$p.value[[1]]
  expected_maxp <- resExpcLessExactTwo$p.value[[1]]

  expect_equal(resTestLessExact[3], expected_minp)
  expect_equal(resTestLessExact[4], expected_maxp)

  ### less; approxmation (correct = TRUE)
  resTestLessAppro <- boundsPValueNoTies(X,Y, alternative = 'less',
                                         exact = FALSE, correct = TRUE)
  resExpcLessApproOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                            alternative = 'less',
                                            exact = FALSE)
  resExpcLessApproTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                            alternative = 'less',
                                            exact = FALSE)

  expected_minp <- resExpcLessApproOne$p.value[[1]]
  expected_maxp <- resExpcLessApproTwo$p.value[[1]]

  expect_equal(resTestLessAppro[3], expected_minp)
  expect_equal(resTestLessAppro[4], expected_maxp)

  ### less; approxmation (correct = FALSE)
  resTestLessAppro <- boundsPValueNoTies(X,Y, alternative = 'less',
                                         exact = FALSE, correct = FALSE)
  resExpcLessApproOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                            alternative = 'less',
                                            exact = FALSE, correct = FALSE)
  resExpcLessApproTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                            alternative = 'less',
                                            exact = FALSE, correct = FALSE)

  expected_minp <- resExpcLessApproOne$p.value[[1]]
  expected_maxp <- resExpcLessApproTwo$p.value[[1]]

  expect_equal(resTestLessAppro[3], expected_minp)
  expect_equal(resTestLessAppro[4], expected_maxp)

  ### greater; exact
  resTestGreatExact <- boundsPValueNoTies(X,Y, alternative = 'greater',
                                          exact = TRUE, correct = TRUE)
  resExpcGreatExactOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                             alternative = 'greater',
                                             exact = TRUE)
  resExpcGreatExactTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                             alternative = 'greater',
                                             exact = TRUE)

  expected_minp <- resExpcGreatExactTwo$p.value[[1]]
  expected_maxp <- resExpcGreatExactOne$p.value[[1]]

  expect_equal(resTestGreatExact[3], expected_minp)
  expect_equal(resTestGreatExact[4], expected_maxp)

  ### greater; approxmation (correct = TRUE)
  resTestGreatAppro <- boundsPValueNoTies(X,Y, alternative = 'greater',
                                          exact = FALSE, correct = TRUE)
  resExpcGreatApproOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                             alternative = 'greater',
                                             exact = FALSE)
  resExpcGreatApproTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                             alternative = 'greater',
                                             exact = FALSE)

  expected_minp <- resExpcGreatApproTwo$p.value[[1]]
  expected_maxp <- resExpcGreatApproOne$p.value[[1]]

  expect_equal(resTestGreatAppro[3], expected_minp)
  expect_equal(resTestGreatAppro[4], expected_maxp)

  ### greater; approxmation (correct = FALSE)
  resTestGreatAppro <- boundsPValueNoTies(X,Y, alternative = 'greater',
                                          exact = FALSE, correct = FALSE)
  resExpcGreatApproOne <- stats::wilcox.test(ImputedXOne,ImputedYOne,
                                             alternative = 'greater',
                                             exact = FALSE, correct = FALSE)
  resExpcGreatApproTwo <- stats::wilcox.test(ImputedXTwo,ImputedYTwo,
                                             alternative = 'greater',
                                             exact = FALSE, correct = FALSE)

  expected_minp <- resExpcGreatApproTwo$p.value[[1]]
  expected_maxp <- resExpcGreatApproOne$p.value[[1]]

  expect_equal(resTestGreatAppro[3], expected_minp)
  expect_equal(resTestGreatAppro[4], expected_maxp)

})

