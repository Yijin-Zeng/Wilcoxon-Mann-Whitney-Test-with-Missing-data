# Bound p-value of the test in the presence of missing data (ties may exist)

boundsPValueWithTies <- function(X, Y, alternative, lower.boundary,
                                 upper.boundary, exact, correct) {

  # Sample size
  n <- length(X)
  m <- length(Y)

  # Observed samples in X and Y, respectively
  X_prime <- X[!is.na(X)]
  Y_prime <- Y[!is.na(Y)]
  Z_prime <- c(X_prime, Y_prime)
  r <- rank(Z_prime)

  # Observed sample size
  n_prime <- length(X_prime)
  m_prime <- length(Y_prime)

  # Compute bounds of WMWS tatistic using boundsWMWStatistic function
  Bounds <- boundsWMWStatistic(X, Y, ties = TRUE, lower.boundary = lower.boundary,
                               upper.boundary = upper.boundary)

  lowerBoundWMWStatistic <- Bounds[1]
  upperBoundWMWStatistic <- Bounds[2]

  # Decide if exact
  if(is.null(exact)){
    exact <- (n < 50) && (m < 50)
  }

  if(exact){
    warning("cannot bound exact p-value with ties")
    exact <- 0
  }

  # using normal approximation
  correct_lower <- 0
  correct_upper <- 0
  if(correct){
    correct_lower <- switch(alternative,
                            "two.sided" = sign((lowerBoundWMWStatistic - n*m/2)) * 0.5,
                            "greater" = 0.5,
                            "less" = -0.5)
    correct_upper <- switch(alternative,
                            "two.sided" = sign((upperBoundWMWStatistic - n*m/2)) * 0.5,
                            "greater" = 0.5,
                            "less" = -0.5)
  }


  nties <- table(r)

  mu <- n*m/2

  sigmaSquareMax <- n*m*(n+m+1)/12 - n*m*sum(nties^3 - nties)/(12*(n+m)*(n+m-1))

  nties[nties == max(nties)][1] <- nties[nties == max(nties)][1] + n + m - n_prime - m_prime

  sigmaSquareMin <- n*m*(n+m+1)/12 - n*m*sum(nties^3 - nties)/(12*(n+m)*(n+m-1))

  if( (sigmaSquareMax == 0)|(sigmaSquareMin == 0) ){
    stop("cannot compute valid p-value")
  }

  Z_1 <- (lowerBoundWMWStatistic - correct_lower - mu)/sqrt(sigmaSquareMin)

  Z_2 <- (upperBoundWMWStatistic - correct_upper - mu)/sqrt(sigmaSquareMax)

  Z_3 <- (upperBoundWMWStatistic - correct_upper - mu)/sqrt(sigmaSquareMin)

  Z_4 <- (lowerBoundWMWStatistic - correct_lower - mu)/sqrt(sigmaSquareMax)
  #Note:  [lowerBoundWMWStatistic - correct_lower - mu, upperBoundWMWStatistic - correct_upper - mu] will bound (WMWStatistic - correct - mu) regardless whether correct is TRUE or FALSE


  # compute p_1
  switch(alternative,
         'two.sided' = {
           if ( Z_1 < 0){
             p <- stats::pnorm(Z_1)
           }else{
             p <- 1 - stats::pnorm(Z_1)
           }
           p_1 <- 2*p
         },
         'greater' = {
           p_1 <- stats::pnorm(Z_1, lower.tail = FALSE)
         },
         'less' = {
           p_1 <- stats::pnorm(Z_1)
         })

  # compute p_2
  switch(alternative,
         'two.sided' = {
           if ( Z_2 < 0){
             p <- stats::pnorm(Z_2)
           }else{
             p <- 1 - stats::pnorm(Z_2)
           }
           p_2 <- 2*p
         },
         'greater' = {
           p_2 <- stats::pnorm(Z_2, lower.tail = FALSE)
         },
         'less' = {
           p_2 <- stats::pnorm(Z_2)
         })

  # compute p_3
  switch(alternative,
         'two.sided' = {
           if ( Z_3 < 0){
             p <- stats::pnorm(Z_3)
           }else{
             p <- 1 - stats::pnorm(Z_3)
           }
           p_3 <- 2*p
         },
         'greater' = {
           p_3 <- stats::pnorm(Z_3, lower.tail = FALSE)
         },
         'less' = {
           p_3 <- stats::pnorm(Z_3)
         })

  # compute p_4
  switch(alternative,
         'two.sided' = {
           if ( Z_4 < 0){
             p <- stats::pnorm(Z_4)
           }else{
             p <- 1 - stats::pnorm(Z_4)
           }
           p_4 <- 2*p
         },
         'greater' = {
           p_4 <- stats::pnorm(Z_4, lower.tail = FALSE)
         },
         'less' = {
           p_4 <- stats::pnorm(Z_4)
         })


  # decide bounds of p-value
  switch(alternative,
         'two.sided' = {

           if (((lowerBoundWMWStatistic - n*m/2) <0) & ((upperBoundWMWStatistic - n*m/2) <0) ){
             lowerBoundPValue <- p_1
             upperBoundPValue <- p_2
           }else if (((lowerBoundWMWStatistic - n*m/2) <0) & ((upperBoundWMWStatistic - n*m/2) >= 0) ) {
             lowerBoundPValue <- min(p_1,p_3)
             upperBoundPValue <- 1
           }else{
             lowerBoundPValue <- p_3
             upperBoundPValue <- p_4
           }

         },
         'greater' = {

           if (((lowerBoundWMWStatistic - n*m/2) <0) & ((upperBoundWMWStatistic - n*m/2) <0) ){
             lowerBoundPValue <- p_2
             upperBoundPValue <- p_1
           }else if (((lowerBoundWMWStatistic - n*m/2) <0) & ((upperBoundWMWStatistic - n*m/2) >= 0) ) {
             lowerBoundPValue <- p_3
             upperBoundPValue <- p_1
           }else{
             lowerBoundPValue <- p_3
             upperBoundPValue <- p_4
           }

           },
         'less' = {

           if (((lowerBoundWMWStatistic - n*m/2) <0) & ((upperBoundWMWStatistic - n*m/2) <0) ){
             lowerBoundPValue <- p_1
             upperBoundPValue <- p_2
           }else if (((lowerBoundWMWStatistic - n*m/2) <0) & ((upperBoundWMWStatistic - n*m/2) >= 0) ) {
             lowerBoundPValue <- p_1
             upperBoundPValue <- p_3
           }else{
             lowerBoundPValue <- p_4
             upperBoundPValue <- p_3
           }

         })

  return(c(lowerBoundWMWStatistic,upperBoundWMWStatistic, lowerBoundPValue, upperBoundPValue, exact))

}
