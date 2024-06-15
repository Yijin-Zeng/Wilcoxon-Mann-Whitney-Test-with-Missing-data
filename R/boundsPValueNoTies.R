# Bound p-value of the test in the presence of missing data (no ties allowed)

boundsPValueNoTies <- function(X, Y, alternative, exact, correct) {
  
  # Sample size
  n <- length(X)
  m <- length(Y)
  
  # Observed samples in X and Y, respectively
  X_prime <- X[!is.na(X)]
  Y_prime <- Y[!is.na(Y)]
  
  # Observed sample size
  n_prime <- length(X_prime)
  m_prime <- length(Y_prime)
  
  # Compute bounds of WMWS statistic using boundsWMWStatistic function
  Bounds <- boundsWMWStatistic(X, Y, ties = FALSE)
  lowerBoundWMWStatistic <- Bounds[1]
  upperBoundWMWStatistic <- Bounds[2]

  # Decide if exact 
  if(is.null(exact)){
   exact <- (n < 50) && (m < 50)
  }
  
  # Compute p_1, p_2 -- the p-values corresponding to the minimum and maximum WMW test statistics, respectively
  if(exact){
    # compute p_1
    switch(alternative, 
           'two.sided'= {
             if (lowerBoundWMWStatistic > (n * m /2)){
               p <- stats::pwilcox(lowerBoundWMWStatistic-1, n, m, lower.tail = FALSE)
             }else{
               p <- stats::pwilcox(lowerBoundWMWStatistic, n, m)
             }
             p_1 <- min(2*p,1)
           },
           'greater' = {
             p_1 <- stats::pwilcox(lowerBoundWMWStatistic-1, n, m, lower.tail = FALSE)
           },
           'less' = {
             p_1 <- stats::pwilcox(lowerBoundWMWStatistic, n, m,)
           })
    # compute p_2
    switch(alternative, 
           'two.sided'= {
             if (upperBoundWMWStatistic > (n * m /2)){
               p <- stats::pwilcox(upperBoundWMWStatistic-1, n, m, lower.tail = FALSE)
             }else{
               p <- stats::pwilcox(upperBoundWMWStatistic, n, m)
             }
             p_2 <- min(2*p,1)
           },
           'greater' = {
             p_2 <- stats::pwilcox(upperBoundWMWStatistic-1, n, m, lower.tail = FALSE)
           },
           'less' = {
             p_2 <- stats::pwilcox(upperBoundWMWStatistic, n, m,)
           })
    }else{
    # if not exact, using normal approximation
    correct_lower <- 0
    correct_upper <- 0
    
    if(correct){
      # using continuity correction
      correct_lower <- switch(alternative,
                           "two.sided" = sign((lowerBoundWMWStatistic - n*m/2)) * 0.5,
                           "greater" = 0.5,
                           "less" = -0.5)
      correct_upper <- switch(alternative,
                              "two.sided" = sign((upperBoundWMWStatistic - n*m/2)) * 0.5,
                              "greater" = 0.5,
                              "less" = -0.5)
      }
    
    mu <- n*m/2
    sigma <- sqrt(n*m*(n+m+1)/12)
    
    lowerBoundZ <- (lowerBoundWMWStatistic - correct_lower - mu)/sigma
    upperBoundZ <- (upperBoundWMWStatistic - correct_upper - mu)/sigma
    ### Note: [lowerBoundZ, upperBoundZ] will bound (WMWStatistic - correct)/sigma regardless if correct is TRUE or FALSE
    
    # compute p_1
    switch(alternative,
           'two.sided' = {
             if (lowerBoundZ < 0){
               p <- stats::pnorm(lowerBoundZ)
             }else{
               p <- 1 - stats::pnorm(lowerBoundZ)
             }
             p_1 <- 2*p
           },
           'greater' = {
             p_1 <- stats::pnorm(lowerBoundZ, lower.tail = FALSE)
           },
           'less' = {
             p_1 <- stats::pnorm(lowerBoundZ)
           })

    # compute p_2
    switch(alternative,
           'two.sided' = {
             if (upperBoundZ < 0){
               p <- stats::pnorm(upperBoundZ)
             }else{
               p <- 1 - stats::pnorm(upperBoundZ)
             }
             p_2 <- 2*p
           },
           'greater' = {
             p_2 <- stats::pnorm(upperBoundZ, lower.tail = FALSE)
           },
           'less' = {
             p_2 <- stats::pnorm(upperBoundZ)
           })
  }
  
  # Compute bounds of p-values
  lowerBoundPValue <- min(p_1, p_2)
  
  switch(alternative,
         'two.sided' = {
           if ( ((lowerBoundWMWStatistic - n*m/2) * (upperBoundWMWStatistic -n*m/2)) > 0){
             upperBoundPValue <- max(p_1,p_2)
           }else{
             upperBoundPValue <- 1
           }
         },
         'greater' = {
           upperBoundPValue <- p_1
         },
         'less' = {
           upperBoundPValue <- p_2
         })
  
  return(c(lowerBoundWMWStatistic,upperBoundWMWStatistic, lowerBoundPValue, upperBoundPValue, exact))
  
}