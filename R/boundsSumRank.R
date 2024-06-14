# Compute the bounds of sum of ranks in the presence of missing data.

boundsSumRank <- function(X, Y, ties, lower.boundary, upper.boundary){

  # Sample size
  n <- length(X)
  m <- length(Y)

  # Observed samples in X and Y, respectively
  X_prime <- X[!is.na(X)]
  Y_prime <- Y[!is.na(Y)]

  # All observed samples
  Z_prime <-c(X_prime, Y_prime)
  r <- rank(Z_prime)

  # Observed sample size
  n_prime <- length(X_prime)
  m_prime <- length(Y_prime)
  
  # Sum of ranks of X_prime in Z_prime
  rankSumX_prime <- sum(r[1:n_prime])

  # Bounds of sum of ranks of X in Z without ties
  lowerBoundSumRank <- rankSumX_prime + (n - n_prime) * (n + n_prime + 1)/2
  upperBoundSumRank <- rankSumX_prime + (n * (n + 2*m + 1) - n_prime * (n_prime + 2*m_prime + 1))/2
  
  # Bounds of sum of ranks of X in Z with ties
  if(ties) {
    # If ties are allowed, tighter bounds may exist
    
    if (lower.boundary == 'equal'){
      a <- min(Z_prime)
    }else{
      a <- -Inf
    }
          
    if (upper.boundary == 'equal'){
      b <- max(Z_prime)
    }else{
      b <- Inf
    }
   
    lowerBoundSumRank <- lowerBoundSumRank + (sum(Y_prime == a) * (n - n_prime) 
                                           + sum(X_prime == b) * (m - m_prime))/2
    upperBoundSumRank <- upperBoundSumRank - (sum(X_prime == a) * (m - m_prime) 
                                           + sum(Y_prime == b) * (n - n_prime))/2   
  }

  return(c(lowerBoundSumRank, upperBoundSumRank))
}

