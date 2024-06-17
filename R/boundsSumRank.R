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
    min_observed <- min(Z_prime)
    max_observed <- max(Z_prime)

    if (lower.boundary <= min_observed){
      a <- lower.boundary
    }else{
      warning('lower.boundary must be smaller or equal than the minimum of all observed data. lower.boundary is set to -Inf')
      a <- -Inf
    }

    if (upper.boundary >= max_observed){
      b <- upper.boundary
    }else{
      warning('upper.boundary must be larger or equal than the maximum of all observed data. upper.boundary is set to Inf')
      b <- Inf
    }

    lowerBoundSumRank <- lowerBoundSumRank + (sum(Y_prime == a) * (n - n_prime)
                                           + sum(X_prime == b) * (m - m_prime))/2

    upperBoundSumRank <- upperBoundSumRank - (sum(X_prime == a) * (m - m_prime)
                                           + sum(Y_prime == b) * (n - n_prime))/2
  }

  return(c(lowerBoundSumRank, upperBoundSumRank))
}

