checkTies <- function(X, Y, ties) {

  Z <- c(X,Y)

  # Observed samples in X and Y, respectively
  X_prime <- X[!is.na(X)]
  Y_prime <- Y[!is.na(Y)]

  # All observed samples
  Z_prime <-c(X_prime, Y_prime)
  r <- rank(Z_prime)

  # Check ties
  if(is.null(ties)){
    # If ties is not specified, decide ties according to observed samples
    ties <- (length(r) != length(unique(r)))
  }else{
    # If ties is specified, consider false cases

    if( (ties == FALSE) & (length(r) != length(unique(r))) ){
      warning("observed samples are tied, you may want to specify ties = TRUE")
    }

    if( (ties == TRUE) & (length(r) == length(unique(r))) & (length(Z) == length(Z_prime))){
      warning("all samples are observed distinct numbers, ties can only be FALSE")
      ties <- FALSE
    }

    if( (ties == FALSE) & (length(r) != length(unique(r))) & (length(Z) == length(Z_prime))){
      warning("all samples are observed with tied observations, ties can only be TRUE")
      ties <- TRUE
    }

  }

  return(ties)
}
