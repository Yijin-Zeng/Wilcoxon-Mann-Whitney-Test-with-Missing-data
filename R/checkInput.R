checkInput <- function(X,Y){

  # Ensure the length of impute
  if ( (length(X) < 1) | (length(Y) < 1) ){
    stop("not enough valid samples in 'X' or 'Y'")
  }

  # Observed Samples
  X_prime <- X[!is.na(X)]
  Y_prime <- Y[!is.na(Y)]

  # Check if X_prime and Y_prime only contain numeric
  # If the length of X_prime or Y_prime is 0, return 0
  if ( (length(X_prime) < 1)&(length(Y_prime) < 1) ){
    # both X and Y only contain NA
    return(0)
  }else if ( (length(X_prime) < 1)&(length(Y_prime) >= 1) ){
    # X only contain NA
    # Y contains non-NA
    if(!is.numeric(Y_prime)){
      stop("observed samples must be numeric")
    }else{
      return(0)
    }
  }else if ( (length(X_prime) >= 1)&(length(Y_prime) < 1) ){
    # Y only contain NA
    # X contains non-NA
    if(!is.numeric(X_prime)){
      stop("observed samples must be numeric")
    }else{
      return(0)
    }
  }else{
    # Ensure inputs are numeric
    if ( (!is.numeric(X_prime)) | (!is.numeric(Y_prime)) ){
      stop("observed samples must be numeric")
    }
  }

  # Check Input pass, return 1
  return(1)
}
