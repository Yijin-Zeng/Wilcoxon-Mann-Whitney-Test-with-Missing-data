boundsWMWStatistic <- function(X, Y, ties, lower.boundary, upper.boundary){
  
  # Sample size
  n <- length(X)
  
  # Compute bounds of sum of ranks using boundsSumRank function
  Bounds <- boundsSumRank(X, Y, ties, lower.boundary, upper.boundary)
  lowerBoundSumRank <- Bounds[1]
  upperBoundSumRank <- Bounds[2]
  
  # Compute bounds of WMW test statistic
  lowerBoundWMWStatistic <- lowerBoundSumRank - n * (n + 1) / 2
  upperBoundWMWStatistic <- upperBoundSumRank - n * (n + 1) / 2
  
  return(c(lowerBoundWMWStatistic, upperBoundWMWStatistic))
}