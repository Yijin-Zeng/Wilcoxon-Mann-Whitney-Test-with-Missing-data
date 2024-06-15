# This function performs the method proposed in the paper ``On two-sample testing for data with arbitrarily missing values'.
# When no missing data is presented, this function returns exactly the same results as stats::wilcox.test().
# When missing data is presented, this function returns a p-value that controls the Type I error regardless of the values of missing data, along with the bounds of the Wilcoxon-Mann-Whitney test statistic and the bounds of the p-value of the test without missing data.

#' @md
#' @export
#'
#' @title Two-sample Wilcoxon-Mann-Whitney Test in the Presence of Arbitrarily Missing Data
#'
#' @description Performs Wilcoxon-Mann-Whitney two-sample hypothesis tests on (potentially) partially observed vectors of data with controlled Type I error regardless of values of missing data.
#'
#' @usage wmwm.test(X, Y, alternative = c("two.sided", "less", "greater"),
#' ties = NULL, lower.boundary = -Inf,
#' upper.boundary = Inf, exact = NULL, correct = TRUE)
#'
#' @param X,Y numeric vectors of data values with potential missing data. Inf and -Inf values will be omitted.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param exact a logical indicating whether the bounds should be of an exact p-value.
#' @param ties a logical indicating whether samples could be tied.
#'   * If observed samples contain tied samples, ties defaults to TRUE.
#'   * If observed samples do not contain tied samples, ties defaults to FALSE.
#' @param lower.boundary (when ties is TRUE) a number specifying the lower bound of the data set, must be smaller or equal than the minimum of all observed data.
#' @param upper.boundary (when ties is TRUE) a number specifying the upper bound of the data set, must be larger or equal than the maximum of all observed data.
#' @param correct a logical indicating whether the bounds should be of a p-value applying continuity correction in the normal approximation.
#'
#' @details  This function allows us to perform two-sample Wilcoxon-Mann-Whitney test in the presence of missing data with controlled controlled Type I error regardless of the values of missing data.
#'
#' This function will compute the bounds of the Wilcoxon-Mann-Whitney test statistic and p-values without missing data. The p-value of this test is then returned as the maximum possible p-value of the Wilcoxon-Mann-Whitney test.
#'
#' When `X` and `Y` consist of real-valued, potentially tied samples, by additionally specifying `ties = TRUE` (this is automatically done if ties exist in observed samples `X'` and `Y'`),
#' this function deals with ties using mid-ranks method.
#'
#' By default (if exact is not specified), this function returns bounds of an exact p-value if the `X` and `Y` both contain less than 50 samples and there are no ties. Otherwise, bounds of a p-value calculated using normal approximation with continuity correction will be returned.
#'
#' @return
#'
#'  \item{p.value}{the p-value for the test}
#'
#'  \item{bounds.statistic}{bounds of the value of the Wilcoxon-Mann-Whitney test statistic without missing data}
#'
#'  \item{bounds.pvalue}{bounds of the p-value of the Wilcoxon-Mann-Whitney test without missing data.}
#'
#'  \item{alternative}{a character string describing the alternative hypothesis.}
#'
#'  \item{ties.method}{a character string describing whether samples are considered tied.}
#'
#'  \item{description.bounds}{a character string describing the bounds.}
#'
#'  \item{data.name}{a character string giving the names of the data.}
#'
#'
#' @references
#' \itemize{
#'  \item Zeng Y, Adams NM, Bodenham DA. On two-sample testing for data with arbitrarily missing values. arXiv preprint arXiv:2403.15327. 2024 Mar 22.
#'  \item Mann, Henry B., and Donald R. Whitney. "On a test of whether one of two random variables is stochastically larger than the other." \emph{The annals of mathematical statistics} (1947): 50-60.
#'  \item Lehmann, Erich Leo, and Howard J. D'Abrera. Nonparametrics: statistical methods based on ranks. Holden-day, 1975.
#'  \item Schafer, Joseph L., and John W. Graham. "Missing data: our view of the state of the art." \emph{Psychological methods} 7.2 (2002): 147.
#' }
#'
#' @seealso [stats::wilcox.test()]
#'
#' @examples
#'
#' ### Samples assumed distinct real-values
#' X <- rnorm(100,0,1)
#' X[1:5] <- NA
#' Y <- rnorm(50,1,1)
#' Y[1:5] <- NA
#' wmwm.test(X,Y)
#'
#' ### Samples assumed potentially tied
#' X <- rpois(50,1)
#' X[1:5] <- NA
#' Y <- rpois(50,3)
#' wmwm.test(X,Y)
#' wmwm.test(X,Y, ties = TRUE)
#'
wmwm.test <- function(X, Y, alternative = c("two.sided", "less",
                                  "greater"), ties = NULL,
                                  lower.boundary = -Inf,
                                  upper.boundary = Inf,
                                  exact = NULL, correct = TRUE)
  {

  alternative <- match.arg(alternative)
  DNAME <- paste(deparse(substitute(X)), "and", deparse(substitute(Y)))

  # Remove all infinite
  X <- X[is.finite(X) | is.na(X)]
  Y <- Y[is.finite(Y) | is.na(Y)]

  # Check input
  if(!checkInput(X, Y)){
    ######## checkInput return 0, thus either X or Y contains only NA ##########

    warning("either 'X' or 'Y' does not contain any observed sample")

    # check ties
    ties <- checkTies(X,Y,ties)

    ## broadest bounds only
    BOUNDSWMW <- c(0, length(X)*length(Y))
    BOUNDSPVALUE <- c(0,1)

    ## description of bounds
    DESCRIPTIONBOUNDS <- "either 'X' or 'Y' does not contain any observed sample."

  }else{
    #### checkInput return 1, thus both X and Y contains at least one observed samples ####

    ties <- checkTies(X,Y,ties)

    ########### compute bounds
    if(ties) {
    ######################## checkInput return 1 ###############################
      # compute the bounds of p-values
      BOUNDS <- boundsPValueWithTies(X,Y, alternative = alternative,
                                     lower.boundary = lower.boundary,
                                     upper.boundary = upper.boundary,
                                     exact = exact, correct = correct)
      BOUNDSWMW <- BOUNDS[1:2]
      BOUNDSPVALUE <- BOUNDS[3:4]
      exact <- BOUNDS[5]

    }else{

      BOUNDS <- boundsPValueNoTies(X,Y,alternative = alternative, exact = exact,
                                   correct = correct)

      BOUNDSWMW <- BOUNDS[1:2]
      BOUNDSPVALUE <- BOUNDS[3:4]
      exact <- BOUNDS[5]
    }


    ########### description of bounds
    if(length(c(X,Y)) ==  length(c(X[!is.na(X)],Y[!is.na(Y)]))){
      # no missing data
      DESCRIPTIONBOUNDS <- 'X and Y are fully observed, the p-value can be computed directly.'
    }#else{
      # exist missing data

      # description of bounds of test statistic
      #if(!ties){
      #  DESCRIPTIONBOUNDSSTATISTIC <- 'bounds.statistic is the bounds of the test statistic assuming all samples are distinct;'
      #}else{
      #  DESCRIPTIONBOUNDSSTATISTIC <- 'bounds.statistic is the bounds of the test statistic assuming samples are potentially tied'

        #if( (lower.boundary == 'equal')&(upper.boundary != 'equal')   ){
         # DESCRIPTIONBOUNDSSTATISTIC <- paste(DESCRIPTIONBOUNDSSTATISTIC, ' samples not smaller than the minimum observed samples;', sep ="")
        #}else if((lower.boundary != 'equal')&(upper.boundary == 'equal')){
        #  DESCRIPTIONBOUNDSSTATISTIC <- paste(DESCRIPTIONBOUNDSSTATISTIC, ' samples not larger than the maximum observed samples;', sep = "")
        #}else if((lower.boundary == 'equal')&(upper.boundary == 'equal')){
        #  DESCRIPTIONBOUNDSSTATISTIC <- paste(DESCRIPTIONBOUNDSSTATISTIC, ' samples not smaller than the minimum observed samples and not larger than the maximum observed samples;',sep = "")
        #}else{
      #    DESCRIPTIONBOUNDSSTATISTIC <- paste(DESCRIPTIONBOUNDSSTATISTIC,';', sep = "")
        #}

     # }


      # description of bounds of p-value
      if(exact == TRUE){
        DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the exact p-value when data are not missing'
      }else{
        if(correct == TRUE){
          DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction when data are not missing.'
        }else{
          DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the p-value obtained using normal approximation when data are not missing.'
        }

      }

    #}
  }

  #DESCRIPTIONBOUNDS <- paste(DESCRIPTIONBOUNDS, 'The null hypothesis should be rejected if', BOUNDSPVALUE[2], 'is smaller or equal than the pre-speficied significance level')

  RES <- list(p.value = BOUNDSPVALUE[2],
              bounds.statistic = BOUNDSWMW,
              bounds.pvalue =  BOUNDSPVALUE,
              alternative = alternative,
              ties.method = ties,
              description.bounds = DESCRIPTIONBOUNDS,
              data.name = DNAME)

  return(RES)
}
