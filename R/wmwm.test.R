# This function performs the two-sample hypothesis method with missing data
# proposed in the paper "On two-sample testing for data with arbitrarily
# missing values".
# When no missing data is presented, this function returns exactly
# the same results as stats::wilcox.test().
# When missing data is presented, this function returns a p-value that controls
# the Type I error regardless of the values of missing data,
# along with the bounds of the Wilcoxon-Mann-Whitney test statistic,
# and the bounds of the p-value without missing data.

#' @md
#' @export
#'
#' @title Wilcoxon-Mann-Whitney Test in the Presence of
#' Arbitrarily Missing Data
#'
#' @description Performs the two-sample Wilcoxon-Mann-Whitney test in the presence
#' of missing data, which controls the Type I error regardless of the values of
#' missing data.
#'
#' @usage wmwm.test(X, Y, alternative = c("two.sided", "less", "greater"),
#' ties = NULL, lower.boundary = -Inf, upper.boundary = Inf,
#' exact = NULL, correct = TRUE)
#'
#' @param X,Y numeric vectors of data values with potential missing data.
#' Inf and -Inf values will be omitted.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of "two.sided" (default), "greater" or "less".
#' You can specify just the initial letter.
#' @param exact a logical indicating whether the bounds should be of an
#' exact p-value.
#' @param ties a logical indicating whether samples could be tied.
#'   * If observed samples contain tied samples, ties defaults to TRUE.
#'   * If observed samples do not contain tied samples, ties defaults to FALSE.
#' @param lower.boundary (when ties is TRUE) a number specifying the
#' lower bound of the data set, must be smaller or equal than
#' the minimum of all observed data.
#' @param upper.boundary (when ties is TRUE) a number specifying the
#' upper bound of the data set, must be larger or equal than
#' the maximum of all observed data.
#' @param correct a logical indicating whether the bounds should be of
#' a p-value applying continuity correction in the normal approximation.
#'
#' @details \code{wmwm.test()} performs the two-sample hypothesis test method
#' proposed in (Zeng et al., 2024) for univariate data
#' when not all data are observed.
#' Bounds of the Wilcoxon-Mann-Whitney test statistic and its p-value
#' will be computed in the presence of missing data.
#' The p-value of the test method proposed in (Zeng et al., 2024) is then
#' returned as the maximum possible p-value of the Wilcoxon-Mann-Whitney test.
#'
#' By default (if \code{exact} is not specified), this function returns
#' bounds of an exact p-value if the length of \code{X} and \code{Y} are both
#' smaller than 50, and there are no tied observations.
#' Otherwise, bounds of a p-value calculated using normal approximation
#' with continuity correction will be returned.
#'
#' @return
#'
#'  \item{p.value}{the p-value for the test.}
#'
#'  \item{bounds.statistic}{bounds of the value of the Wilcoxon-Mann-Whitney
#'  test statistic.}
#'
#'  \item{bounds.pvalue}{bounds of the p-value of the Wilcoxon-Mann-Whitney
#'  test.}
#'
#'  \item{alternative}{a character string describing the alternative hypothesis.}
#'
#'  \item{ties.method}{a character string describing whether samples are
#'  considered tied.}
#'
#'  \item{description.bounds}{a character string describing the bounds of the p-value.}
#'
#'  \item{data.name}{a character string giving the names of the data.}
#'
#'
#' @references
#' \itemize{
#'  \item Zeng Y, Adams NM, Bodenham DA. On two-sample testing for data with
#'  arbitrarily missing values. arXiv preprint arXiv:2403.15327. 2024 Mar 22.
#'
#'  \item Mann, Henry B., and Donald R. Whitney. "On a test of whether one
#'  of two random variables is stochastically larger than the other."
#'  {The Annals of Mathematical Statistics} (1947): 50-60.
#'
#'  \item Lehmann, Erich Leo, and Howard J. D'Abrera. Nonparametrics:
#'  statistical methods based on ranks. Holden-day, 1975.
#' }
#'
#' @seealso [stats::wilcox.test()] when data are fully observed.
#'
#' @examples
#' #### Assume all samples are distinct.
#' X <- c(6.2, 3.5, NA, 7.6, 9.2)
#' Y <- c(0.2, 1.3, -0.5, -1.7)
#'
#' ## By default, when the sample sizes of both X and Y are smaller than 50,
#' ## exact distribution will be used.
#' wmwm.test(X, Y, ties = FALSE, alternative = 'two.sided')
#'
#' ## using normality approximation with continuity correction:
#' wmwm.test(X, Y, ties = FALSE, alternative = 'two.sided', exact = FALSE, correct = TRUE)
#'
#' #### Assume samples can be tied.
#' X <- c(6, 9, NA, 7, 9)
#' Y <- c(0, 1, 0, -1)
#'
#' ## When the samples can be tied, normality approximation will be used.
#' ## By default, lower.boundary = -Inf, upper.boundary = Inf.
#' wmwm.test(X, Y, ties = TRUE, alternative = 'two.sided')
#'
#' ## specifying lower.boundary and upper.boundary:
#' wmwm.test(X, Y, ties = TRUE, alternative = 'two.sided', lower.boundary = -1, upper.boundary = 9)


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
    #if(length(c(X,Y)) ==  length(c(X[!is.na(X)],Y[!is.na(Y)]))){
      # no missing data
      #DESCRIPTIONBOUNDS <- 'X and Y are fully observed, the p-value can be computed directly.'
    #}else{
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
        DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the exact p-value'
      }else{
        if(correct == TRUE){
          DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction'
        }else{
          DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the p-value obtained using normal approximation'
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
