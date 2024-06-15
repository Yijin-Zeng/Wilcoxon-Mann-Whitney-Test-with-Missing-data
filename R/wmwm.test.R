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
        DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the exact p-value.'
      }else{
        if(correct == TRUE){
          DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the p-value obtained using normal approximation with continuity correction.'
        }else{
          DESCRIPTIONBOUNDS <- 'bounds.pvalue is the bounds of the p-value obtained using normal approximation.'
        }

      }

    #}
  }

  DESCRIPTIONBOUNDS <- paste(DESCRIPTIONBOUNDS, 'The null hypothesis should be rejected if', BOUNDSPVALUE[2], 'is smaller or equal than the pre-speficied significance level')

  RES <- list(p.value = BOUNDSPVALUE[2],
              bounds.statistic = BOUNDSWMW,
              bounds.pvalue =  BOUNDSPVALUE,
              alternative = alternative,
              description.bounds = DESCRIPTIONBOUNDS,
              data.name = DNAME)

  return(RES)

}
