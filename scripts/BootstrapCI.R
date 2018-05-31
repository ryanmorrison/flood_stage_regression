#' BootstrapCI Conducts bootstrap to randomly sample an extreme value series 'n' times for a 
#'  specified distribution to estimate confidence interval for each given 
#'  non-exceedance probability.
#' @param fitted.model Fitted distribution (see ?frequencyAnalysis)
#' @param series A vector representing an extreme value series (e.g., annual maximum flood)
#' @param distribution A three-character name of the extreme value distribution (see ?dist.list())
#' @param n.resamples An integer representing number of re-samples to conduct
#' @param nep A vector of non-exceedance probabilities
#' @param ci The confidence interval 
#' @export
#' @import lmomco
#' @return A list containing a data frame of confidence bounds for quantile estimates for each 
#' non-exceedance probability, a matrix containing estimated distribution parameters for each resample,
#' and a matrix of quantile estimates for each resample
BootstrapCI <- function(series, distribution, n.resamples=1E3, nep=nonexceeds(), ci=0.95) {

  # compute frequency analysis
  fa <- FrequencyAnalysis(series=series, distribution=distribution, nep=nep)
  
  # extract fitted model parameters and flag as to whether the 
  # distribution is based on log transformed data
    base.params <- fa$distribution$parameters
    isTransformed <- fa$distribution$logTransformed
    
    # create output matrices to store parameter sets and quantile estimates
    param.sets <- matrix(NA, nrow = n.resamples, ncol = length(base.params$para))
    quantile.estimates <- matrix(NA, nrow = n.resamples, ncol = length(nep), 
                dimnames = list(NULL, nep) ) 
    
    # begin bootstrapping procedure
    for(i in 1:n.resamples) {
    
        valid.moments <- FALSE
    j <- 0
    
    # allow up to 20 re-tries to re-sample 
    while(!valid.moments & j < 20) {  
      
      # sample 'n' random variates from base distribution
          data <- rlmomco(n=length(series), base.params)
            
          # compute sample l-moments
      sample.moms = lmom.ub(data)
        
      valid.moments <- are.lmom.valid(sample.moms)
      j <- j + 1
    }
    
    # error handling
        if(!valid.moments) {
       stop("Bootstrapping failed to sample valid l-moments")
        } else {
      # estimate distribution parameters
          dist.par <- lmom2par(sample.moms, base.params$type)
        
          # store the distribution parameters
          param.sets[i,] <- dist.par$para
  
          # estimate quantiles at NEP
          estimated <- qlmomco(nep, dist.par)
        
      # convert quantile estimates to real values if
      # distribution was transformed
          if(isTransformed) estimated <- 10^estimated
        
          # store the quantiles at the desired AEP values
          quantile.estimates[i,] <- estimated
      } 
  
    }
  
  # now calculate confidence limits for quantiles
  p <- c((1-ci)/2, (1+ci)/2)
  ci <- sapply(colnames(quantile.estimates), 
               FUN=function(x){
                quantile(quantile.estimates[,x], probs=p, na.rm=TRUE)})

    # now return list object containing output
    return(
        list(
              ci = data.frame(
                nonexceed_prob = nep,
                lower = as.vector(ci[1,]),
                true = fa$output$estimate,
                upper = as.vector(ci[2,]) ),
              parameters = param.sets,
              quantiles = quantile.estimates)
  )
  
}