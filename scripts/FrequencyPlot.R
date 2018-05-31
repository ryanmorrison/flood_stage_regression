#' FrequencyPlot Generates a nice-looking (hydrologist-centric) frequency plot
#' @param series A vector representing an extreme value series (e.g., annual maximum flood)
#' @param ci A data frame containing confidence intervals (lower, true, upper) derived from 
#'  calling BootstrapCI()
#' @export ggplot
#' @import ggplot2
#' @import scales
frequencyPlot <- function(series, ci) {
  
  library(ggplot2)
  library(scales)
  
  # determine plotting positions
  bwpeaks <- data.frame(PROB = pp(series, sort = FALSE), FLOW = series)
  # xbreaks <- c(0.002,0.01,0.1,0.25,0.5,0.8,0.9,0.95,0.975,0.99,0.995, 0.998)
  xbreaks <- c(0.002,0.01,0.1,0.25,0.5,0.8,0.9,0.95,0.975,0.99, 0.998) #remove 200-year label
  log.range <- log10(range(series, ci[,-1], na.rm = TRUE))
  lower <- 10^floor(log.range[1])
  upper <- 10^ceiling(log.range[2])
  cap <- lower
  ybreaks <- NULL
  while(cap < upper) {
    ybreaks <- c(ybreaks, seq(cap, cap*9, by = cap*10))
    cap <- cap * 10
  }
  
  # now plot
  ggplot(bwpeaks) + 
    geom_point(aes(x=PROB, y=FLOW)) + 
    theme_bw(base_size = 14) + 
    scale_y_continuous(trans="log10", breaks=ybreaks, name=expression(paste("Discharge [", m^{3}*s^{-1},"]"))) +
    scale_x_continuous(trans=probability_trans(distribution="norm"),
                       breaks=xbreaks, labels=signif(prob2T(xbreaks), digits=4),
                       name="Return period [yrs]") +
    geom_line(data=ci, aes(x=nonexceed_prob, y=true), color="red") +
    geom_line(data=ci, aes(x=nonexceed_prob, y=lower), color="red", lty=2) +
    geom_line(data=ci, aes(x=nonexceed_prob, y=upper), color="red", lty=2)
  
}