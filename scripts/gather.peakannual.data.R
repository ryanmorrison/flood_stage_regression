# Function to collect peak annual flow data for each gage
gather.peakannual.data <- function(gage_list) {
  a <- length(gage_list)
  b <- list()
  for (i in 1:a) {
    b[[i]] <- readNWISpeak(gage_list[i], convertType = FALSE)
  }
  return(b)
}