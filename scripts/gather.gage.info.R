# Function to store attributes of each gage
gather.gage.info <- function(gage_list) {
  a <- length(gage_list)
  b <- list()
  for (i in 1:a) {
    b[[i]] <- readNWISsite(gage_list[i])
  }
  return(b)
} 