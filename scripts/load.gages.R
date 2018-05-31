# Function to collect gage numbers from user-provided csv file
load.gages <- function(gages_csv) {
  site_numbers <- read.csv(gages_csv, header = FALSE)
  paste0("0",site_numbers)
}