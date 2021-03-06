#######################################################
### Initialization
#######################################################

# Load packages
library(dataRetrieval)
library(ggplot2)
library(dplyr)

# Set working directory
setwd("/Users/Morrison/Documents/Active Research Projects/Hydrogeomorphic Floodplain Delineation/flood_stage_regression")

# Load other functions
source("scripts/load.gages.R")
source("scripts/gather.gage.info.R")
source("scripts/gather.peakannual.data.R")

#######################################################
### Gather and filter peak discharge information
#######################################################

# Collect gage numbers stored in csv file
gage_numbers <- load.gages("data/wabash_gages.csv")

# Store gage attributes
gage_info <- gather.gage.info(gage_numbers)

# Collect and store peak annual flow data for each gage
gage_peakannual <- gather.peakannual.data(gage_numbers)

# Find the start and end datas for each gage using the "whatNWISdata" function
other_gage_data <- whatNWISdata(gage_numbers, service="dv", statCd="00003", parameterCd="00060")
other_gage_data <- other_gage_data %>% arrange(site_no)
start_date <- other_gage_data %>% select(begin_date)
end_date <- other_gage_data %>% select(end_date)

# Replace end date with the date on which regressions were performed.
end_date <- end_date %>% mutate(end_date=replace(end_date, end_date == "2018-05-31", "2018-01-15"))


# Filter data using USGS qualification codes
# See http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes/help?output_formats_help#published_peak_streamflow_data
filtercodes_peak_cd <- c("1", "2", "3", "4", "8", "A", "B") 
filtercodes_gage_ht_cd <- c("1", "2", "3", "4", "5")

gage_peakannual_filter <- lapply(gage_peakannual, subset, !peak_cd%in%filtercodes_peak_cd)
gage_peakannual_filter <- lapply(gage_peakannual_filter, subset, !gage_ht_cd%in%filtercodes_gage_ht_cd)
#gage_peakannual_filter <- lapply(gage_peakannual_filter, function(x) na.omit(x))

# Collect record length for each gage
gagerecordlength <- list()
for (i in 1:length(gage_peakannual_filter)) {
  gagerecordlength[i] <- length(gage_peakannual_filter[[i]]$peak_dt)
}
gagerecordlength

gagelengthmean <- mean(sapply(gagerecordlength, mean))
gagelengthmean

#######################################################
### Organize and export results
#######################################################

gage_numbers_df <- as.data.frame(gage_numbers)
gage_name <- as.data.frame(unlist(lapply(gage_info, function(x) x[3])))
gage_lat <- as.data.frame(unlist(lapply(gage_info, function(x) x[7])))
gage_lon <- as.data.frame(unlist(lapply(gage_info, function(x) x[8])))
#gage_datum <- as.data.frame(unlist(lapply(gage_info, function(x) x[12])))
gage_drainarea <- as.data.frame(unlist(lapply(gage_info, function(x) x[30])))
gage_drainarea <- gage_drainarea * 1/0.38610 #Convert drainage area from sq. mi. to sq. km.
gage_begin_date <- as.data.frame
gagerecordlength <- as.data.frame(unlist(gagerecordlength))

gage_output <- cbind(gage_numbers_df,
                     gage_name,
                     gage_lat, 
                     gage_lon, 
                     #gage_datum, 
                     gage_drainarea, 
                     gagerecordlength)

# Re-arrange data in ascending order of gage number
gage_output <- gage_output %>% arrange(gage_numbers)

# Add start and end dates of gage records
gage_output <- cbind(gage_output, start_date, end_date)

colnames(gage_output) <- c("gage", 
                           "name",
                           "latitude", 
                           "longitude", 
                           #"datum", 
                           "drain_area_km2", 
                           "record_length_yr",
                           "start_date",
                           "end_date")

rownames(gage_output) <- NULL

write.csv(gage_output, 
          file = "output/wabash_gage_data.csv", 
          row.names = FALSE)
