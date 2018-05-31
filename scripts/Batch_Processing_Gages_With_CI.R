#######################################################
### Initialization
#######################################################

# Load packages
library(dataRetrieval)
library(ggplot2)
library(lmomco)

# Set working directory
setwd("/Users/Morrison/Documents/Research Projects/Hydrogeomorphic Floodplain Delineation/flood_stage_regressions")

# Load flood frequency analysis code
source("scripts/FrequencyAnalysis.R")
source("scripts/BootstrapCI.R")
source("scripts/FrequencyPlot.R")

# Load other functions
source("scripts/load.gages.R")
source("scripts/gather.gage.info.R")
source("scripts/gather.peakannual.data.R")
source("scripts/bootstrap_loess.R")


#######################################################
### Gather and filter peak discharge information
#######################################################

# Store gage numbers
gage_numbers <- load.gages("data/test2_gages.csv")

# Store gage attributes
gage_info <- gather.gage.info(gage_numbers)

# Collect and store peak annual flow data for each gage
gage_peakannual <- gather.peakannual.data(gage_numbers)

# Filter data using USGS qualification codes
# See http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes/help?output_formats_help#published_peak_streamflow_data
filtercodes_peak_cd <- c("1", "2", "3", "4", "8", "A", "B") 
filtercodes_gage_ht_cd <- c("1", "2", "3", "4", "5")

gage_peakannual_filter <- lapply(gage_peakannual, 
                                 subset, 
                                 !peak_cd%in%filtercodes_peak_cd)
gage_peakannual_filter <- lapply(gage_peakannual_filter, 
                                 subset, 
                                 !gage_ht_cd%in%filtercodes_gage_ht_cd)

# Only keep gage number, peak annual discharge, and peak annual stage
gage_peakannual_only <- lapply(gage_peakannual_filter, 
                               subset, 
                               select=c("site_no", "gage_ht", "peak_va"))

# Remove rows with NA
gage_peakannual_only <- lapply(gage_peakannual_only, 
                               function(x) na.omit(x))

# Collect base elevations for each gage
base_elevations <- lapply(gage_info, 
                          function(x) x[which(colnames(x)=="alt_va")])


# Add base elevations to gage heights
for (i in 1:length(base_elevations)) {
  for (j in 1:nrow(gage_peakannual_only[[i]]))
  gage_peakannual_only[[i]][j,4] <- gage_peakannual_only[[i]][j,2] + base_elevations[[i]]
}

# Convert stage English units to SI units
gage_peakannual_only <- lapply(gage_peakannual_only, 
                             function(x) cbind(x, x$alt_va *0.3048))

# Convert discharge English units to SI units
gage_peakannual_only <- lapply(gage_peakannual_only, 
                               function(x) cbind(x, x$peak_va *0.0283168))

# Remove columns with English units and rename columns
gage_peakannual_si <- lapply(gage_peakannual_only, '[', c(1, 5,6))
gage_peakannual_si <- lapply(gage_peakannual_si, 
                             setNames, c("site_no", "stage_m", "discharge_cms"))

# Create and store rating curves for each gage
# ratingcurves <- lapply(gage_peakannual_only, 
#                        function(x) qplot(peak_va, alt_va, data = x))


#######################################################
### Flood frequency analysis
#######################################################

# Collect only peak annual discharge records
ffa_data <- lapply(gage_peakannual_si, subset, select=c("discharge_cms"))

# Specify Log Pearson III distribution
dist <- "lp3"

# Calculate flood frequencies for each gage
ffa <- lapply(ffa_data, function(x) FrequencyAnalysis(series=unname(unlist(x)), 
                                                      distribution=dist))

# Calculate bootstrap confidence intervals for each gage
confidence <- lapply(ffa_data, function(x) BootstrapCI(series = unname(unlist(x)),
             distribution = dist,
             n.resamples = 1E3,
             ci = 0.95))

# Collect return period discharges
# No longer need to use this code because the new confidence interval codes
# included the upper, lower, and "true" values of Q for each return period.
return_period_25 <- lapply(ffa, function(x) x[[2]][25,3])
return_period_50 <- lapply(ffa, function(x) x[[2]][26,3])
return_period_100 <- lapply(ffa, function(x) x[[2]][27,3])

return_period_25_ci <- lapply(confidence, function(x) x[[1]][25,2:4])
return_period_50_ci <- lapply(confidence, function(x) x[[1]][26,2:4])
return_period_100_ci <- lapply(confidence, function(x) x[[1]][27,2:4])


# all_return_periods <- lapply(ffa, function(x) x[[2]])
all_return_periods_ci <- lapply(confidence, function(x) x[[1]])

# Extract all "ci" elements from lists
ci_elements <- lapply(confidence, subset, select = c("ci"))

# Plot all flood frequency curves
p <- vector("list", length = length(ffa_data))
for (k in 1:length(ffa_data)) {
  peakflow <- unname(unlist(ffa_data[k]))
  bounds <- as.data.frame(all_return_periods_ci[k])
  p[[k]] <- frequencyPlot(peakflow, bounds)
}
p


#######################################################
### Estimate stage for 100-year discharge using stage vs. discharge
#######################################################

# Fit LOESS curve to stage vs discharge data
loess_param <- lapply(gage_peakannual_si, 
                      function(x) loess(unlist(x[3]) ~ unlist(x[2])))

# # Bootstrap LOESS
# a <- vector("list", length = length(loess_param))
# for (q in 1:length(loess_param)) {
#   loess_input <- loess_param[[q]]
#   # a[[q]] <- l.boot(loess_input, R = 10) 
# }
# 
# gages_loess_boot <- lapply(loess_param, function(x) l.boot(x, R = 100))
# gages_loess_boot <- lapply(loess_param, loess.boot(lo.object = loess_param,
#                                                    R = 100))
# gages_loess_boot <- loess.boot(loess_param, R = 100)

# Plot LOESS curve
# j <- order(MaxAnnual$Discharge_cfs)
# lines(MaxAnnual$Discharge[j], ls$fitted[j], col = "red")

# Predict stages for return periods
stage_25 <- list()
for (i in 1:length(loess_param)) {
  stage_25[[i]] <- predict(loess_param[[i]], return_period_25[[i]])
}

stage_50 <- list()
for (i in 1:length(loess_param)) {
  stage_50[[i]] <- predict(loess_param[[i]], return_period_50[[i]])
}

stage_100 <- list()
for (i in 1:length(loess_param)) {
  stage_100[[i]] <- predict(loess_param[[i]], return_period_100[[i]])
}


#######################################################
### Classify gages as "reference" or "nonreference"
#######################################################

# Load Gages II database records
gagesII <- read.csv("data/gagesII.txt",
                    colClasses = c("numeric", 
                                   rep("character", 4), 
                                   "numeric", 
                                   "character", 
                                   rep("numeric", 2), 
                                   rep("character", 3), 
                                   rep("numeric", 3), 
                                   "character"))

gagesII_inclusion <- data.frame(nrow = length(gage_numbers))
for (i in 1:length(gage_numbers)) {
  if (gage_numbers[i]%in%gagesII$STAID) {
    gagesII_inclusion[i] <- subset(gagesII, 
                                   gagesII$STAID == gage_numbers[i], 
                                   select = "CLASS") 
  }
    else {"Missing GagesII"}
}


#######################################################
### Organize and export results
#######################################################

gage_numbers_df <- as.data.frame(gage_numbers)
gage_lat <- as.data.frame(unlist(lapply(gage_info, function(x) x[7])))
gage_lon <- as.data.frame(unlist(lapply(gage_info, function(x) x[8])))
gage_datum <- as.data.frame(unlist(lapply(gage_info, function(x) x[12])))
gage_drainarea <- as.data.frame(unlist(lapply(gage_info, function(x) x[30])))
gage_q25 <- as.data.frame(unlist(return_period_25))
gage_q50 <- as.data.frame(unlist(return_period_50))
gage_q100 <- as.data.frame(unlist(return_period_100))
gage_h25 <- as.data.frame(unlist(stage_25))
gage_h50 <- as.data.frame(unlist(stage_50))
gage_h100 <- as.data.frame(unlist(stage_100))
ref <- t(gagesII_inclusion)

gage_output <- cbind(gage_numbers_df, 
                     gage_lat, 
                     gage_lon, 
                     gage_datum, 
                     gage_drainarea, 
                     gage_q25,
                     gage_q50, 
                     gage_q100, 
                     gage_h25, 
                     gage_h50, 
                     gage_h100, 
                     ref)
colnames(gage_output) <- c("gage", 
                           "latitude", 
                           "longitude", 
                           "datum", 
                           "drain_area", 
                           "q25",
                           "q50", 
                           "q100", 
                           "h25",
                           "h50", 
                           "h100", 
                           "gagesII")
rownames(gage_output) <- NULL

write.csv(gage_output, 
          file = "output/susquehanna_gage_discharge_stage_results.csv", 
          row.names = FALSE)
