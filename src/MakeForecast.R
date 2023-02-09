#User input
cat("\n################################################################\n")
cat(" 4 WEEK AHEAD FORECAST RELATIVE TO  WEEKLY ADJUSTED CLOSE \n")
cat("################################################################\n\n")

args <- commandArgs(trailingOnly = TRUE)
start_date <- as.Date(args[1])


cat("forecasting start_date: ", as.character(start_date))


# Calculate end date as 4 weeks ahead of start date
end_date <- start_date + as.difftime(4*7, units = "days")
cat("\n4 week ahead date: ", as.character(end_date), "\n")
cat("-----------------------------------------------------\n")

# Set DRE to constant after being bought
if (start_date < as.Date("2022-09-01")) {
  freeze_DRE <- FALSE
} else {
  freeze_DRE <- TRUE
}

# Run the first R script
source("preprocess-and-forecast.R")

# # Run the second R script
# source("src/process_data.R")




