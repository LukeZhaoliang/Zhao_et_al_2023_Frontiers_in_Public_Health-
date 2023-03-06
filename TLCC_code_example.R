
setwd("~/Documents/Dell F/RLanguage/Other_data_analysis/All_data")

# Read the datasets into R
N1 <- read.csv("TLCC_N1.csv")
Cases <- read.csv("TLCC_Cases.csv")

# Convert the date column to a date format
N1$Date <- as.Date(N1$Date, "%m/%d/%y")
Cases$Date <- as.Date(Cases$Date, "%m/%d/%y")

# Extract the numeric columns for cross-correlation analysis
N1 <- N1[, -1]
Cases <- Cases[, -1]

# Calculate the cross-correlation function between N1 and Cases
ccf_result <- ccf(N1, Cases, lag.max = 60, plot = TRUE)

# Find the lag time corresponding to the maximum cross-correlation
max_lag <- ccf_result$lag[which.max(ccf_result$acf)]

# Print the lag time
cat("The lag time between N1 and Cases is", max_lag, "days.")
