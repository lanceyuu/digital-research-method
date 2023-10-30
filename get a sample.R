# Load required libraries
library(readxl)
library(dplyr)

# Read the Excel file
setwd("/Users/lanceyu/Dropbox (BI Norwegian Business School)/Course/PhD course/session 2")
library(readxl)
data <- read_excel("20231024_Reddit_NewTubers.xlsx")

#Get the number of rows in the data
total_rows <- nrow(data)

# Calculate the number of rows for the 10% sample
sample_size <- round(0.1 * total_rows)

# Draw a random sample of 10% of the rows
set.seed(123)  # For reproducibility
sample_data <- sample_n(data, size = sample_size)

# Optionally, write the sample to a new Excel file
#install.packages("writexl")
library(writexl)
write_xlsx(sample_data, "sample_data.xlsx")
