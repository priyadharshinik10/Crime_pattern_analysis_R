# ==============================================================================
# Script 01: Data Import and Structure Inspection
# Project: Crime Pattern Analysis and Crime Risk Prediction in India
# ==============================================================================

# Ensure output messaging is clean
cat("----------------------------------------------------------------------\n")
cat("STEP 1: IMPORTING DATASET AND INITIAL INSPECTION\n")
cat("----------------------------------------------------------------------\n")

# Define relative path to dataset
data_path <- "Data/india_district_crime_2014_2023_30k.csv"

# Check if data file exists
if (!file.exists(data_path)) {
  stop("Error: Dataset file not found at path: ", data_path)
}

# Import dataset
crime_raw <- read.csv(data_path, stringsAsFactors = FALSE)

# Display basic info
cat("Dataset loaded successfully!\n")
cat("Dimensions (Rows, Columns):", paste(dim(crime_raw), collapse = " x "), "\n\n")

cat("Column Names:\n")
print(colnames(crime_raw))

cat("\nFirst 6 Rows:\n")
print(head(crime_raw))

cat("\nLast 6 Rows:\n")
print(tail(crime_raw))

cat("\nDataset Structure:\n")
str(crime_raw)

cat("\nSummary Statistics:\n")
print(summary(crime_raw))

cat("----------------------------------------------------------------------\n")
cat("STEP 1 COMPLETED SUCCESSFULLY!\n")
cat("----------------------------------------------------------------------\n\n")