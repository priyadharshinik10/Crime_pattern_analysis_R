# ==============================================================================
# Script 02: Data Cleaning and Data Quality Verification
# Project: Crime Pattern Analysis and Crime Risk Prediction in India
# ==============================================================================

cat("----------------------------------------------------------------------\n")
cat("STEP 2: DATA CLEANING AND QUALITY AUDIT\n")
cat("----------------------------------------------------------------------\n")

# Import raw data
data_path <- "Data/india_district_crime_2014_2023_30k.csv"
crime <- read.csv(data_path, stringsAsFactors = FALSE)
initial_rows <- nrow(crime)

cat("Initial dataset row count:", initial_rows, "\n\n")

# 1. Missing value check
missing_count <- sum(is.na(crime))
cat("1. Missing values check (NA count):", missing_count, "\n")

# 2. Duplicate check
duplicate_count <- sum(duplicated(crime))
cat("2. Duplicate records check:", duplicate_count, "\n")

# 3. Data type verification
cat("3. Data types verification:\n")
cat("   State:", class(crime$State), "\n")
cat("   District:", class(crime$District), "\n")
cat("   Year:", class(crime$Year), "\n")
cat("   Crime_Type:", class(crime$Crime_Type), "\n")
cat("   Cases_Reported:", class(crime$Cases_Reported), "\n")
cat("   Chargesheeted:", class(crime$Chargesheeted), "\n")
cat("   Convictions:", class(crime$Convictions), "\n")
cat("   Population:", class(crime$Population), "\n")
cat("   Crime_Rate_per_100k:", class(crime$Crime_Rate_per_100k), "\n")

# 4. Invalid numeric values check (negative counts)
invalid_negatives <- sum(crime$Cases_Reported < 0 | crime$Chargesheeted < 0 | 
                         crime$Convictions < 0 | crime$Crime_Rate_per_100k < 0)
cat("4. Negative numeric values check:", invalid_negatives, "\n")

# 5. Invalid years check (valid range 2014 to 2023)
invalid_years <- sum(crime$Year < 2014 | crime$Year > 2023)
cat("5. Invalid years check (outside 2014-2023):", invalid_years, "\n")

# 6. Zero/negative population check
invalid_pop <- sum(crime$Population <= 0)
cat("6. Zero or negative population check:", invalid_pop, "\n")

# 7. Zero denominators check (for rates)
zero_pop_denom <- sum(crime$Population == 0)
zero_cases_denom <- sum(crime$Cases_Reported == 0)
cat("7. Zero denominators check:\n")
cat("   Zero population denominators:", zero_pop_denom, "\n")
cat("   Zero cases reported denominators:", zero_cases_denom, "\n")

# 8. Basic consistency checks (Logical hierarchy)
# Chargesheeted should not exceed Cases_Reported
invalid_chargesheet <- sum(crime$Chargesheeted > crime$Cases_Reported)
# Convictions should not exceed Chargesheeted or Cases_Reported
invalid_conviction <- sum(crime$Convictions > crime$Cases_Reported | crime$Convictions > crime$Chargesheeted)

cat("8. Logical consistency checks:\n")
cat("   Chargesheeted > Cases_Reported:", invalid_chargesheet, "\n")
cat("   Convictions > Cases_Reported or Chargesheeted:", invalid_conviction, "\n")

# Data Filter / Clean step
# Retain valid records based on logical rules
cleaned_crime <- crime[
  !is.na(crime$State) &
  !is.na(crime$District) &
  !is.na(crime$Year) &
  crime$Year >= 2014 & crime$Year <= 2023 &
  crime$Population > 0 &
  crime$Cases_Reported >= 0 &
  crime$Chargesheeted >= 0 &
  crime$Convictions >= 0 &
  crime$Chargesheeted <= crime$Cases_Reported &
  crime$Convictions <= crime$Chargesheeted, 
]

final_rows <- nrow(cleaned_crime)
cat("\nSummary of Data Cleaning:\n")
cat("   Records before cleaning:", initial_rows, "\n")
cat("   Records after cleaning :", final_rows, "\n")
cat("   Records removed        :", initial_rows - final_rows, "\n")

# Ensure Output directory exists and write cleaned dataset
if (!dir.exists("Output")) {
  dir.create("Output")
}

output_csv_path <- "Output/cleaned_crime_data.csv"
write.csv(cleaned_crime, output_csv_path, row.names = FALSE)

cat("\nCleaned dataset saved successfully to:", output_csv_path, "\n")
cat("----------------------------------------------------------------------\n")
cat("STEP 2 COMPLETED SUCCESSFULLY!\n")
cat("----------------------------------------------------------------------\n\n")
