# ==============================================================================
# Script 04: Feature Engineering
# Project: Crime Pattern Analysis and Crime Risk Prediction in India
# ==============================================================================

cat("----------------------------------------------------------------------\n")
cat("STEP 4: FEATURE ENGINEERING\n")
cat("----------------------------------------------------------------------\n")

# Import cleaned dataset
cleaned_path <- "Output/cleaned_crime_data.csv"
if (!file.exists(cleaned_path)) {
  stop("Cleaned dataset not found. Please run 02_cleaning.R first.")
}

crime <- read.csv(cleaned_path, stringsAsFactors = FALSE)

cat("Input data rows:", nrow(crime), "\n\n")

# 1. Feature: Conviction Rate
cat("1. Computing Conviction_Rate (Convictions / Cases_Reported)...\n")
crime$Conviction_Rate <- ifelse(crime$Cases_Reported > 0, 
                                round(crime$Convictions / crime$Cases_Reported, 4), 0)

# 2. Feature: Chargesheet Rate
cat("2. Computing Chargesheet_Rate (Chargesheeted / Cases_Reported)...\n")
crime$Chargesheet_Rate <- ifelse(crime$Cases_Reported > 0, 
                                 round(crime$Chargesheeted / crime$Cases_Reported, 4), 0)

# 3. Feature: Cases_Per_100k
cat("3. Computing Cases_Per_100k ((Cases_Reported / Population) * 100,000)...\n")
crime$Cases_Per_100k <- round((crime$Cases_Reported / crime$Population) * 100000, 2)

# 4. Feature: Data-driven Crime Risk Categories (Low, Medium, High)
cat("4. Creating statistical, data-driven Crime_Risk categories...\n")

# Use 33.33rd and 66.67th percentiles (quantiles) for balanced thresholding
quantiles <- quantile(crime$Crime_Rate_per_100k, probs = c(0.3333, 0.6667), na.rm = TRUE)
q1_thresh <- unname(round(quantiles[1], 2))
q2_thresh <- unname(round(quantiles[2], 2))

cat("   Thresholds derived from dataset Crime_Rate_per_100k distribution:\n")
cat("   - Low Risk    : Crime_Rate <= ", q1_thresh, "\n")
cat("   - Medium Risk : ", q1_thresh, " < Crime_Rate <= ", q2_thresh, "\n")
cat("   - High Risk   : Crime_Rate > ", q2_thresh, "\n\n")

crime$Crime_Risk <- cut(
  crime$Crime_Rate_per_100k,
  breaks = c(-Inf, q1_thresh, q2_thresh, Inf),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

# Convert categorical variables to factors
crime$State <- as.factor(crime$State)
crime$District <- as.factor(crime$District)
crime$Crime_Type <- as.factor(crime$Crime_Type)
crime$Crime_Risk <- as.factor(crime$Crime_Risk)

cat("Crime_Risk Distribution:\n")
print(table(crime$Crime_Risk))

cat("\nSummary of engineered features:\n")
print(summary(crime[, c("Conviction_Rate", "Chargesheet_Rate", "Cases_Per_100k", "Crime_Risk")]))

# Save feature engineered dataset
write.csv(crime, "Output/cleaned_crime_data.csv", row.names = FALSE)

cat("\nFeature engineering complete. Saved updated data to: Output/cleaned_crime_data.csv\n")
cat("----------------------------------------------------------------------\n")
cat("STEP 4 COMPLETED SUCCESSFULLY!\n")
cat("----------------------------------------------------------------------\n\n")
