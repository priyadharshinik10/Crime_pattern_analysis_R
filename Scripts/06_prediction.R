# ==============================================================================
# Script 06: Temporal Next-Year Crime Risk Prediction
# Project: Crime Pattern Analysis and Crime Risk Prediction in India
# ==============================================================================

cat("----------------------------------------------------------------------\n")
cat("STEP 6: TEMPORAL NEXT-YEAR RISK PREDICTION (YEAR 2023 TARGET)\n")
cat("----------------------------------------------------------------------\n")

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(randomForest)
})

# Load saved trained models and temporal dataset object
models_path <- "Output/trained_models.rds"
if (!file.exists(models_path)) {
  stop("Trained models not found. Please run 05_machine_learning.R first.")
}

saved_obj <- readRDS(models_path)
rf_model <- saved_obj$rf_model
temporal_crime <- saved_obj$temporal_crime

# Filter test dataset (Predictor Year 2022 predicting Target Year 2023)
test_dataset <- temporal_crime %>% filter(Year == 2022)

# Ensure categorical predictors match training factor levels
if (!is.null(rf_model$forest$xlevels$State)) {
  test_dataset$State <- factor(test_dataset$State, levels = rf_model$forest$xlevels$State)
}
if (!is.null(rf_model$forest$xlevels$Crime_Type)) {
  test_dataset$Crime_Type <- factor(test_dataset$Crime_Type, levels = rf_model$forest$xlevels$Crime_Type)
}

cat("Loaded held-out temporal test dataset containing", nrow(test_dataset), "records.\n")
cat("Predictor Information: Year 2022 Operational Metrics\n")
cat("Target Variable      : Year 2023 Crime Risk Category (Next_Year_Crime_Risk)\n\n")

# Generate predictions using Random Forest model
cat("Generating predictions using Random Forest model...\n")
rf_pred <- predict(rf_model, test_dataset)

# Combine prediction results
predictions_df <- data.frame(
  State = test_dataset$State,
  District = test_dataset$District,
  Predictor_Year = test_dataset$Year,
  Target_Year = test_dataset$Year + 1,
  Crime_Type = test_dataset$Crime_Type,
  Cases_Reported = test_dataset$Cases_Reported,
  Population = test_dataset$Population,
  Actual_Next_Year_Risk = test_dataset$Next_Year_Crime_Risk,
  Predicted_Next_Year_Risk = rf_pred,
  Match = ifelse(test_dataset$Next_Year_Crime_Risk == rf_pred, "Correct", "Incorrect"),
  stringsAsFactors = FALSE
)

# Export predictions to Output/predictions.csv
write.csv(predictions_df, "Output/predictions.csv", row.names = FALSE)
cat("Full temporal predictions saved successfully to: Output/predictions.csv\n\n")

# Display first 20 sample predictions in console
cat("======================================================================\n")
cat("SAMPLE TEMPORAL PREDICTION RESULTS (FIRST 20 HELD-OUT RECORDS):\n")
cat("======================================================================\n")
print(head(predictions_df[, c("State", "District", "Predictor_Year", "Target_Year", 
                              "Crime_Type", "Actual_Next_Year_Risk", 
                              "Predicted_Next_Year_Risk", "Match")], 20))

# Summary accuracy on test set
correct_count <- sum(predictions_df$Match == "Correct")
total_count   <- nrow(predictions_df)
cat("\nSummary of Next-Year Temporal Test Predictions (Target Year 2023):\n")
cat(sprintf("   Total Test Records  : %d\n", total_count))
cat(sprintf("   Correct Predictions : %d\n", correct_count))
cat(sprintf("   Test Set Accuracy   : %.2f%%\n", (correct_count / total_count) * 100))
cat("----------------------------------------------------------------------\n")
cat("STEP 6 COMPLETED SUCCESSFULLY!\n")
cat("----------------------------------------------------------------------\n\n")
