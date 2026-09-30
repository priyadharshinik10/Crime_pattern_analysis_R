# ==============================================================================
# Script 05: Temporal Next-Year Machine Learning (Decision Tree & Random Forest)
# Project: Crime Pattern Analysis and Crime Risk Prediction in India
# ==============================================================================

cat("----------------------------------------------------------------------\n")
cat("STEP 5: TEMPORAL NEXT-YEAR MACHINE LEARNING MODELING & EVALUATION\n")
cat("----------------------------------------------------------------------\n")

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(rpart)
  library(rpart.plot)
  library(randomForest)
})

# Import cleaned & feature-engineered dataset
data_path <- "Output/cleaned_crime_data.csv"
if (!file.exists(data_path)) {
  stop("Error: cleaned_crime_data.csv not found. Please run scripts 02 and 04 first.")
}

crime <- read.csv(data_path, stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# METHODOLOGICAL CORRECTION: TEMPORAL NEXT-YEAR TARGET CONSTRUCTION
# ------------------------------------------------------------------------------
cat("METHODOLOGICAL NOTE ON TARGET LEAKAGE & TEMPORAL PREDICTION:\n")
cat("To prevent contemporaneous feature-to-target reconstruction leakage,\n")
cat("we construct a true forecasting task where current year (t) operational features\n")
cat("predict NEXT YEAR'S (t+1) Crime Risk category.\n\n")

# Sort dataset chronologically per district and crime type
temporal_crime <- crime %>%
  arrange(State, District, Crime_Type, Year) %>%
  group_by(State, District, Crime_Type) %>%
  mutate(Next_Year_Crime_Risk = lead(Crime_Risk, order_by = Year)) %>%
  ungroup() %>%
  filter(!is.na(Next_Year_Crime_Risk)) # Drop year 2023 as 2024 target is absent

cat("Dataset Transformation Completed:\n")
cat("   Total valid predictor-target pairs (Years 2014-2022):", nrow(temporal_crime), "rows\n")
cat("   Target Variable: Next_Year_Crime_Risk (Low, Medium, High)\n\n")

# Ensure factors are formatted properly
temporal_crime$Next_Year_Crime_Risk <- factor(temporal_crime$Next_Year_Crime_Risk, levels = c("Low", "Medium", "High"))
temporal_crime$State <- as.factor(temporal_crime$State)
temporal_crime$Crime_Type <- as.factor(temporal_crime$Crime_Type)

# Select predictors (current year t operational data only, excluding current/future rates)
predictors <- c("Cases_Reported", "Chargesheeted", "Convictions", "Population", 
                "Conviction_Rate", "Chargesheet_Rate", "Crime_Type", "State")

ml_data <- temporal_crime[, c("Next_Year_Crime_Risk", "Year", predictors)]

# ------------------------------------------------------------------------------
# CHRONOLOGICAL TRAIN / TEST SPLIT (TEMPORAL SEPARATION)
# ------------------------------------------------------------------------------
# Training Set: Predictor Years 2014-2021 (Predicting Targets 2015-2022) -> 24,000 rows
# Testing Set : Predictor Year 2022 (Predicting Target 2023)            ->  3,000 rows

train_data <- ml_data %>% filter(Year <= 2021) %>% select(-Year)
test_data  <- ml_data %>% filter(Year == 2022) %>% select(-Year)

cat("Chronological Data Partitioning Complete:\n")
cat("   Training Set (Years 2014-2021):", nrow(train_data), "rows\n")
cat("   Testing Set  (Year 2022)      :", nrow(test_data), "rows\n\n")

# Helper function to compute classification metrics for 3-class target
calculate_metrics <- function(actual, predicted, model_name) {
  cm <- table(Actual = actual, Predicted = predicted)
  
  # Total accuracy
  accuracy <- sum(diag(cm)) / sum(cm)
  
  classes <- levels(actual)
  precision_vec <- numeric(length(classes))
  recall_vec    <- numeric(length(classes))
  f1_vec        <- numeric(length(classes))
  
  for (i in seq_along(classes)) {
    cls <- classes[i]
    tp <- cm[cls, cls]
    fp <- sum(cm[, cls]) - tp
    fn <- sum(cm[cls, ]) - tp
    
    precision_vec[i] <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    recall_vec[i]    <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    f1_vec[i]        <- ifelse((precision_vec[i] + recall_vec[i]) > 0, 
                               2 * (precision_vec[i] * recall_vec[i]) / (precision_vec[i] + recall_vec[i]), 0)
  }
  
  macro_precision <- mean(precision_vec)
  macro_recall    <- mean(recall_vec)
  macro_f1        <- mean(f1_vec)
  
  cat("======================================================================\n")
  cat("MODEL EVALUATION:", model_name, "(Next-Year Prediction)\n")
  cat("======================================================================\n")
  cat("Confusion Matrix:\n")
  print(cm)
  cat("\nDetailed Metrics:\n")
  cat(sprintf("Accuracy       : %.4f (%.2f%%)\n", accuracy, accuracy * 100))
  cat(sprintf("Macro Precision: %.4f\n", macro_precision))
  cat(sprintf("Macro Recall   : %.4f\n", macro_recall))
  cat(sprintf("Macro F1-Score : %.4f\n\n", macro_f1))
  
  return(data.frame(
    Model = model_name,
    Accuracy = round(accuracy, 4),
    Precision = round(macro_precision, 4),
    Recall = round(macro_recall, 4),
    F1_Score = round(macro_f1, 4),
    stringsAsFactors = FALSE
  ))
}

# ------------------------------------------------------------------------------
# MODEL 1: DECISION TREE (rpart)
# ------------------------------------------------------------------------------
cat("Training Decision Tree Model on 2014-2021 Data...\n")
dt_model <- rpart(
  Next_Year_Crime_Risk ~ .,
  data = train_data,
  method = "class",
  control = rpart.control(maxdepth = 5, cp = 0.005)
)

dt_pred <- predict(dt_model, test_data, type = "class")
dt_results <- calculate_metrics(test_data$Next_Year_Crime_Risk, dt_pred, "Decision Tree")

# Save Decision Tree Visualization to Graphs/
png("Graphs/15_decision_tree.png", width = 1000, height = 700, res = 120)
rpart.plot(
  dt_model,
  main = "Decision Tree: Next-Year Crime Risk Classification",
  extra = 104,
  box.palette = "RdYlGn",
  shadow.col = "gray",
  nn = TRUE
)
dev.off()
cat("Saved Decision Tree Plot to Graphs/15_decision_tree.png\n\n")

# ------------------------------------------------------------------------------
# MODEL 2: RANDOM FOREST (randomForest)
# ------------------------------------------------------------------------------
cat("Training Random Forest Model (ntree = 100)...\n")
set.seed(42)
rf_model <- randomForest(
  Next_Year_Crime_Risk ~ .,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

rf_pred <- predict(rf_model, test_data)
rf_results <- calculate_metrics(test_data$Next_Year_Crime_Risk, rf_pred, "Random Forest")

# Extract Variable Importance
importance_matrix <- importance(rf_model)
imp_df <- data.frame(
  Feature = rownames(importance_matrix),
  MeanDecreaseGini = importance_matrix[, "MeanDecreaseGini"],
  stringsAsFactors = FALSE
) %>% arrange(desc(MeanDecreaseGini))

cat("Random Forest Feature Importance for Next-Year Prediction:\n")
print(imp_df)

# Save Variable Importance Plot to Graphs/
png("Graphs/16_variable_importance.png", width = 900, height = 600, res = 120)
par(mar = c(5, 8, 4, 2))
barplot(
  height = rev(imp_df$MeanDecreaseGini),
  names.arg = rev(imp_df$Feature),
  horiz = TRUE,
  las = 1,
  col = "#3498db",
  main = "Random Forest Feature Importance (Next-Year Prediction)",
  xlab = "Mean Decrease Gini"
)
grid(nx = NULL, ny = NA)
dev.off()
cat("Saved Variable Importance Plot to Graphs/16_variable_importance.png\n\n")

# ------------------------------------------------------------------------------
# SAVE COMBINED MODEL RESULTS TO OUTPUT
# ------------------------------------------------------------------------------
model_results <- rbind(dt_results, rf_results)
write.csv(model_results, "Output/model_results.csv", row.names = FALSE)

cat("Combined Temporal Model Evaluation Metrics Saved to Output/model_results.csv:\n")
print(model_results)

# Save trained models and test partition info for prediction script
saveRDS(list(
  rf_model = rf_model, 
  dt_model = dt_model, 
  train_data = train_data,
  test_data = test_data,
  temporal_crime = temporal_crime
), "Output/trained_models.rds")

cat("----------------------------------------------------------------------\n")
cat("STEP 5 COMPLETED SUCCESSFULLY!\n")
cat("----------------------------------------------------------------------\n\n")
