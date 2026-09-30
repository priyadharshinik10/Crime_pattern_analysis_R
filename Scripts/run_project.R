# ==============================================================================
# MASTER RUNNER SCRIPT: Crime Pattern Analysis & Risk Prediction in R
# Project: Crime Pattern Analysis and Crime Risk Prediction in India
# ==============================================================================

cat("======================================================================\n")
cat("STARTING COMPLETE PROJECT EXECUTION WORKFLOW\n")
cat("======================================================================\n\n")

start_time <- Sys.time()

# Helper function to run a script safely
run_step <- function(script_path, step_name) {
  cat(paste0(">>> Running ", step_name, " (", script_path, ")...\n"))
  if (!file.exists(script_path)) {
    stop("Script file not found: ", script_path)
  }
  source(script_path, local = FALSE)
  cat(paste0("<<< Completed ", step_name, " successfully!\n\n"))
}

# Execute Workflow Steps in Order
run_step("Scripts/01_import.R", "Step 1: Data Import & Structure Inspection")
run_step("Scripts/02_cleaning.R", "Step 2: Data Cleaning & Quality Audit")
run_step("Scripts/04_feature_engineering.R", "Step 3: Feature Engineering & Risk Categorization")
run_step("Scripts/03_eda.R", "Step 4: Exploratory Data Analysis & Visualization")
run_step("Scripts/05_machine_learning.R", "Step 5: Temporal Next-Year Machine Learning Modeling")
run_step("Scripts/06_prediction.R", "Step 6: Temporal Next-Year Test Predictions")

end_time <- Sys.time()
total_duration <- round(difftime(end_time, start_time, units = "secs"), 2)

cat("======================================================================\n")
cat("VERIFICATION OF GENERATED ARTIFACTS AND OUTPUTS:\n")
cat("======================================================================\n")

# Check CSV files
csv_files <- c(
  "Output/cleaned_crime_data.csv",
  "Output/model_results.csv",
  "Output/predictions.csv"
)

cat("\n1. Output CSV Files:\n")
for (f in csv_files) {
  if (file.exists(f)) {
    cat(sprintf("   [OK] %-30s (%d rows, %.2f KB)\n", f, nrow(read.csv(f)), file.size(f)/1024))
  } else {
    cat(sprintf("   [MISSING] %s\n", f))
  }
}

# Check Graphs
cat("\n2. Generated Visualization PNGs (Graphs/):\n")
png_files <- list.files("Graphs", pattern = "\\.png$", full.names = TRUE)
cat(sprintf("   Total PNG files generated: %d\n", length(png_files)))
for (f in sort(png_files)) {
  cat(sprintf("   [OK] %-35s (%.2f KB)\n", basename(f), file.size(f)/1024))
}

cat("\n======================================================================\n")
cat(sprintf("PROJECT EXECUTION COMPLETED IN %s SECONDS!\n", total_duration))
cat("======================================================================\n\n")
