# Test complete pipeline with fixed synthetic data
# joseph.bulbulia@gmail.com

cat("=== TESTING COMPLETE PIPELINE ===\n\n")

# setup
library(tidyverse)

# set working directory if needed
if (!file.exists("code/config/config.R")) {
  setwd("/Users/joseph/GIT/epic-models/2024/24-john-kerr-science-trust-SIMPLE/osf_reproducible_analysis")
}

# set to use synthetic data
cat("Setting up to use synthetic data...\n")
source("code/config/config.R")
USE_REAL_DATA <- FALSE  # force synthetic data

# step 1: generate synthetic data
cat("\n1. GENERATING SYNTHETIC DATA\n")
source("data/synthetic/generate_synthetic_data.R")
synthetic_data <- generate_synthetic_trust_data(
  n_participants = 1000,  # smaller for testing
  n_waves = 4,
  baseline_year = 2019,
  seed = 2024
)
write.csv(synthetic_data, 
          "data/synthetic/synthetic_trust_data.csv",
          row.names = FALSE)
cat("✓ Synthetic data generated and saved\n")

# step 2: load and prepare data
cat("\n2. LOADING AND PREPARING DATA\n")
tryCatch({
  source("code/01_load_prepare_data.R")
  cat("✓ Data loading completed successfully\n")
}, error = function(e) {
  cat("✗ Error in data loading:", e$message, "\n")
  stop(e)
})

# step 3: fit models
cat("\n3. FITTING MODELS\n")
tryCatch({
  source("code/02_fit_models.R")
  cat("✓ Model fitting completed successfully\n")
}, error = function(e) {
  cat("✗ Error in model fitting:", e$message, "\n")
  stop(e)
})

# check results
cat("\n=== PIPELINE TEST COMPLETE ===\n")
cat("\nChecking outputs:\n")

# check processed data
if (file.exists("data/processed/mids_obj.rds")) {
  mids_obj <- readRDS("data/processed/mids_obj.rds")
  cat("✓ MICE object created with", mids_obj$m, "imputations\n")
}

# check model outputs
if (file.exists("results/model_outputs/predictions_all.rds")) {
  predictions <- readRDS("results/model_outputs/predictions_all.rds")
  cat("✓ Predictions generated for:\n")
  cat("  - Observed data:", names(predictions$observed), "\n")
  cat("  - Imputed data:", names(predictions$imputed), "\n")
  
  # show key results
  cat("\nKey results - Trust in Science:\n")
  obs <- predictions$observed$gee_science
  imp <- predictions$imputed$gee_science
  
  cat("Observed: Year 0 =", round(obs$predicted[1], 3), 
      ", Year 3 =", round(obs$predicted[4], 3), "\n")
  cat("Imputed:  Year 0 =", round(imp$predicted[1], 3),
      ", Year 3 =", round(imp$predicted[4], 3), "\n")
  cat("Difference shows selection bias:", 
      round(obs$predicted[4] - imp$predicted[4], 3), "\n")
}

cat("\n✓ Pipeline test successful!\n")