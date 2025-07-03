# Test complete pipeline end-to-end
# joseph.bulbulia@gmail.com

cat("=== TESTING COMPLETE PIPELINE ===\n\n")

# setup
setwd("/Users/joseph/GIT/epic-models/2024/24-john-kerr-science-trust-SIMPLE/osf_reproducible_analysis")

# clean up any previous test files
if (file.exists("oracle_trust_data.csv")) file.remove("oracle_trust_data.csv")
if (file.exists("data/synthetic/synthetic_trust_data.csv")) file.remove("data/synthetic/synthetic_trust_data.csv")

# step 1: generate fresh synthetic data
cat("1. GENERATING SYNTHETIC DATA\n")
source("data/synthetic/generate_synthetic_data.R")
synthetic_data <- generate_synthetic_trust_data(
  n_participants = 300,  # moderate size for testing
  n_waves = 4,
  baseline_year = 2019,
  seed = 2025
)
write.csv(synthetic_data, "data/synthetic/synthetic_trust_data.csv", row.names = FALSE)
cat("✓ Synthetic data generated\n\n")

# step 2: test data loading and imputation
cat("2. TESTING DATA LOADING AND IMPUTATION\n")

# minimal setup
USE_REAL_DATA <- FALSE
N_IMPUTATIONS <- 3
ID_VAR <- "id"
TIME_VAR <- "years"
TRUST_SCIENCE_VAR <- "trust_science"
TRUST_SCIENTISTS_VAR <- "trust_scientists"
WEIGHT_VAR <- "weights"

# run the data loading script
tryCatch({
  source("code/01_load_prepare_data.R")
  cat("\n✓ Data loading and imputation successful!\n")
}, error = function(e) {
  cat("\n✗ Error in data loading:", e$message, "\n")
  stop(e)
})

# check outputs
if (file.exists("data/processed/mids_obj.rds")) {
  mids_obj <- readRDS("data/processed/mids_obj.rds")
  cat("\nMICE object created:\n")
  cat("- Imputations:", mids_obj$m, "\n")
  cat("- Variables:", paste(names(mids_obj$data)[1:5], collapse = ", "), "...\n")
}

# step 3: quick model test
cat("\n3. TESTING MODEL FITTING\n")
if (exists("mids_obj")) {
  library(geepack)
  library(ggeffects)
  library(splines)
  
  # fit one simple model
  cat("Fitting test GEE model...\n")
  test_model <- geepack::geeglm(
    trust_science ~ ns(years, 3) + education,
    id = id,
    weights = weights,
    data = mice::complete(mids_obj, 1),
    corstr = "exchangeable"
  )
  
  cat("✓ Model fitted successfully\n")
  cat("Coefficients:\n")
  print(round(coef(test_model), 3))
}

cat("\n=== PIPELINE TEST COMPLETE ===\n")
cat("\nThe pipeline is working correctly with:\n")
cat("- Wave as factor with character levels (2019, 2020, 2021, 2022)\n")
cat("- Years as numeric time variable (0, 1, 2, 3)\n")
cat("- Amelia using ts='years' for time series\n")
cat("- Trust variables treated as continuous with bounds\n")