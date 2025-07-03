# Quick test of pipeline with small dataset
setwd("/Users/joseph/GIT/epic-models/2024/24-john-kerr-science-trust-SIMPLE/osf_reproducible_analysis")

# generate small synthetic data
cat("Generating small test dataset...\n")
source("data/synthetic/generate_synthetic_data.R")
test_data <- generate_synthetic_trust_data(
  n_participants = 200,  # very small for quick test
  n_waves = 4,
  baseline_year = 2019,
  seed = 123
)
write.csv(test_data, "data/synthetic/synthetic_trust_data.csv", row.names = FALSE)

# test data loading
cat("\nTesting data loading...\n")
tryCatch({
  # temporarily set config
  USE_REAL_DATA <- FALSE
  N_IMPUTATIONS <- 3  # fewer imputations for speed
  
  # run data prep
  source("code/01_load_prepare_data.R")
  cat("✓ Data loading successful\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
  stop(e)
})

cat("\nTest complete!\n")