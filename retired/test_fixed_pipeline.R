# Test pipeline with fixed synthetic data structure
# joseph.bulbulia@gmail.com

cat("=== TESTING FIXED DATA STRUCTURE ===\n\n")

# setup
setwd("/Users/joseph/GIT/epic-models/2024/24-john-kerr-science-trust-SIMPLE/osf_reproducible_analysis")

# step 1: generate synthetic data with correct structure
cat("1. GENERATING SYNTHETIC DATA\n")
source("data/synthetic/generate_synthetic_data.R")

# generate small test data
test_data <- generate_synthetic_trust_data(
  n_participants = 200,
  n_waves = 4,
  baseline_year = 2019,
  seed = 999
)

# check data structure
cat("\nData structure check:\n")
cat("- Columns:", paste(names(test_data), collapse = ", "), "\n")
cat("- Wave class:", class(test_data$wave), "\n")
cat("- Wave levels:", paste(levels(test_data$wave), collapse = ", "), "\n")
cat("- Years unique values:", paste(sort(unique(test_data$years)), collapse = ", "), "\n")
cat("- Years class:", class(test_data$years), "\n")

# save data
write.csv(test_data, "data/synthetic/synthetic_trust_data.csv", row.names = FALSE)

# step 2: test Amelia imputation
cat("\n2. TESTING AMELIA IMPUTATION\n")

# prepare for amelia
library(Amelia)
library(mice)

# set parameters
USE_REAL_DATA <- FALSE
N_IMPUTATIONS <- 3  # just 3 for testing

# convert wave to character (Amelia requirement for idvars)
test_data$wave <- as.character(test_data$wave)

# prepare variables
id_vars <- c("id", "wave", "age_baseline")
nominal_vars <- c("gender", "ethnicity")
ordinal_vars <- c("education")

# bounds for trust variables
bounds_matrix <- matrix(c(1, 1, 7, 2, 1, 7, 3, 1, 7), nrow = 3, ncol = 3, byrow = TRUE)
rownames(bounds_matrix) <- c("trust_science", "trust_scientists", "education")

# run amelia
cat("Running Amelia with ts='years' and wave in idvars...\n")
tryCatch({
  amelia_result <- Amelia::amelia(
    test_data,
    m = N_IMPUTATIONS,
    idvars = id_vars,
    noms = nominal_vars,
    ords = ordinal_vars,
    bounds = bounds_matrix,
    empri = 0.01 * nrow(test_data),
    ts = "years",
    cs = "id",
    p2s = 0
  )
  
  cat("✓ Amelia imputation successful!\n")
  cat("  Generated", length(amelia_result$imputations), "imputations\n")
  
  # check first imputation
  imp1 <- amelia_result$imputations[[1]]
  cat("\nFirst imputation summary:\n")
  cat("  Rows:", nrow(imp1), "\n")
  cat("  Complete trust_science:", sum(!is.na(imp1$trust_science)), "\n")
  cat("  Missing trust_science:", sum(is.na(imp1$trust_science)), "\n")
  
}, error = function(e) {
  cat("✗ Amelia error:", e$message, "\n")
})

cat("\n=== TEST COMPLETE ===\n")