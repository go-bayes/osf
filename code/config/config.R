# Configuration Settings for Trust in Science Analysis
# Modify these settings to customize the analysis

# =============================================================================
# DATA SETTINGS
# =============================================================================

# use real data (if available) or synthetic data?
USE_REAL_DATA <- FALSE  # set to TRUE if you have access to nzavs data

# data paths (only needed if USE_REAL_DATA = TRUE)
if (USE_REAL_DATA) {
  DATA_PATH <- "/path/to/your/nzavs/data"  # update this path
  AMELIA_FILE_BOUNDS <- "dat_long_imputed_bounds_ordinal_15"
  AMELIA_FILE_LOG <- "dat_long_amelia_log"
} else {
  # use synthetic data
  DATA_PATH <- "data/synthetic"
  SYNTHETIC_DATA_FILE <- "synthetic_trust_data.rds"
}

# output directory for saved models and intermediate files
push_mods <- "data/processed"

# create directory if it doesn't exist
if (!dir.exists(push_mods)) {
  dir.create(push_mods, recursive = TRUE)
}

# =============================================================================
# ANALYSIS SETTINGS  
# =============================================================================

# multiple imputation settings
N_IMPUTATIONS <- 10  # number of imputations (reduce to 2 for quick demo)

# model settings
SPLINE_DF <- 3  # degrees of freedom for natural splines
CORRELATION_STRUCTURE <- "exchangeable"  # for gee models

# random seed for reproducibility
SEED <- 123

# =============================================================================
# OUTPUT SETTINGS
# =============================================================================

# figure settings
FIGURE_HEIGHT <- 10
FIGURE_WIDTH <- 10  
FIGURE_DPI <- 300
FIGURE_FORMAT <- c("png", "pdf")  # formats to save

# table settings
TABLE_DIGITS <- 3
SAVE_INTERMEDIATE <- TRUE  # save intermediate model outputs?

# =============================================================================
# SAMPLE INFORMATION (from manuscript)
# =============================================================================

# study details
STUDY_NAME <- "New Zealand Attitudes and Values Study"
N_PARTICIPANTS <- 4000  # reduced for faster testing
BASELINE_WAVE <- 11
BASELINE_YEAR <- 2019
FINAL_WAVE <- 15  # updated for 5 waves
FINAL_YEAR <- 2023  # corrected to match Wave 15
N_WAVES <- 5  # 5 waves!

# wave labels
WAVE_LABELS <- c(
  "Wave 11 (2019)",
  "Wave 12 (2020)",  # COVID year
  "Wave 13 (2021)",
  "Wave 14 (2022)",
  "Wave 15 (2023)"   # 5th wave
)

# =============================================================================
# VARIABLE INFORMATION
# =============================================================================

# outcome variables
TRUST_SCIENCE_VAR <- "trust_science"
TRUST_SCIENTISTS_VAR <- "trust_scientists"

# factor versions
TRUST_SCIENCE_FACTOR <- "trust_science_factor"
TRUST_SCIENTISTS_FACTOR <- "trust_scientists_factor"

# factor levels
FACTOR_LEVELS <- c("low", "med", "high")
FACTOR_LABELS <- c("Low (1-3)", "Medium (4-5)", "High (6-7)")

# id variable
ID_VAR <- "id"

# time variable
TIME_VAR <- "years"

# weight variable
WEIGHT_VAR <- "weights"

# =============================================================================
# PLOT THEMES
# =============================================================================

# color schemes
COLOR_OBSERVED <- "metro"
COLOR_IMPUTED <- "us"

# plot labels
LABEL_SCIENCE <- "Social Value of Science"
LABEL_SCIENTISTS <- "Trust in Scientists"

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# function to construct formula
make_formula <- function(outcome, predictor = TIME_VAR, spline_df = SPLINE_DF) {
  as.formula(sprintf("%s ~ ns(%s, %d)", outcome, predictor, spline_df))
}

# function to check if using real data
is_real_data <- function() {
  USE_REAL_DATA && file.exists(file.path(DATA_PATH, paste0(AMELIA_FILE_BOUNDS, ".qs")))
}

# print configuration summary
if (interactive()) {
  cat("\n=== CONFIGURATION LOADED ===\n")
  cat("Data source:", ifelse(USE_REAL_DATA, "Real NZAVS data", "Synthetic data"), "\n")
  cat("Imputations:", N_IMPUTATIONS, "\n")
  cat("Spline df:", SPLINE_DF, "\n")
  cat("Output formats:", paste(FIGURE_FORMAT, collapse = ", "), "\n")
  cat("===========================\n\n")
}
