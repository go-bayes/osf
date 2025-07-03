# Load and Prepare Data for Trust in Science Analysis
# This script loads either real NZAVS data or synthetic demonstration data
# joseph.bulbulia@gmail.com

# setup
cat("==========================================================================\n")
cat("STEP 1: LOAD AND PREPARE DATA\n")
cat("==========================================================================\n\n")

# source configuration (must come first to set push_mods)
source("code/config/config.R")

# source custom functions
source("code/functions/margot_amelia_to_mice_fixed.R")

# Note: fix_mids_factors.R is sourced later if needed

# check data source
cat("Data source:", ifelse(USE_REAL_DATA, "Real NZAVS data", "Synthetic data"), "\n\n")

if (USE_REAL_DATA) {
  # ========================================================================
  # LOAD REAL DATA
  # ========================================================================
  cat("Loading real NZAVS data...\n")
  
  # check if data files exist
  amelia_bounds_path <- file.path(DATA_PATH, paste0(AMELIA_FILE_BOUNDS, ".qs"))
  
  if (!file.exists(amelia_bounds_path)) {
    stop("Cannot find Amelia bounds file at: ", amelia_bounds_path, 
         "\nPlease update DATA_PATH in config.R or set USE_REAL_DATA = FALSE")
  }
  
  # load amelia objects
  cat("Loading Amelia objects...\n")
  amelia_bounds <- qs::qread(amelia_bounds_path)
  
  # load original data (for mice conversion)
  cat("Loading original data with missing values...\n")
  original_data_path <- file.path(DATA_PATH, "dat_long_original.qs")
  if (file.exists(original_data_path)) {
    dat_long_original <- qs::qread(original_data_path)
  } else {
    cat("Warning: Original data not found. Will reconstruct from imputations.\n")
    dat_long_original <- NULL
  }
  
} else {
  # ========================================================================
  # LOAD OR CREATE SYNTHETIC DATA
  # ========================================================================
  cat("Using synthetic demonstration data...\n")
  
  synthetic_path <- file.path(DATA_PATH, SYNTHETIC_DATA_FILE)
  
  if (!file.exists(synthetic_path)) {
    cat("Synthetic data not found. Generating...\n")
    source("data/synthetic/generate_synthetic_data.R")
    
    # generate synthetic data
    synthetic_data <- generate_synthetic_trust_data(
      n_participants = N_PARTICIPANTS,
      n_waves = N_WAVES,
      baseline_year = BASELINE_YEAR,
      seed = SEED
    )
    
    # save for future use
    write.csv(synthetic_data, synthetic_path, row.names = FALSE)
    cat("Synthetic data saved to:", synthetic_path, "\n")
  } else {
    # load existing synthetic data
    synthetic_data <- read.csv(synthetic_path)
    cat("Loaded existing synthetic data from:", synthetic_path, "\n")
  }
  
  # create amelia objects from synthetic data
  cat("\nCreating multiple imputations from synthetic data...\n")
  
  # prepare data for amelia
  dat_for_amelia <- synthetic_data
  
  # convert wave to character (Amelia requirement)
  dat_for_amelia$wave <- as.character(dat_for_amelia$wave)
  
  # no need to remove factor variables - they're not created in synthetic data anymore
  
  # specify variables
  # id variables (removed from imputation model)
  # note: ID_VAR (id) is specified separately as cs parameter, so don't include here
  id_vars <- c("wave", "age_baseline")
  
  # nominal variables (categorical to be imputed)
  nominal_vars <- c("gender", "ethnicity")
  
  # note: trust variables are continuous, not ordinal
  # education could be ordinal but we'll let it be continuous for simplicity
  
  # run amelia (bounds for continuous variables)
  cat("Running Amelia with bounds for trust variables...\n")
  # bounds for trust variables only (1-7 scale)
  bounds_matrix <- matrix(c(which(names(dat_for_amelia) == TRUST_SCIENCE_VAR), 1, 7,
                           which(names(dat_for_amelia) == TRUST_SCIENTISTS_VAR), 1, 7), 
                         nrow = 2, ncol = 3, byrow = TRUE)
  
  amelia_bounds <- Amelia::amelia(
    dat_for_amelia,
    m = N_IMPUTATIONS,
    idvars = id_vars,
    noms = nominal_vars,
    bounds = bounds_matrix,
    empri = 0.01 * nrow(dat_for_amelia),
    ts = "years",
    cs = "id",
    p2s = 0
  )
  
  # log transformation not appropriate for trust variables on 1-7 scale
  # removed amelia_log computation
  
  # use synthetic data as "original" for mice conversion
  dat_long_original <- synthetic_data
}

# ========================================================================
# CONVERT TO MICE FORMAT
# ========================================================================
cat("\n=== CONVERTING TO MICE FORMAT ===\n")

# convert bounds version
cat("\nConverting Amelia bounds object to MICE...\n")
mids_bounds <- margot_amelia_to_mice_fixed(
  amelia_bounds, 
  original_data = dat_long_original,
  verbose = TRUE
)

# log version not needed - removed

# choose which version to use for main analysis
cat("\nUsing bounds version for main analysis (more stable for ordinal models)\n")
mids_obj <- mids_bounds

# ========================================================================
# CREATE FACTOR VARIABLES
# ========================================================================
cat("\n=== CREATING FACTOR VARIABLES ===\n")

# function to create factors using cut
create_trust_factor <- function(trust_values) {
  cut(trust_values,
      breaks = c(0, 3.5, 5.5, 8),
      labels = c("low", "med", "high"),
      include.lowest = TRUE,
      ordered = TRUE)
}

# add factor variables to the mids object
cat("Adding factor variables to mids object...\n")

# add to original data
mids_obj$data[[TRUST_SCIENCE_FACTOR]] <- create_trust_factor(mids_obj$data[[TRUST_SCIENCE_VAR]])
mids_obj$data[[TRUST_SCIENTISTS_FACTOR]] <- create_trust_factor(mids_obj$data[[TRUST_SCIENTISTS_VAR]])

# add empty imp slots for new variables (they have no missing values in original data)
mids_obj$imp[[TRUST_SCIENCE_FACTOR]] <- matrix(NA, nrow = 0, ncol = mids_obj$m)
mids_obj$imp[[TRUST_SCIENTISTS_FACTOR]] <- matrix(NA, nrow = 0, ncol = mids_obj$m)

# update nmis
mids_obj$nmis[TRUST_SCIENCE_FACTOR] <- 0
mids_obj$nmis[TRUST_SCIENTISTS_FACTOR] <- 0

# update where matrix
mids_obj$where <- cbind(mids_obj$where, 
                       matrix(FALSE, nrow = nrow(mids_obj$data), ncol = 2,
                             dimnames = list(NULL, c(TRUST_SCIENCE_FACTOR, TRUST_SCIENTISTS_FACTOR))))

cat("  ✓ Factor variables created successfully\n")

# ========================================================================
# PREPARE OBSERVED DATA
# ========================================================================
cat("\n=== PREPARING OBSERVED (COMPLETE CASE) DATA ===\n")

# extract complete cases from first imputation
dat_observed <- mice::complete(mids_obj, 1)

# identify complete cases
# include education in complete case analysis
complete_vars <- c(TRUST_SCIENCE_VAR, TRUST_SCIENTISTS_VAR, TIME_VAR, ID_VAR, WEIGHT_VAR, "education")
complete_rows <- complete.cases(dat_observed[, complete_vars])

dat_observed <- dat_observed[complete_rows, ]

cat("Complete cases: ", nrow(dat_observed), " observations\n", sep = "")
cat("Complete cases by wave:\n")
print(table(dat_observed[[TIME_VAR]]))

# ========================================================================
# SAVE PROCESSED DATA
# ========================================================================
cat("\n=== SAVING PROCESSED DATA ===\n")

# save mice objects
saveRDS(mids_obj, "data/processed/mids_obj.rds")
saveRDS(mids_bounds, "data/processed/mids_bounds.rds")
# mids_log not saved - log transformation not used
cat("Saved MICE objects\n")

# save observed data
saveRDS(dat_observed, "data/processed/dat_observed.rds")
cat("Saved observed data\n")

# save data summary
data_summary <- list(
  n_total = nrow(mids_obj$data),
  n_observed = nrow(dat_observed),
  n_imputations = mids_obj$m,
  variables = names(mids_obj$data),
  missing_by_var = mids_obj$nmis,
  waves = sort(unique(mids_obj$data[[TIME_VAR]])),
  data_source = ifelse(USE_REAL_DATA, "NZAVS", "Synthetic")
)

saveRDS(data_summary, "data/processed/data_summary.rds")

# print summary
cat("\n==========================================================================\n")
cat("DATA LOADING COMPLETE\n")
cat("==========================================================================\n")
cat("Total observations:", data_summary$n_total, "\n")
cat("Complete cases:", data_summary$n_observed, "\n")
cat("Imputations:", data_summary$n_imputations, "\n")
cat("Data source:", data_summary$data_source, "\n")
cat("\nProcessed data saved to: data/processed/\n")
cat("\nNext step: Run 02_fit_models.R\n")