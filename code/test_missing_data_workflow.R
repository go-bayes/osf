# Test Missing Data Workflow with Small Dataset
# This validates the Amelia -> MICE conversion process
# joseph.bulbulia@gmail.com

cat("==========================================================================\n")
cat("TESTING MISSING DATA WORKFLOW\n")
cat("==========================================================================\n\n")

# load required packages
library(Amelia)
library(mice)
library(tidyverse)

# set seed for reproducibility
set.seed(123)

# ========================================================================
# STEP 1: CREATE SMALL TEST DATASET
# ========================================================================
cat("Creating small test dataset...\n")

# parameters
n_participants <- 100
n_waves <- 4

# create participants with time-invariant characteristics
participants <- data.frame(
  id = 1:n_participants,
  gender = sample(c("Male", "Female"), n_participants, replace = TRUE),
  ethnicity = sample(c("European", "Maori", "Asian"), n_participants, replace = TRUE),
  age_baseline = round(runif(n_participants, 20, 70))
)

# create long format data
long_data <- expand.grid(
  id = 1:n_participants,
  wave = 0:(n_waves - 1),
  stringsAsFactors = FALSE
)

# merge with participant data
long_data <- merge(long_data, participants, by = "id")

# add time variables
long_data$year <- 2019 + long_data$wave

# generate complete outcome data first
long_data$trust_science_complete <- round(runif(nrow(long_data), 1, 7), 1)
long_data$trust_scientists_complete <- round(runif(nrow(long_data), 1, 7), 1)

# create realistic dropout pattern
# probability of dropout increases over time
dropout_prob <- c(0, 0.1, 0.25, 0.4)  # by wave

# create dropout indicator for each participant
participant_dropout <- data.frame(id = 1:n_participants)
for (w in 1:(n_waves-1)) {
  # if already dropped out, stay dropped out
  if (w == 1) {
    participant_dropout[[paste0("dropout_wave_", w)]] <- 
      rbinom(n_participants, 1, dropout_prob[w+1])
  } else {
    prev_dropout <- participant_dropout[[paste0("dropout_wave_", w-1)]]
    new_dropout <- rbinom(n_participants, 1, dropout_prob[w+1])
    participant_dropout[[paste0("dropout_wave_", w)]] <- 
      pmax(prev_dropout, new_dropout)
  }
}

# apply dropout to create missing data
long_data$missing <- FALSE
for (w in 1:(n_waves-1)) {
  wave_rows <- long_data$wave == w
  dropout_col <- paste0("dropout_wave_", w)
  dropout_ids <- participant_dropout$id[participant_dropout[[dropout_col]] == 1]
  long_data$missing[wave_rows & long_data$id %in% dropout_ids] <- TRUE
}

# create observed data with missingness
long_data$trust_science <- long_data$trust_science_complete
long_data$trust_scientists <- long_data$trust_scientists_complete
long_data$trust_science[long_data$missing] <- NA
long_data$trust_scientists[long_data$missing] <- NA

# remove complete versions and missing indicator
long_data$trust_science_complete <- NULL
long_data$trust_scientists_complete <- NULL
long_data$missing <- NULL

# sort by id and wave
long_data <- long_data[order(long_data$id, long_data$wave), ]

# print summary
cat("\nDataset created:\n")
cat("  Participants:", n_participants, "\n")
cat("  Waves:", n_waves, "\n")
cat("  Total observations:", nrow(long_data), "\n")

# check missing data pattern
missing_by_wave <- aggregate(
  is.na(long_data$trust_science),
  by = list(wave = long_data$wave),
  FUN = mean
)
names(missing_by_wave)[2] <- "prop_missing"
cat("\nMissing data by wave:\n")
print(missing_by_wave)

# ========================================================================
# STEP 2: RUN AMELIA IMPUTATION
# ========================================================================
cat("\n=== RUNNING AMELIA IMPUTATION ===\n")

# prepare for amelia
# time-invariant variables should be constant within id
# for amelia, we need to specify the panel structure

cat("Configuring Amelia for panel data...\n")

# run amelia with panel structure
amelia_result <- amelia(
  long_data,
  m = 5,  # just 5 imputations for testing
  ts = "wave",  # time variable
  cs = "id",    # cross-section ID
  noms = c("gender", "ethnicity"),  # nominal variables
  idvars = c("year"),  # variables not to use in imputation
  empri = 0.01 * nrow(long_data),  # empirical prior
  p2s = 1  # print to screen
)

cat("\nAmelia imputation complete!\n")

# check imputed values exist
imp1 <- amelia_result$imputations[[1]]
cat("\nChecking first imputation:\n")
cat("  Missing in original:", sum(is.na(long_data$trust_science)), "\n")
cat("  Missing in imputed:", sum(is.na(imp1$trust_science)), "\n")

# ========================================================================
# STEP 3: CONVERT TO MICE FORMAT
# ========================================================================
cat("\n=== CONVERTING TO MICE FORMAT ===\n")

# source the conversion function
# first check if we have here package, otherwise use relative path
if (requireNamespace("here", quietly = TRUE)) {
  source(("code/functions/margot_amelia_to_mice_fixed.R"))
} else {
  source("functions/margot_amelia_to_mice_fixed.R")
}

# convert to mice
mids_test <- margot_amelia_to_mice_fixed(
  amelia_result,
  original_data = long_data,
  verbose = TRUE
)

# ========================================================================
# STEP 4: VALIDATE MICE OBJECT
# ========================================================================
cat("\n=== VALIDATING MICE OBJECT ===\n")

# check class
cat("MICE object class:", class(mids_test), "\n")

# complete first imputation
complete1 <- mice::complete(mids_test, 1)
cat("\nFirst imputation summary:\n")
cat("  Rows:", nrow(complete1), "\n")
cat("  Missing trust_science:", sum(is.na(complete1$trust_science)), "\n")
cat("  Missing trust_scientists:", sum(is.na(complete1$trust_scientists)), "\n")

# test factor creation
complete1$trust_science_factor <- cut(
  complete1$trust_science,
  breaks = c(0, 3.5, 5.5, 8),
  labels = c("low", "med", "high"),
  include.lowest = TRUE
)

cat("\nFactor creation test:\n")
print(table(complete1$trust_science_factor, useNA = "always"))

# ========================================================================
# STEP 5: COMPARE PATTERNS
# ========================================================================
cat("\n=== COMPARING MISSING DATA PATTERNS ===\n")

# original pattern
orig_pattern <- aggregate(
  cbind(
    trust_science_miss = is.na(long_data$trust_science),
    trust_scientists_miss = is.na(long_data$trust_scientists)
  ),
  by = list(wave = long_data$wave),
  FUN = mean
)

cat("\nOriginal missing proportions by wave:\n")
print(orig_pattern)

# check if imputation worked
if (sum(is.na(complete1$trust_science)) == 0) {
  cat("\n✓ SUCCESS: Imputation filled all missing values!\n")
} else {
  cat("\n✗ ERROR: Imputation did not fill missing values\n")
  
  # debug info
  cat("\nDEBUG INFO:\n")
  cat("Original data dimensions:", dim(long_data), "\n")
  cat("Imputed data dimensions:", dim(imp1), "\n")
  cat("MICE data dimensions:", dim(mids_test$data), "\n")
  
  # check a few rows
  cat("\nSample of data (first 5 rows with missing):\n")
  missing_rows <- which(is.na(long_data$trust_science))[1:5]
  cat("Original:\n")
  print(long_data[missing_rows, c("id", "wave", "trust_science")])
  cat("\nAfter Amelia:\n")
  print(imp1[missing_rows, c("id", "wave", "trust_science")])
  cat("\nAfter MICE complete:\n")
  print(complete1[missing_rows, c("id", "wave", "trust_science")])
}

cat("\n==========================================================================\n")
cat("TEST COMPLETE\n")
cat("==========================================================================\n")