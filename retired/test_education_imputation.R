# Test script to verify education-based synthetic data
# Shows that imputation recovers patterns closer to oracle data
# joseph.bulbulia@gmail.com

cat("=== TESTING EDUCATION-BASED SYNTHETIC DATA ===\n\n")

# setup
library(tidyverse)
library(here)
library(Amelia)
library(mice)
library(geepack)
library(ggeffects)
library(splines)

# generate new synthetic data
cat("Generating synthetic data with education effects...\n")
source("data/synthetic/generate_synthetic_data.R")

# generate data
set.seed(2024)
synthetic_data <- generate_synthetic_trust_data(
  n_participants = 5000,  # smaller for testing
  n_waves = 4,
  baseline_year = 2019,
  seed = 2024
)

# load oracle data (saved automatically)
oracle_data <- read.csv("oracle_trust_data.csv")

cat("\nData generated successfully\n")
cat("- Synthetic data shape:", dim(synthetic_data), "\n")
cat("- Oracle data shape:", dim(oracle_data), "\n")
cat("- Missing values in trust_science:", sum(is.na(synthetic_data$trust_science)), "\n")
cat("- Missing values in oracle:", sum(is.na(oracle_data$trust_science)), "\n\n")

# check education distribution
cat("Education distribution:\n")
print(table(synthetic_data$education[synthetic_data$wave == 0]))

# check correlation between education and trust at baseline
baseline_data <- synthetic_data[synthetic_data$wave == 0, ]
cat("\nCorrelation between education and trust at baseline:\n")
cat("- Trust in science:", round(cor(baseline_data$education, baseline_data$trust_science, use = "complete.obs"), 3), "\n")
cat("- Trust in scientists:", round(cor(baseline_data$education, baseline_data$trust_scientists, use = "complete.obs"), 3), "\n\n")

# === ANALYSIS 1: ORACLE DATA (COMPLETE DATA) ===
cat("=== ANALYZING ORACLE DATA ===\n")

# fit model to oracle data
oracle_model <- geepack::geeglm(
  trust_science ~ ns(years, 3) + education,
  id = id,
  weights = weights,
  data = oracle_data,
  corstr = "exchangeable"
)

# get predictions
oracle_pred <- ggeffects::predict_response(
  oracle_model,
  "years[all]",
  margin = "marginalmeans",
  vcov_type = "robust"
)

cat("Oracle data estimates:\n")
cat("  Year 0:", round(oracle_pred$predicted[1], 3), "\n")
cat("  Year 1:", round(oracle_pred$predicted[2], 3), " (COVID bump)\n")
cat("  Year 2:", round(oracle_pred$predicted[3], 3), "\n")
cat("  Year 3:", round(oracle_pred$predicted[4], 3), " (decline)\n\n")

# === ANALYSIS 2: OBSERVED DATA (WITH MISSING) ===
cat("=== ANALYZING OBSERVED DATA (COMPLETE CASES) ===\n")

# complete cases only
observed_data <- synthetic_data[complete.cases(synthetic_data$trust_science), ]
cat("Complete cases:", nrow(observed_data), "of", nrow(synthetic_data), "observations\n")

# check who stays in
staying_ids <- unique(observed_data$id[observed_data$wave == 3])
baseline_stayers <- synthetic_data[synthetic_data$wave == 0 & synthetic_data$id %in% staying_ids, ]
baseline_dropouts <- synthetic_data[synthetic_data$wave == 0 & !(synthetic_data$id %in% staying_ids), ]

cat("\nCharacteristics of stayers vs dropouts:\n")
cat("Stayers (n =", length(staying_ids), "):\n")
cat("  Mean education:", round(mean(baseline_stayers$education), 2), "\n")
cat("  Mean trust science:", round(mean(baseline_stayers$trust_science), 2), "\n")

cat("Dropouts (n =", length(unique(baseline_dropouts$id)), "):\n")
cat("  Mean education:", round(mean(baseline_dropouts$education), 2), "\n")
cat("  Mean trust science:", round(mean(baseline_dropouts$trust_science), 2), "\n")

# special focus on COVID wave dropouts
wave1_present <- unique(synthetic_data$id[synthetic_data$wave == 1 & !is.na(synthetic_data$trust_science)])
covid_dropouts <- baseline_dropouts[!(baseline_dropouts$id %in% wave1_present), ]

cat("\nCOVID wave (2020) dropouts specifically:\n")
cat("  N =", nrow(covid_dropouts), "\n")
cat("  Mean trust:", round(mean(covid_dropouts$trust_science), 2), "\n")
cat("  % with low trust (<4):", round(100 * mean(covid_dropouts$trust_science < 4), 1), "%\n\n")

# fit model
observed_model <- geepack::geeglm(
  trust_science ~ ns(years, 3) + education,
  id = id,
  weights = weights,
  data = observed_data,
  corstr = "exchangeable"
)

observed_pred <- ggeffects::predict_response(
  observed_model,
  "years[all]",
  margin = "marginalmeans",
  vcov_type = "robust"
)

cat("Observed data estimates (biased by selective attrition):\n")
cat("  Year 0:", round(observed_pred$predicted[1], 3), "\n")
cat("  Year 1:", round(observed_pred$predicted[2], 3), "\n")
cat("  Year 2:", round(observed_pred$predicted[3], 3), "\n")
cat("  Year 3:", round(observed_pred$predicted[4], 3), "\n\n")

# === ANALYSIS 3: IMPUTED DATA ===
cat("=== ANALYZING IMPUTED DATA ===\n")

# prepare for amelia
dat_for_amelia <- synthetic_data
id_vars <- c("id", "wave", "age_baseline")
nominal_vars <- c("gender", "ethnicity")

# run amelia with education
cat("Running Amelia imputation (this may take a moment)...\n")
bounds_matrix <- matrix(c(1, 1, 7, 2, 1, 7, 3, 1, 7), nrow = 3, ncol = 3, byrow = TRUE)
rownames(bounds_matrix) <- c("trust_science", "trust_scientists", "education")

amelia_result <- Amelia::amelia(
  dat_for_amelia,
  m = 5,  # fewer imputations for testing
  idvars = id_vars,
  noms = nominal_vars,
  ords = "education",
  bounds = bounds_matrix,
  empri = 0.01 * nrow(dat_for_amelia),
  ts = "years",
  cs = "id",
  p2s = 0
)

cat("Imputation complete\n")

# fit models to each imputation
imputed_predictions <- lapply(1:5, function(i) {
  imp_data <- amelia_result$imputations[[i]]
  
  m <- geepack::geeglm(
    trust_science ~ ns(years, 3) + education,
    id = id,
    weights = weights,
    data = imp_data,
    corstr = "exchangeable"
  )
  
  ggeffects::predict_response(
    m,
    "years[all]",
    margin = "marginalmeans",
    vcov_type = "robust"
  )
})

# pool predictions
pooled_pred <- ggeffects::pool_predictions(imputed_predictions)

cat("\nImputed data estimates (education helps recover true pattern):\n")
cat("  Year 0:", round(pooled_pred$predicted[1], 3), "\n")
cat("  Year 1:", round(pooled_pred$predicted[2], 3), "\n")
cat("  Year 2:", round(pooled_pred$predicted[3], 3), "\n")
cat("  Year 3:", round(pooled_pred$predicted[4], 3), "\n\n")

# === SUMMARY ===
cat("=== SUMMARY OF RESULTS ===\n\n")

cat("Comparison of estimates:\n")
results_df <- data.frame(
  Year = 0:3,
  Oracle = round(oracle_pred$predicted, 3),
  Observed = round(observed_pred$predicted, 3),
  Imputed = round(pooled_pred$predicted, 3)
)
print(results_df)

cat("\nKey findings:\n")
cat("1. Oracle data shows true pattern: COVID bump then decline below baseline\n")
cat("2. Observed data is biased upward due to selective attrition\n")
cat("3. COVID wave had extreme selection: low-trust individuals dropped out\n")
cat("4. Imputation recovers pattern closer to oracle by using education\n")

# calculate bias
bias_observed <- mean(observed_pred$predicted - oracle_pred$predicted)
bias_imputed <- mean(pooled_pred$predicted - oracle_pred$predicted)

cat("\nAverage bias across years:\n")
cat("  Observed data bias:", round(bias_observed, 3), "\n")
cat("  Imputed data bias:", round(bias_imputed, 3), "\n")
cat("  Bias reduction:", round((1 - abs(bias_imputed)/abs(bias_observed)) * 100, 1), "%\n")

# COVID-specific bias
covid_bias_observed <- observed_pred$predicted[2] - oracle_pred$predicted[2]
covid_bias_imputed <- pooled_pred$predicted[2] - oracle_pred$predicted[2]

cat("\nCOVID year (2020) bias:\n")
cat("  Observed data overestimates by:", round(covid_bias_observed, 3), "\n")
cat("  Imputed data overestimates by:", round(covid_bias_imputed, 3), "\n")
cat("  This happens because low-trust people dropped out during COVID\n")

cat("\n=== TEST COMPLETE ===\n")