# Fit Statistical Models for Trust in Science Analysis
# This script fits all models for both observed and imputed data
# joseph.bulbulia@gmail.com

# setup
cat("==========================================================================\n")
cat("STEP 2: FIT STATISTICAL MODELS\n")
cat("==========================================================================\n\n")

# load required packages
library(tidyverse)
library(here)
library(mice)
library(geepack)
library(MASS)
library(ggeffects)
library(splines)
library(sandwich)

# source configuration
source("code/config/config.R")

# load processed data
cat("Loading processed data...\n")
mids_obj <- readRDS("data/processed/mids_obj.rds")
dat_observed <- readRDS("data/processed/dat_observed.rds")
data_summary <- readRDS("data/processed/data_summary.rds")

cat("Data loaded successfully\n")
cat("- Total observations:", data_summary$n_total, "\n")
cat("- Complete cases:", data_summary$n_observed, "\n")
cat("- Imputations:", data_summary$n_imputations, "\n\n")

# ========================================================================
# FIT MODELS FOR OBSERVED DATA
# ========================================================================
cat("=== FITTING MODELS FOR OBSERVED DATA ===\n\n")

# continuous outcomes - gee models
cat("Fitting GEE models for continuous outcomes (observed data)...\n")

# trust in science
gee_science_observed <- geepack::geeglm(
  trust_science ~ ns(years, 3) + education,
  id = id,
  weights = weights,
  data = dat_observed,
  corstr = "exchangeable"
)

# trust in scientists  
gee_scientists_observed <- geepack::geeglm(
  trust_scientists ~ ns(years, 3) + education,
  id = id,
  weights = weights,
  data = dat_observed,
  corstr = "exchangeable"
)

cat("  ✓ GEE models fitted\n")

# categorical outcomes - proportional odds models
cat("\nFitting proportional odds models for categorical outcomes (observed data)...\n")

# trust in science factor
polr_science_observed <- MASS::polr(
  trust_science_factor ~ ns(years, 3) + education,
  weights = weights,
  data = dat_observed,
  Hess = TRUE
)

# trust in scientists factor
polr_scientists_observed <- MASS::polr(
  trust_scientists_factor ~ ns(years, 3) + education,
  weights = weights,
  data = dat_observed,
  Hess = TRUE
)

cat("  ✓ Proportional odds models fitted\n")

# generate predictions for observed data
cat("\nGenerating predictions for observed data...\n")

# gee predictions
gee_predictions_science_observed <- ggeffects::predict_response(
  gee_science_observed, 
  "years[all]",
  margin = "marginalmeans",
  vcov_type = "robust"
)

gee_predictions_scientists_observed <- ggeffects::predict_response(
  gee_scientists_observed,
  "years[all]", 
  margin = "marginalmeans",
  vcov_type = "robust"
)

# polr predictions (with cluster-robust ses)
polr_predictions_science_observed <- ggeffects::predict_response(
  polr_science_observed,
  "years[all]",
  margin = "marginalmeans",
  vcov_fun = function(x) sandwich::vcovCL(x, cluster = dat_observed$id)
)

polr_predictions_scientists_observed <- ggeffects::predict_response(
  polr_scientists_observed,
  "years[all]",
  margin = "marginalmeans", 
  vcov_fun = function(x) sandwich::vcovCL(x, cluster = dat_observed$id)
)

cat("  ✓ Predictions generated\n")

# ========================================================================
# FIT MODELS FOR IMPUTED DATA
# ========================================================================
cat("\n=== FITTING MODELS FOR IMPUTED DATA ===\n\n")

# continuous outcomes - gee models
cat("Fitting GEE models across imputations...\n")
cat("  Processing", N_IMPUTATIONS, "imputations...\n")

# trust in science - fit to each imputation
gee_science_imputed <- lapply(1:N_IMPUTATIONS, function(i) {
  if (i == 1) cat("    Imputation", i)
  else if (i %% 5 == 0) cat("...", i)
  
  m <- geepack::geeglm(
    trust_science ~ ns(years, 3) + education,
    id = id,
    weights = weights,
    data = mice::complete(mids_obj, action = i),
    corstr = "exchangeable"
  )
  
  ggeffects::predict_response(
    m, 
    "years[all]",
    margin = "marginalmeans",
    vcov_type = "robust"
  )
})
cat("\n  ✓ Trust in science GEE models fitted\n")

# trust in scientists
gee_scientists_imputed <- lapply(1:N_IMPUTATIONS, function(i) {
  if (i == 1) cat("    Imputation", i)
  else if (i %% 5 == 0) cat("...", i)
  
  m <- geepack::geeglm(
    trust_scientists ~ ns(years, 3) + education,
    id = id,
    weights = weights,
    data = mice::complete(mids_obj, action = i),
    corstr = "exchangeable"
  )
  
  ggeffects::predict_response(
    m,
    "years[all]",
    margin = "marginalmeans",
    vcov_type = "robust"
  )
})
cat("\n  ✓ Trust in scientists GEE models fitted\n")

# pool gee predictions
cat("\nPooling GEE predictions across imputations...\n")
gee_predictions_science_imputed <- ggeffects::pool_predictions(gee_science_imputed)
gee_predictions_scientists_imputed <- ggeffects::pool_predictions(gee_scientists_imputed)
cat("  ✓ GEE predictions pooled\n")

# categorical outcomes - proportional odds models
cat("\nFitting proportional odds models across imputations...\n")

# trust in science factor
polr_science_imputed <- lapply(1:N_IMPUTATIONS, function(i) {
  if (i == 1) cat("    Imputation", i)
  else if (i %% 5 == 0) cat("...", i)
  
  dat_i <- mice::complete(mids_obj, action = i)
  
  m <- MASS::polr(
    trust_science_factor ~ ns(years, 3) + education,
    weights = weights,
    data = dat_i,
    Hess = TRUE
  )
  
  ggeffects::predict_response(
    m,
    "years[all]",
    margin = "marginalmeans",
    vcov_fun = function(x) sandwich::vcovCL(x, cluster = dat_i$id)
  )
})
cat("\n  ✓ Trust in science proportional odds models fitted\n")

# trust in scientists factor  
polr_scientists_imputed <- lapply(1:N_IMPUTATIONS, function(i) {
  if (i == 1) cat("    Imputation", i)
  else if (i %% 5 == 0) cat("...", i)
  
  dat_i <- mice::complete(mids_obj, action = i)
  
  m <- MASS::polr(
    trust_scientists_factor ~ ns(years, 3) + education,
    weights = weights,
    data = dat_i,
    Hess = TRUE
  )
  
  ggeffects::predict_response(
    m,
    "years[all]",
    margin = "marginalmeans",
    vcov_fun = function(x) sandwich::vcovCL(x, cluster = dat_i$id)
  )
})
cat("\n  ✓ Trust in scientists proportional odds models fitted\n")

# pool polr predictions
cat("\nPooling proportional odds predictions across imputations...\n")
polr_predictions_science_imputed <- ggeffects::pool_predictions(polr_science_imputed)
polr_predictions_scientists_imputed <- ggeffects::pool_predictions(polr_scientists_imputed)
cat("  ✓ Proportional odds predictions pooled\n")

# ========================================================================
# SAVE MODEL OUTPUTS
# ========================================================================
cat("\n=== SAVING MODEL OUTPUTS ===\n")

# create results directory if needed
if (!dir.exists("results/model_outputs")) {
  dir.create("results/model_outputs", recursive = TRUE)
}

# save observed models
model_outputs_observed <- list(
  gee_science = gee_science_observed,
  gee_scientists = gee_scientists_observed,
  polr_science = polr_science_observed,
  polr_scientists = polr_scientists_observed
)
saveRDS(model_outputs_observed, "results/model_outputs/models_observed.rds")

# save predictions
predictions_all <- list(
  observed = list(
    gee_science = gee_predictions_science_observed,
    gee_scientists = gee_predictions_scientists_observed,
    polr_science = polr_predictions_science_observed,
    polr_scientists = polr_predictions_scientists_observed
  ),
  imputed = list(
    gee_science = gee_predictions_science_imputed,
    gee_scientists = gee_predictions_scientists_imputed,
    polr_science = polr_predictions_science_imputed,
    polr_scientists = polr_predictions_scientists_imputed
  )
)
saveRDS(predictions_all, "results/model_outputs/predictions_all.rds")

# save model summaries
cat("\nGenerating model summaries...\n")

# function to safely extract model info
safe_summary <- function(model, name) {
  tryCatch({
    list(
      name = name,
      coefficients = coef(model),
      n_obs = nobs(model),
      class = class(model)[1]
    )
  }, error = function(e) {
    list(name = name, error = as.character(e))
  })
}

model_summaries <- list(
  observed = list(
    gee_science = safe_summary(gee_science_observed, "GEE Science Observed"),
    gee_scientists = safe_summary(gee_scientists_observed, "GEE Scientists Observed"),
    polr_science = safe_summary(polr_science_observed, "POLR Science Observed"),
    polr_scientists = safe_summary(polr_scientists_observed, "POLR Scientists Observed")
  ),
  data_info = data_summary
)

saveRDS(model_summaries, "results/model_outputs/model_summaries.rds")

cat("  ✓ Model outputs saved\n")

# ========================================================================
# PRINT SUMMARY
# ========================================================================
cat("\n==========================================================================\n")
cat("MODEL FITTING COMPLETE\n") 
cat("==========================================================================\n")

# print key estimates at baseline and final year
cat("\nKey estimates (marginal means):\n")
cat("\nTrust in Science (1-7 scale):\n")
cat("  Observed data:\n")
cat("    Year 0:", round(gee_predictions_science_observed$predicted[1], 3), "\n")
cat("    Year 3:", round(gee_predictions_science_observed$predicted[4], 3), "\n")
cat("  Imputed data:\n")
cat("    Year 0:", round(gee_predictions_science_imputed$predicted[1], 3), "\n")
cat("    Year 3:", round(gee_predictions_science_imputed$predicted[4], 3), "\n")

cat("\nTrust in Scientists (1-7 scale):\n")
cat("  Observed data:\n")
cat("    Year 0:", round(gee_predictions_scientists_observed$predicted[1], 3), "\n")
cat("    Year 3:", round(gee_predictions_scientists_observed$predicted[4], 3), "\n")
cat("  Imputed data:\n")
cat("    Year 0:", round(gee_predictions_scientists_imputed$predicted[1], 3), "\n")
cat("    Year 3:", round(gee_predictions_scientists_imputed$predicted[4], 3), "\n")

cat("\nModel outputs saved to: results/model_outputs/\n")
cat("\nNext step: Run 03_create_visualizations.R\n")