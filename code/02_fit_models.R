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
  trust_science ~ ns(years, 3),
  id = id,
  weights = weights,
  data = dat_observed,
  corstr = "exchangeable"
)

# trust in scientists
gee_scientists_observed <- geepack::geeglm(
  trust_scientists ~ ns(years, 3),
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
  trust_science_factor ~ ns(years, 3),
  weights = weights,
  data = dat_observed,
  Hess = TRUE
)

# trust in scientists factor
polr_scientists_observed <- MASS::polr(
  trust_scientists_factor ~ ns(years, 3),
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

  dat_imp <- mice::complete(mids_obj, action = i) %>%
    arrange(id, years)  # ensure proper sorting for GEE

  m <- geepack::geeglm(
    trust_science ~ ns(years, 3),
    id = id,
    weights = weights,
    data = dat_imp,
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

  dat_imp <- mice::complete(mids_obj, action = i) %>%
    arrange(id, years)  # ensure proper sorting for GEE

  m <- geepack::geeglm(
    trust_scientists ~ ns(years, 3),
    id = id,
    weights = weights,
    data = dat_imp,
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
    trust_science_factor ~ ns(years, 3),
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
    trust_scientists_factor ~ ns(years, 3),
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
cat("    Year 4:", round(gee_predictions_science_observed$predicted[5], 3), "\n")
cat("  Imputed data:\n")
cat("    Year 0:", round(gee_predictions_science_imputed$predicted[1], 3), "\n")
cat("    Year 4:", round(gee_predictions_science_imputed$predicted[5], 3), "\n")

cat("\nTrust in Scientists (1-7 scale):\n")
cat("  Observed data:\n")
cat("    Year 0:", round(gee_predictions_scientists_observed$predicted[1], 3), "\n")
cat("    Year 4:", round(gee_predictions_scientists_observed$predicted[5], 3), "\n")
cat("  Imputed data:\n")
cat("    Year 0:", round(gee_predictions_scientists_imputed$predicted[1], 3), "\n")
cat("    Year 4:", round(gee_predictions_scientists_imputed$predicted[5], 3), "\n")

cat("\nModel outputs saved to: results/model_outputs/\n")

# ========================================================================
# ORACLE DATA ANALYSIS (COMPLETE DATA WITHOUT MISSINGNESS)
# ========================================================================
cat("\n=== ANALYZING ORACLE DATA (GROUND TRUTH) ===\n")

# load oracle data
oracle_path_rds <- here::here("data", "synthetic", "oracle_trust_data.rds")
oracle_path_csv <- here::here("data", "synthetic", "oracle_trust_data.csv")
if (file.exists(oracle_path_rds)) {
  dat_oracle <- readRDS(oracle_path_rds)
} else if (file.exists(oracle_path_csv)) {
  dat_oracle <- read.csv(oracle_path_csv)

  # add factor variables and ensure sorting
  dat_oracle <- dat_oracle %>%
    mutate(
      trust_science_factor = case_when(
        trust_science <= 3 ~ "low",
        trust_science < 6 ~ "med",
        trust_science >= 6 ~ "high"
      ) %>%
        factor(levels = c("low", "med", "high"), ordered = TRUE),
      trust_scientists_factor = case_when(
        trust_scientists <= 3 ~ "low",
        trust_scientists < 6 ~ "med",
        trust_scientists >= 6 ~ "high"
      ) %>%
        factor(levels = c("low", "med", "high"), ordered = TRUE)
    ) %>%
    arrange(id, years)  # ensure proper sorting for GEE models

  cat("Oracle data loaded:", nrow(dat_oracle), "observations\n")

  # fit GEE models to oracle data
  cat("\nFitting GEE models to oracle data...\n")

  gee_science_oracle <- geepack::geeglm(
    trust_science ~ ns(years, 3),
    id = id,
    weights = weights,
    data = dat_oracle,
    corstr = "exchangeable"
  )

  gee_scientists_oracle <- geepack::geeglm(
    trust_scientists ~ ns(years, 3),
    id = id,
    weights = weights,
    data = dat_oracle,
    corstr = "exchangeable"
  )

  # generate predictions
  gee_predictions_science_oracle <- ggeffects::predict_response(
    gee_science_oracle,
    "years[all]",
    margin = "marginalmeans",
    vcov_type = "robust"
  )

  gee_predictions_scientists_oracle <- ggeffects::predict_response(
    gee_scientists_oracle,
    "years[all]",
    margin = "marginalmeans",
    vcov_type = "robust"
  )

  # create plots with specified settings
  cat("Creating oracle data plots...\n")

  oracle_science_plot <- plot(
    gee_predictions_science_oracle,
    show_data = TRUE,
    ci_style = "dash",
    colors = "viridis",
    jitter = .5,
    dot_alpha = .01,
    dot_size = 2,
    limits = c(4.5, 5.5)
  ) +
    geom_point(
      aes(x = x, y = predicted),
      color = "darkgreen",
      size = 2,
      alpha = 1
    ) +
    labs(
      x = "Years: 2019-2023",
      y = "Trust in Science (1-7)",
      title = "Trust in Science - Oracle Data (Ground Truth)"
    ) +
    theme_bw()

  oracle_scientists_plot <- plot(
    gee_predictions_scientists_oracle,
    show_data = TRUE,
    ci_style = "dash",
    colors = "viridis",
    jitter = .5,
    dot_alpha = .01,
    dot_size = 2,
    limits = c(4.5, 5.5)
  ) +
    geom_point(
      aes(x = x, y = predicted),
      color = "darkgreen",
      size = 2,
      alpha = 1
    ) +
    labs(
      x = "Years: 2019-2023",
      y = "Trust in Scientists (1-7)",
      title = "Trust in Scientists - Oracle Data (Ground Truth)"
    ) +
    theme_bw()

  # save oracle plots
  library(patchwork)

  oracle_combined <- oracle_science_plot + oracle_scientists_plot +
    plot_annotation(
      title = "Oracle Data: True Population Trends (No Missing Data)",
      subtitle = "Shows actual trust trajectories before selective attrition"
    )

  ggsave(
    filename = "results/figures/oracle_combined.png",
    plot = oracle_combined,
    width = 14, height = 6, dpi = 300
  )

  # print oracle results
  cat("\nOracle Data Results:\n")
  cat("\nTrust in Science (Oracle - Ground Truth):\n")
  cat("  Year 0:", round(gee_predictions_science_oracle$predicted[1], 3), "\n")
  cat("  Year 4:", round(gee_predictions_science_oracle$predicted[5], 3), "\n")
  cat("  Change:", round(gee_predictions_science_oracle$predicted[5] -
                       gee_predictions_science_oracle$predicted[1], 3), "\n")

  cat("\nTrust in Scientists (Oracle - Ground Truth):\n")
  cat("  Year 0:", round(gee_predictions_scientists_oracle$predicted[1], 3), "\n")
  cat("  Year 4:", round(gee_predictions_scientists_oracle$predicted[5], 3), "\n")
  cat("  Change:", round(gee_predictions_scientists_oracle$predicted[5] -
                       gee_predictions_scientists_oracle$predicted[1], 3), "\n")

  # create comparison plot
  cat("\nCreating comparison plot (Observed vs Imputed vs Oracle)...\n")

  # prepare data for comparison
  comparison_science <- bind_rows(
    gee_predictions_science_observed %>%
      as.data.frame() %>%
      mutate(Data = "Observed (Complete Cases)"),
    gee_predictions_science_imputed %>%
      as.data.frame() %>%
      mutate(Data = "Imputed (Missing Corrected)"),
    gee_predictions_science_oracle %>%
      as.data.frame() %>%
      mutate(Data = "Oracle (Ground Truth)")
  )

  comparison_plot <- ggplot(comparison_science, aes(x = x, y = predicted,
                                                    color = Data, fill = Data)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Observed (Complete Cases)" = "red",
                                 "Imputed (Missing Corrected)" = "dodgerblue",
                                 "Oracle (Ground Truth)" = "darkgreen")) +
    scale_fill_manual(values = c("Observed (Complete Cases)" = "red",
                                "Imputed (Missing Corrected)" = "dodgerblue",
                                "Oracle (Ground Truth)" = "darkgreen")) +
    scale_x_continuous(breaks = 0:4, labels = 2019:2023) +
    labs(
      x = "Year",
      y = "Trust in Science (1-7)",
      title = "Comparison of Trust in Science Trajectories",
      subtitle = "Showing how selective attrition biases observed trends",
      caption = "Oracle = true population trend; Observed = complete cases only; Imputed = missing data corrected"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

  ggsave(
    filename = "results/figures/comparison_trust_science.png",
    plot = comparison_plot,
    width = 10, height = 8, dpi = 300
  )

  cat("  ✓ Oracle analysis complete\n")

} else {
  cat("\nWarning: Oracle data not found at:\n")
  cat("  -", oracle_path_rds, "\n")
  cat("  -", oracle_path_csv, "\n")
}
cat("\nNext step: Run 03_create_visualizations.R\n")


# this is the key plot
#comparison_plot
