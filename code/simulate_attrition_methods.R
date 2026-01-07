# simulate_attrition_methods.R - simplified simulation with baseline trust dropout
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(MASS) # for polr
library(splines)
library(ggeffects)
library(geepack)
library(here)

# anchor here() to this script location
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg) else NULL
if (!is.null(script_path)) {
  script_path_abs <- script_path
  if (!grepl("^/", script_path_abs)) {
    script_path_abs <- file.path(getwd(), script_path_abs)
  }
  script_path_abs <- normalizePath(script_path_abs, mustWork = FALSE)
  if (file.exists(script_path_abs)) {
    project_root <- normalizePath(file.path(dirname(script_path_abs), ".."), mustWork = FALSE)
    if (dir.exists(project_root)) {
      here::set_here(project_root)
    }
  }
}
tryCatch(
  here::i_am("code/simulate_attrition_methods.R"),
  error = function(err) {
    message("Warning: here::i_am failed; using current working directory.")
  }
)

# ensure output directories exist
output_dirs <- c(
  here::here("results", "figures"),
  here::here("results", "objects")
)
for (dir_path in output_dirs) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# helper functions
scale_vec <- function(x) {
  x <- as.numeric(x)
  x_sd <- sd(x, na.rm = TRUE)
  if (is.na(x_sd) || x_sd == 0) {
    return(rep(0, length(x)))
  }
  as.numeric((x - mean(x, na.rm = TRUE)) / x_sd)
}

calibrate_intercept <- function(eta, target_prob) {
  target_prob <- pmin(pmax(target_prob, 1e-6), 1 - 1e-6)
  if (length(eta) == 0) {
    return(qlogis(target_prob))
  }
  f <- function(a) mean(plogis(a + eta)) - target_prob
  uniroot(f, interval = c(-10, 10))$root
}

# parameters
n_participants <- 40000 # larger sample for clearer patterns
n_waves <- 5
baseline_year <- 0

# generate participant characteristics
cat("Creating participant characteristics...\n")
participants <- data.frame(
  id = 1:n_participants,
  # age distribution - slightly older to reflect NZAVS oversampling
  age_baseline = round(rnorm(n_participants, mean = 50, sd = 15)),
  # gender - oversample women to reflect NZAVS design
  gender = sample(c("Female", "Male"), n_participants,
    replace = TRUE, prob = c(0.62, 0.38)
  ),
  # ethnicity - slightly oversample Māori, undersample Pacific/Asian
  ethnicity = sample(c("NZ European", "Maori", "Pacific", "Asian", "Other"),
    n_participants,
    replace = TRUE,
    prob = c(0.68, 0.17, 0.06, 0.10, 0.04)
  ),
  # education: 1-7 scale - key predictor of trust group
  education = sample(1:7, n_participants,
    replace = TRUE,
    prob = c(0.05, 0.10, 0.20, 0.25, 0.20, 0.15, 0.05)
  )
)

# assign trust groups for simulation
group_probs <- c(low = 0.1, medium = 0.3, high = 0.6)
participants$trust_group <- sample(
  names(group_probs),
  n_participants,
  replace = TRUE,
  prob = group_probs
)

# generate baseline trust from group membership
cat("\nGenerating baseline trust from group membership...\n")
participants <- participants |>
  mutate(
    trust_science_baseline = case_when(
      trust_group == "low" ~ sample(1:3, n(), replace = TRUE),
      trust_group == "medium" ~ sample(4:5, n(), replace = TRUE),
      trust_group == "high" ~ sample(6:7, n(), replace = TRUE)
    ),
    # auxiliary only, used to improve imputation of trust_science
    trust_scientists_baseline = pmin(
      7,
      pmax(1, round(trust_science_baseline + rnorm(n(), 0, 0.5)))
    )
  )

# check correlation
cat(
  "Baseline correlation between trust measures:",
  round(cor(
    participants$trust_science_baseline,
    participants$trust_scientists_baseline
  ), 3), "\n"
)

# check distribution
cat("\nBaseline trust by group:\n")
aggregate(trust_science_baseline ~ trust_group,
  data = participants,
  function(x) round(c(mean = mean(x), n = length(x)), 2)
)

# trust groups and baseline scores now drive the attrition mechanism

# create long format data
cat("\nCreating longitudinal structure...\n")
long_data <- expand.grid(
  id = participants$id,
  years = 0:(n_waves - 1),
  stringsAsFactors = FALSE
)

# merge with participant data
long_data <- merge(long_data, participants, by = "id")

# add wave variable
long_data$wave <- factor(baseline_year + long_data$years,
  levels = baseline_year:(baseline_year + n_waves - 1)
)

# generate trust trajectories using linear slopes
cat("Generating trust outcomes with linear slopes...\n")

# define slopes for each group - low and medium decline, high stays stable
slopes <- c(low = -0.1, medium = -0.1, high = 0.0)

long_data <- long_data %>%
  mutate(
    # trust in science with linear trend
    trust_science = trust_science_baseline +
      slopes[trust_group] * years +
      if_else(years == 0, 0, rnorm(n(), 0, 0.15)),
    # trust in scientists with similar linear trend
    trust_scientists = trust_scientists_baseline +
      slopes[trust_group] * years +
      if_else(years == 0, 0, rnorm(n(), 0, 0.15))
  ) |>
  # bound to 1-7 scale
  mutate(across(
    c(trust_science, trust_scientists),
    ~ round(pmin(7, pmax(1, .)))
  ))

# add post-stratification weights (before missingness)
cat("\nAdding post-stratification weights...\n")
long_data$weights <- 1

# save oracle data (before missingness)
cat("\nSaving oracle data...\n")
oracle_data <- long_data %>%
  arrange(id, years)

# generate dropout with group retention targets
cat("\nGenerating dropout with group retention targets...\n")

target_retention <- c(high = 0.70, medium = 0.45, low = 0.25)
drop_prob_group <- 1 - target_retention^(1 / (n_waves - 1))

cat("Per-wave dropout probabilities:\n")
print(round(drop_prob_group, 3))

group_sizes <- participants |>
  count(trust_group) |>
  tibble::deframe()
group_sizes <- group_sizes[names(drop_prob_group)]

expected_overall_attrition <- function(drop_probs, group_sizes, n_waves) {
  survival <- group_sizes
  attrition <- numeric(n_waves - 1)
  for (t in seq_len(n_waves - 1)) {
    dropped <- sum(survival * drop_probs)
    at_risk <- sum(survival)
    attrition[t] <- dropped / at_risk
    survival <- survival * (1 - drop_probs)
  }
  attrition
}

expected_attrition <- expected_overall_attrition(drop_prob_group, group_sizes, n_waves)
cat("Expected overall attrition by year:\n")
print(round(expected_attrition, 3))

cat("Target retention (final wave):\n")
print(target_retention)

# function to compute categorical summaries
compute_cat_summary <- function(data) {
  data %>%
    mutate(
      trust_science_factor = factor(
        case_when(
          is.na(trust_science) ~ NA_character_,
          trust_science <= 3 ~ "low",
          trust_science < 6 ~ "med",
          trust_science >= 6 ~ "high"
        ),
        levels = c("low", "med", "high"),
        ordered = TRUE
      ),
      trust_scientists_factor = factor(
        case_when(
          is.na(trust_scientists) ~ NA_character_,
          trust_scientists <= 3 ~ "low",
          trust_scientists < 6 ~ "med",
          trust_scientists >= 6 ~ "high"
        ),
        levels = c("low", "med", "high"),
        ordered = TRUE
      )
    )
}

simulate_dropout <- function(long_data, scenario_label, drop_prob_group, beta_params) {
  long_data <- long_data |>
    arrange(id, years) |>
    group_by(id) |>
    mutate(
      lag_trust_science = lag(trust_science, default = dplyr::first(trust_science_baseline))
    ) |>
    ungroup()

  ids <- sort(unique(long_data$id))
  drop_year <- rep(NA_integer_, length(ids))
  names(drop_year) <- ids

  for (t in seq_len(n_waves - 1)) {
    ids_at_risk <- ids[is.na(drop_year)]
    if (length(ids_at_risk) == 0) {
      next
    }

    data_t <- long_data |>
      dplyr::filter(years == t, id %in% ids_at_risk)

    if (nrow(data_t) == 0) {
      next
    }

    for (group_name in names(drop_prob_group)) {
      data_g <- data_t |>
        dplyr::filter(trust_group == group_name)

      if (nrow(data_g) == 0) {
        next
      }

      eta <- beta_params$beta_baseline * scale_vec(data_g$trust_science_baseline) +
        beta_params$beta_lag * scale_vec(data_g$lag_trust_science)

      if (scenario_label == "mnar") {
        eta <- eta + beta_params$beta_current * scale_vec(data_g$trust_science)
      }

      intercept <- calibrate_intercept(eta, drop_prob_group[group_name])
      drop_prob <- plogis(intercept + eta)
      dropped_now <- rbinom(nrow(data_g), 1, drop_prob)

      drop_ids <- as.character(data_g$id[dropped_now == 1])
      drop_year[drop_ids] <- t
    }
  }

  long_data <- long_data |>
    dplyr::mutate(
      drop_year = drop_year[as.character(id)],
      dropped = if_else(!is.na(drop_year) & years >= drop_year, 1, 0)
    )

  long_data <- long_data |>
    dplyr::mutate(
      trust_science_obs = if_else(dropped == 1, NA_real_, trust_science),
      trust_scientists_obs = if_else(dropped == 1, NA_real_, trust_scientists)
    )

  observed_data <- long_data |>
    dplyr::mutate(
      trust_science = trust_science_obs,
      trust_scientists = trust_scientists_obs
    ) |>
    dplyr::select(-trust_science_obs, -trust_scientists_obs, -lag_trust_science) |>
    arrange(id, years)

  attrition_summary <- long_data |>
    group_by(trust_group, years) |>
    summarise(
      n_total = n(),
      n_dropped = sum(dropped == 1),
      pct_dropped = round(100 * n_dropped / n_total, 1),
      .groups = "drop"
    ) |>
    pivot_wider(names_from = years, values_from = pct_dropped, names_prefix = "Year_") |>
    dplyr::mutate(scenario = toupper(scenario_label))

  df_person <- long_data |>
    dplyr::filter(years == 0) |>
    dplyr::select(id, trust_group, drop_year)

  dropout_rates <- tidyr::expand_grid(
    trust_group = names(drop_prob_group),
    years = 1:(n_waves - 1)
  ) |>
    rowwise() |>
    mutate(
      n_at_risk = sum(
        df_person$trust_group == trust_group &
          (is.na(df_person$drop_year) | df_person$drop_year >= years)
      ),
      n_drop = sum(
        df_person$trust_group == trust_group &
          df_person$drop_year == years
      ),
      drop_rate = if_else(n_at_risk == 0, NA_real_, n_drop / n_at_risk),
      target_drop_rate = drop_prob_group[trust_group]
    ) |>
    ungroup() |>
    dplyr::mutate(scenario = toupper(scenario_label))

  retention_summary <- df_person |>
    group_by(trust_group) |>
    summarise(
      retention = mean(is.na(drop_year)),
      retention_pct = round(100 * retention, 1),
      .groups = "drop"
    ) |>
    mutate(
      target_retention = target_retention[trust_group],
      target_retention_pct = round(100 * target_retention, 1),
      scenario = toupper(scenario_label)
    )

  list(
    long_data = long_data,
    observed_data = observed_data,
    attrition_summary = attrition_summary,
    dropout_rates = dropout_rates,
    retention_summary = retention_summary
  )
}

run_scenario_analysis <- function(oracle_data, observed_data, scenario_label, attrition_summary, dropout_rates, retention_summary) {
  scenario_lower <- tolower(scenario_label)

  cat("\n\n=== COMPARING ORACLE VS OBSERVED (", scenario_label, ") ===\n")

  # fit models to oracle data
  cat("\nFitting model to oracle data...\n")
  gee_formula <- trust_science ~ factor(years)
  mod_oracle <- geepack::geeglm(
    gee_formula,
    id = id,
    data = oracle_data,
    corstr = "exchangeable"
  )

  pred_oracle <- predict_response(mod_oracle, "years")
  cat("Oracle trajectory (gee, factor years):\n")
  print(round(pred_oracle$predicted, 3))

  # fit models to observed data (available cases)
  cat("\nFitting model to observed data (available cases)...\n")
  observed_complete <- observed_data[!is.na(observed_data$trust_science), ] |>
    arrange(id, years)

  mod_observed <- geepack::geeglm(
    gee_formula,
    id = id,
    weights = weights,
    data = observed_complete,
    corstr = "exchangeable"
  )

  pred_observed <- predict_response(mod_observed, "years")
  cat("Observed trajectory (gee, factor years):\n")
  print(round(pred_observed$predicted, 3))

  # ========================================================================
  # IPCW ANALYSIS
  # ========================================================================
  cat("\n=== RUNNING IPCW ANALYSIS ===\n")

  dat_ipcw <- observed_data |>
    arrange(id, years) |>
    group_by(id) |>
    mutate(
      observed = as.numeric(!is.na(trust_science)),
      lag_trust_science = lag(trust_science, default = dplyr::first(trust_science_baseline)),
      at_risk = lag(observed, default = 1) == 1
    ) |>
    ungroup()

  dat_at_risk <- dat_ipcw |>
    dplyr::filter(at_risk == 1 & years > 0)

  dat_model <- dat_at_risk

  dropout_model <- glm(
    observed ~ trust_science_baseline + lag_trust_science +
      age_baseline + gender + ethnicity + education + factor(years),
    data = dat_model,
    family = binomial
  )

  dat_model <- dat_model |>
    mutate(prob_remain = predict(dropout_model, type = "response"))

  marginal_model <- glm(
    observed ~ factor(years),
    data = dat_model,
    family = binomial
  )

  dat_model <- dat_model |>
    mutate(
      prob_remain_marginal = predict(marginal_model, type = "response"),
      weight_stab = prob_remain_marginal / prob_remain
    )

  p01 <- quantile(dat_model$weight_stab, 0.01)
  p99 <- quantile(dat_model$weight_stab, 0.99)

  dat_model <- dat_model |>
    mutate(weight_trunc = pmin(pmax(weight_stab, p01), p99))

  dat_weights <- dat_model |>
    arrange(id, years) |>
    group_by(id) |>
    mutate(weight_cumulative = cumprod(weight_trunc)) |>
    ungroup() |>
    dplyr::select(id, years, weight_cumulative)

  min_year <- min(observed_data$years)

  dat_weighted <- dat_ipcw |>
    left_join(dat_weights, by = c("id", "years")) |>
    mutate(
      ipcw = if_else(years == min_year, 1, weight_cumulative),
      ipcw = if_else(is.na(ipcw), 0, ipcw),
      combined_weight = ipcw * weights
    )

  ipcw_complete <- dat_weighted |>
    dplyr::filter(observed == 1)

  mod_ipcw <- geepack::geeglm(
    gee_formula,
    id = id,
    weights = combined_weight,
    data = ipcw_complete,
    corstr = "exchangeable"
  )

  pred_ipcw <- predict_response(mod_ipcw, "years")
  cat("IPCW trajectory (gee, factor years):\n")
  print(round(pred_ipcw$predicted, 3))

  # create visualizations
  library(patchwork)

  cat("\n\n=== CREATING CONTINUOUS OUTCOME PLOTS ===\n")

  oracle_mean_plot <- oracle_data |>
    group_by(years) |>
    summarise(mean_trust = mean(trust_science), .groups = "drop")

  observed_mean_plot <- observed_complete |>
    group_by(years) |>
    summarise(mean_trust = mean(trust_science), .groups = "drop")

  # oracle plot
  oracle_plot_science <- ggplot(
    oracle_data,
    aes(x = years, y = trust_science)
  ) +
    geom_jitter(alpha = 0.02, width = 0.2, height = 0.5, color = "grey40") +
    geom_line(
      data = oracle_mean_plot,
      aes(y = mean_trust),
      color = "darkgreen",
      linewidth = 1.1
    ) +
    geom_point(
      data = oracle_mean_plot,
      aes(y = mean_trust),
      color = "darkgreen",
      size = 3
    ) +
    scale_x_continuous(breaks = 0:4, labels = 1:5) +
    labs(
      x = "Year of Study",
      y = "Trust in Science (1-7)",
      title = "Trust in Science - Oracle (Ground Truth)"
    ) +
    theme_bw()

  # observed plot
  observed_plot_science <- ggplot(
    observed_complete,
    aes(x = years, y = trust_science)
  ) +
    geom_jitter(alpha = 0.02, width = 0.2, height = 0.5, color = "grey40") +
    geom_line(
      data = observed_mean_plot,
      aes(y = mean_trust),
      color = "red",
      linewidth = 1.1
    ) +
    geom_point(
      data = observed_mean_plot,
      aes(y = mean_trust),
      color = "red",
      size = 3
    ) +
    scale_x_continuous(breaks = 0:4, labels = 1:5) +
    labs(
      x = "Year of Study",
      y = "Trust in Science (1-7)",
      title = "Trust in Science - Observed (Survivors)"
    ) +
    theme_bw()

  # combined plot
  combined_continuous <- oracle_plot_science + observed_plot_science +
    plot_annotation(
      title = paste("Selection Bias in Trust Trajectories (", scenario_label, ")", sep = ""),
      subtitle = "Low trust individuals drop out, creating artificial stability"
    )

  ggsave(
    here::here("results", "figures", paste0("test_3_continuous_comparison_", scenario_lower, ".png")),
    combined_continuous,
    width = 12,
    height = 6,
    dpi = 300
  )

  # summary statistics
  cat("\n\nSUMMARY (MEAN TRAJECTORIES):\n")
  cat(
    "Oracle: Year 0 =", round(oracle_mean_plot$mean_trust[1], 3),
    "Year 4 =", round(oracle_mean_plot$mean_trust[5], 3),
    "Change =", round(oracle_mean_plot$mean_trust[5] - oracle_mean_plot$mean_trust[1], 3), "\n"
  )
  cat(
    "Observed: Year 0 =", round(observed_mean_plot$mean_trust[1], 3),
    "Year 4 =", round(observed_mean_plot$mean_trust[5], 3),
    "Change =", round(observed_mean_plot$mean_trust[5] - observed_mean_plot$mean_trust[1], 3), "\n"
  )

  # check who remains by year 5
  cat("\n\nWho remains by year 5:\n")
  year4_complete <- observed_complete |>
    dplyr::filter(years == 4) |>
    group_by(trust_group) |>
    summarise(n = n(), mean_trust = round(mean(trust_science), 2), .groups = "drop")

  year4_oracle <- oracle_data |>
    dplyr::filter(years == 4) |>
    group_by(trust_group) |>
    summarise(n_oracle = n(), .groups = "drop")

  year4_summary <- left_join(year4_complete, year4_oracle) |>
    mutate(retention_pct = round(100 * n / n_oracle, 1))

  print(year4_summary)

  cat("\n\nAttrition by group and year:\n")
  print(attrition_summary)

  cat("\n\nPer-wave dropout rates (observed vs target):\n")
  print(dropout_rates)

  cat("\n\nFinal retention vs target:\n")
  print(retention_summary)

  # show overall missingness pattern
  cat("\n\nMissingness pattern in observed data:\n")
  missing_summary <- observed_data |>
    group_by(years) |>
    summarise(
      n_total = n(),
      n_missing = sum(is.na(trust_science)),
      pct_missing = round(100 * n_missing / n_total, 1),
      .groups = "drop"
    )
  print(missing_summary)

  # ========================================================================
  # AMELIA IMPUTATION
  # ========================================================================
  cat("\n\n=== RUNNING AMELIA IMPUTATION ===\n")
  library(Amelia)
  library(mice)
  cat("\nVariables in observed_data:", paste(names(observed_data), collapse = ", "), "\n\n")

  amelia_data <- observed_data |>
    dplyr::select(
      -trust_group, -drop_year, -dropped
    ) |>
    arrange(id, years)

  amelia_data$wave <- as.character(amelia_data$wave)

  cat("Variables in amelia_data:", paste(names(amelia_data), collapse = ", "), "\n\n")

  id_vars <- c("wave", "weights")
  nominal_vars <- c("gender", "ethnicity")

  bounds_matrix <- matrix(c(
    which(names(amelia_data) == "trust_science"), 1, 7,
    which(names(amelia_data) == "trust_scientists"), 1, 7,
    which(names(amelia_data) == "trust_science_baseline"), 1, 7,
    which(names(amelia_data) == "trust_scientists_baseline"), 1, 7
  ), nrow = 4, ncol = 3, byrow = TRUE)

  cat("Running Amelia with m=10 imputations...\n")
  amelia_out <- amelia(
    amelia_data,
    m = 10,
    idvars = id_vars,
    noms = nominal_vars,
    bounds = bounds_matrix,
    ords = c(
      "trust_science",
      "trust_scientists",
      "trust_science_baseline",
      "trust_scientists_baseline"
    ),
    lags = c("trust_science", "trust_scientists"),
    splinetime = 3,
    polytime = 3,
    ts = "years",
    cs = "id"
  )

  cat("Imputation complete!\n")

  # ========================================================================
  # CONVERT TO MICE FORMAT AND FIT MODELS
  # ========================================================================
  cat("\n=== CONVERTING TO MICE FORMAT ===\n")

  source(here::here("code/functions/margot_amelia_to_mice_fixed.R"))
  source(here::here("code/functions/fix_mids_factors.R"))

  mids_data <- margot_amelia_to_mice_fixed(
    amelia_out,
    original_data = amelia_data,
    verbose = TRUE
  )

  mids_data <- fix_mids_factors(mids_data, c("trust_science_factor", "trust_scientists_factor"))

  # ========================================================================
  # RUN MICE IMPUTATION (LONG FORMAT) FOR COMPARISON
  # ========================================================================
  cat("\n=== RUNNING MICE IMPUTATION (LONG FORMAT) ===\n")

  n_imputations_mice <- 5

  mice_long_data <- observed_data |>
    mutate(
      gender = as.factor(gender),
      ethnicity = as.factor(ethnicity)
    ) |>
    arrange(id, years) |>
    group_by(id) |>
    mutate(
      lag_trust_science = lag(trust_science),
      lag_trust_scientists = lag(trust_scientists),
      lag_trust_science = if_else(years == 0, trust_science_baseline, lag_trust_science),
      lag_trust_scientists = if_else(years == 0, trust_scientists_baseline, lag_trust_scientists)
    ) |>
    ungroup()

  mice_methods <- mice::make.method(mice_long_data)
  mice_methods[c(
    "id",
    "weights",
    "trust_science_baseline",
    "trust_scientists_baseline",
    "lag_trust_science",
    "lag_trust_scientists"
  )] <- ""
  mice_methods["trust_science"] <- "pmm"
  mice_methods["trust_scientists"] <- "pmm"

  predictor_matrix <- mice::make.predictorMatrix(mice_long_data)
  predictor_matrix[, "id"] <- 0
  predictor_matrix[c(
    "id",
    "weights",
    "trust_science_baseline",
    "trust_scientists_baseline",
    "lag_trust_science",
    "lag_trust_scientists"
  ), ] <- 0

  trust_science_predictors <- c(
    "trust_scientists",
    "trust_science_baseline",
    "lag_trust_science",
    "years",
    "age_baseline",
    "gender",
    "ethnicity",
    "education",
    "weights"
  )

  trust_scientists_predictors <- c(
    "trust_science",
    "trust_scientists_baseline",
    "lag_trust_scientists",
    "years",
    "age_baseline",
    "gender",
    "ethnicity",
    "education",
    "weights"
  )

  predictor_matrix["trust_science", ] <- 0
  predictor_matrix["trust_science", trust_science_predictors] <- 1
  predictor_matrix["trust_scientists", ] <- 0
  predictor_matrix["trust_scientists", trust_scientists_predictors] <- 1

  mice_obj <- mice::mice(
    mice_long_data,
    m = n_imputations_mice,
    method = mice_methods,
    predictorMatrix = predictor_matrix,
    printFlag = FALSE,
    seed = 2025
  )

  # compute imputed means
  cat("\n\n=== COMPUTING IMPUTED MEANS ===\n")

  compute_weighted_means <- function(data, weight_var) {
    data |>
      group_by(years) |>
      summarise(
        mean_trust = weighted.mean(trust_science, .data[[weight_var]], na.rm = TRUE),
        .groups = "drop"
      )
  }

  oracle_means <- compute_weighted_means(oracle_data, "weights") |>
    mutate(method = "Oracle")
  observed_means <- compute_weighted_means(observed_data, "weights") |>
    mutate(method = "Observed")
  ipcw_means <- compute_weighted_means(ipcw_complete, "combined_weight") |>
    mutate(method = "IPCW")

  amelia_means_list <- lapply(seq_len(amelia_out$m), function(i) {
    amelia_out$imputations[[i]] |>
      group_by(years) |>
      summarise(
        mean_trust = weighted.mean(trust_science, weights, na.rm = TRUE),
        .groups = "drop"
      )
  })

  amelia_means <- bind_rows(amelia_means_list, .id = "imp") |>
    group_by(years) |>
    summarise(
      mean_trust = mean(mean_trust),
      .groups = "drop"
    ) |>
    mutate(method = "Amelia")

  mice_means_list <- lapply(seq_len(n_imputations_mice), function(i) {
    mice::complete(mice_obj, i) |>
      group_by(years) |>
      summarise(
        mean_trust = weighted.mean(trust_science, weights, na.rm = TRUE),
        .groups = "drop"
      )
  })

  mice_means <- bind_rows(mice_means_list, .id = "imp") |>
    group_by(years) |>
    summarise(
      mean_trust = mean(mean_trust),
      .groups = "drop"
    ) |>
    mutate(method = "MICE")

  continuous_means <- bind_rows(
    oracle_means,
    observed_means,
    ipcw_means,
    amelia_means,
    mice_means
  ) |>
    mutate(scenario = scenario_label)

  continuous_change <- continuous_means |>
    group_by(method) |>
    summarise(
      year_0 = mean_trust[years == min(years)],
      year_last = mean_trust[years == max(years)],
      change = year_last - year_0,
      .groups = "drop"
    ) |>
    mutate(
      method = factor(method, levels = c("Oracle", "Observed", "IPCW", "Amelia", "MICE")),
      scenario = scenario_label
    ) |>
    arrange(method)

  cat("\nTrust in Science Results (weighted means):\n")
  for (i in seq_len(nrow(continuous_change))) {
    row <- continuous_change[i, ]
    cat(
      as.character(row$method), ": Year 0 =", round(row$year_0, 3),
      "Year 4 =", round(row$year_last, 3),
      "Change =", round(row$change, 3), "\n"
    )
  }

  p_mean_comparison <- ggplot(
    continuous_means,
    aes(x = years, y = mean_trust, color = method)
  ) +
    geom_line(size = 1.1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 0:4, labels = 1:5) +
    # scale_y_continuous(limits = c(1, 7)) +
    labs(
      x = "Year of Study",
      y = "Mean Trust in Science (1-7)",
      title = paste("Trust in Science: Mean Recovery by Method -", scenario_label),
      subtitle = "Year means with retention-driven attrition"
    ) +
    theme_minimal()

  ggsave(
    here::here("results", "figures", paste0("test_3_mean_comparison_", scenario_lower, ".png")),
    p_mean_comparison,
    width = 10,
    height = 6,
    dpi = 300
  )

  # ========================================================================
  # CATEGORICAL AND ORDINAL COMPARISONS
  # ========================================================================
  cat("\n\n=== CATEGORICAL AND ORDINAL COMPARISONS ===\n")

  create_trust_categories <- function(trust_var) {
    factor(
      case_when(
        is.na(trust_var) ~ NA_character_,
        trust_var <= 3 ~ "Low",
        trust_var < 6 ~ "Medium",
        trust_var >= 6 ~ "High"
      ),
      levels = c("Low", "Medium", "High"),
      ordered = TRUE
    )
  }

  oracle_cat_props <- oracle_data |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    group_by(years) |>
    summarise(
      low = weighted.mean(trust_cat == "Low", weights, na.rm = TRUE),
      medium = weighted.mean(trust_cat == "Medium", weights, na.rm = TRUE),
      high = weighted.mean(trust_cat == "High", weights, na.rm = TRUE),
      .groups = "drop"
    )

  cc_cat_props <- observed_data |>
    dplyr::filter(!is.na(trust_science)) |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    group_by(years) |>
    summarise(
      low = weighted.mean(trust_cat == "Low", weights, na.rm = TRUE),
      medium = weighted.mean(trust_cat == "Medium", weights, na.rm = TRUE),
      high = weighted.mean(trust_cat == "High", weights, na.rm = TRUE),
      .groups = "drop"
    )

  ipcw_cat_props <- ipcw_complete |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    group_by(years) |>
    summarise(
      low = weighted.mean(trust_cat == "Low", combined_weight, na.rm = TRUE),
      medium = weighted.mean(trust_cat == "Medium", combined_weight, na.rm = TRUE),
      high = weighted.mean(trust_cat == "High", combined_weight, na.rm = TRUE),
      .groups = "drop"
    )

  amelia_cat_props_list <- lapply(seq_len(amelia_out$m), function(i) {
    amelia_out$imputations[[i]] |>
      mutate(trust_cat = create_trust_categories(trust_science)) |>
      group_by(years) |>
      summarise(
        low = weighted.mean(trust_cat == "Low", weights, na.rm = TRUE),
        medium = weighted.mean(trust_cat == "Medium", weights, na.rm = TRUE),
        high = weighted.mean(trust_cat == "High", weights, na.rm = TRUE),
        .groups = "drop"
      )
  })

  amelia_cat_props <- bind_rows(amelia_cat_props_list, .id = "imp") |>
    group_by(years) |>
    summarise(
      low = mean(low),
      medium = mean(medium),
      high = mean(high),
      .groups = "drop"
    )

  mice_cat_props_list <- lapply(seq_len(n_imputations_mice), function(i) {
    mice::complete(mice_obj, i) |>
      mutate(trust_cat = create_trust_categories(trust_science)) |>
      group_by(years) |>
      summarise(
        low = weighted.mean(trust_cat == "Low", weights, na.rm = TRUE),
        medium = weighted.mean(trust_cat == "Medium", weights, na.rm = TRUE),
        high = weighted.mean(trust_cat == "High", weights, na.rm = TRUE),
        .groups = "drop"
      )
  })

  mice_cat_props <- bind_rows(mice_cat_props_list, .id = "imp") |>
    group_by(years) |>
    summarise(
      low = mean(low),
      medium = mean(medium),
      high = mean(high),
      .groups = "drop"
    )

  fit_polr_thresholds <- function(data, weight_var = "weights") {
    model <- MASS::polr(
      trust_cat ~ ns(years, 3),
      data = data,
      weights = data[[weight_var]],
      Hess = TRUE
    )
    tibble(
      threshold_low_med = model$zeta[1],
      threshold_med_high = model$zeta[2]
    )
  }

  pool_thresholds <- function(threshold_list, method_name) {
    bind_rows(threshold_list) |>
      summarise(
        threshold_low_med = mean(threshold_low_med),
        threshold_low_med_sd = sd(threshold_low_med),
        threshold_med_high = mean(threshold_med_high),
        threshold_med_high_sd = sd(threshold_med_high)
      ) |>
      mutate(method = method_name)
  }

  oracle_thresholds <- oracle_data |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    fit_polr_thresholds() |>
    mutate(
      threshold_low_med_sd = NA_real_,
      threshold_med_high_sd = NA_real_,
      method = "Oracle"
    )

  cc_thresholds <- observed_data |>
    dplyr::filter(!is.na(trust_science)) |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    fit_polr_thresholds() |>
    mutate(
      threshold_low_med_sd = NA_real_,
      threshold_med_high_sd = NA_real_,
      method = "Complete Case"
    )

  ipcw_thresholds <- ipcw_complete |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    fit_polr_thresholds(weight_var = "combined_weight") |>
    mutate(
      threshold_low_med_sd = NA_real_,
      threshold_med_high_sd = NA_real_,
      method = "IPCW"
    )

  amelia_thresholds <- lapply(seq_len(amelia_out$m), function(i) {
    amelia_out$imputations[[i]] |>
      mutate(trust_cat = create_trust_categories(trust_science)) |>
      fit_polr_thresholds()
  }) |>
    pool_thresholds("Amelia")

  mice_thresholds <- lapply(seq_len(n_imputations_mice), function(i) {
    mice::complete(mice_obj, i) |>
      mutate(trust_cat = create_trust_categories(trust_science)) |>
      fit_polr_thresholds()
  }) |>
    pool_thresholds("MICE")

  ordinal_thresholds <- bind_rows(
    oracle_thresholds,
    cc_thresholds,
    ipcw_thresholds,
    amelia_thresholds,
    mice_thresholds
  ) |>
    mutate(scenario = scenario_label)

  cat("\nOrdinal model thresholds (pooled where applicable):\n")
  print(ordinal_thresholds)

  cat_prop_long <- bind_rows(
    oracle_cat_props |>
      mutate(method = "Oracle"),
    cc_cat_props |>
      mutate(method = "Complete Case"),
    ipcw_cat_props |>
      mutate(method = "IPCW"),
    amelia_cat_props |>
      mutate(method = "Amelia"),
    mice_cat_props |>
      mutate(method = "MICE")
  ) |>
    pivot_longer(
      cols = c(low, medium, high),
      names_to = "category",
      values_to = "proportion"
    ) |>
    mutate(
      category = case_when(
        category == "low" ~ "Low",
        category == "medium" ~ "Medium",
        category == "high" ~ "High",
        TRUE ~ category
      ),
      scenario = scenario_label
    )

  cat_shift_summary <- cat_prop_long |>
    group_by(method, category) |>
    summarise(
      shift = proportion[years == max(years)] - proportion[years == min(years)],
      .groups = "drop"
    )

  cat("\nCategory shift summary (final - baseline):\n")
  print(cat_shift_summary)

  cat_shift_summary_pct <- cat_shift_summary |>
    mutate(shift_pct = 100 * shift)

  cat("\nCategory shift summary (percentage points):\n")
  print(cat_shift_summary_pct)

  cat("\n=== CATEGORY TRANSITION DIAGNOSTICS ===\n")

  baseline_categories <- oracle_data |>
    dplyr::filter(years == 0) |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    dplyr::select(id, trust_cat)

  final_categories_oracle <- oracle_data |>
    dplyr::filter(years == max(years)) |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    dplyr::select(id, trust_cat) |>
    rename(trust_cat_final = trust_cat)

  final_categories_observed <- observed_data |>
    dplyr::filter(years == max(years)) |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    dplyr::select(id, trust_cat) |>
    rename(trust_cat_final = trust_cat)

  oracle_transitions <- baseline_categories |>
    left_join(final_categories_oracle, by = "id") |>
    count(trust_cat, trust_cat_final) |>
    group_by(trust_cat) |>
    mutate(row_prop = n / sum(n)) |>
    ungroup()

  observed_transitions <- baseline_categories |>
    left_join(final_categories_observed, by = "id") |>
    count(trust_cat, trust_cat_final) |>
    group_by(trust_cat) |>
    mutate(row_prop = n / sum(n)) |>
    ungroup()

  cat("\nOracle transitions (baseline to final, row proportions):\n")
  print(oracle_transitions)

  cat("\nObserved transitions (baseline to final, row proportions):\n")
  print(observed_transitions)

  observed_by_group <- observed_data |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    dplyr::filter(!is.na(trust_cat)) |>
    group_by(years, trust_group, trust_cat) |>
    summarise(
      n = n(),
      .groups = "drop"
    ) |>
    group_by(years, trust_group) |>
    mutate(
      total_n = sum(n),
      prop = n / total_n
    ) |>
    ungroup()

  cat("\nObserved category proportions by trust_group and year:\n")
  print(observed_by_group)

  observed_prop_check <- observed_by_group |>
    group_by(years, trust_group) |>
    summarise(prop_sum = sum(prop), .groups = "drop")

  cat("\nObserved category proportion sums (should be 1):\n")
  print(observed_prop_check)

  p_cat <- ggplot(
    cat_prop_long,
    aes(x = years, y = proportion, color = method, group = method)
  ) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = c(
      "Oracle" = "black",
      "Complete Case" = "red",
      "IPCW" = "blue",
      "Amelia" = "darkgreen",
      "MICE" = "orange"
    )) +
    labs(
      x = "Year",
      y = "Proportion",
      title = paste("Category Proportions by Method -", scenario_label),
      subtitle = "Low = ≤3, Medium = 4-5, High = ≥6"
    ) +
    facet_wrap(~category, nrow = 1) +
    theme_minimal()

  ggsave(
    here::here("results", "figures", paste0("test_3_category_proportions_", scenario_lower, ".png")),
    p_cat,
    width = 10,
    height = 6,
    dpi = 300
  )

  # p_cat

  print(cat_prop_long, n = 100)

  # ========================================================================
  # SAVE OUTPUTS
  # ========================================================================
  output_dir <- here::here("results", "objects")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  oracle_data_out <- oracle_data |>
    dplyr::select(-trust_scientists, -trust_scientists_baseline)
  observed_data_out <- observed_data |>
    dplyr::select(-trust_scientists, -trust_scientists_baseline)

  analysis_outputs <- list(
    scenario = scenario_label,
    oracle_data = oracle_data_out,
    observed_data = observed_data_out,
    ipcw_data = dat_weighted,
    attrition_summary = attrition_summary,
    dropout_rates = dropout_rates,
    retention_summary = retention_summary,
    oracle_cat_props = oracle_cat_props,
    complete_case_cat_props = cc_cat_props,
    ipcw_cat_props = ipcw_cat_props,
    amelia_cat_props = amelia_cat_props,
    mice_cat_props = mice_cat_props,
    cat_prop_long = cat_prop_long,
    cat_shift_summary = cat_shift_summary,
    cat_shift_summary_pct = cat_shift_summary_pct,
    oracle_transitions = oracle_transitions,
    observed_transitions = observed_transitions,
    observed_by_group = observed_by_group,
    ordinal_thresholds = ordinal_thresholds,
    pred_oracle = pred_oracle,
    pred_observed = pred_observed,
    pred_ipcw = pred_ipcw,
    continuous_means = continuous_means,
    continuous_change = continuous_change
  )

  saveRDS(
    analysis_outputs,
    file = here::here("results", "objects", paste0("test_3_improved_outputs_", scenario_lower, ".rds"))
  )

  cat("\n\n=== CHECKING DEMOGRAPHIC PREDICTION IN IMPUTED DATA ===\n")
  imp1 <- mice::complete(mids_data, 1)
  imp_model <- lm(trust_science ~ education + age_baseline + gender + ethnicity, data = imp1)
  cat("R-squared of demographics predicting trust in imputed data:", round(summary(imp_model)$r.squared, 3), "\n")
  cat("Coefficients:\n")
  print(round(coef(imp_model), 3))

  analysis_outputs
}

scenario_params <- list(
  mar = list(beta_baseline = -0.2, beta_lag = -0.2, beta_current = 0),
  mnar = list(beta_baseline = -0.2, beta_lag = -0.2, beta_current = -0.25)
)

scenario_results <- list()

for (scenario_name in names(scenario_params)) {
  cat("\n\n=== SCENARIO:", toupper(scenario_name), "===\n")
  dropout_results <- simulate_dropout(
    long_data,
    scenario_name,
    drop_prob_group,
    scenario_params[[scenario_name]]
  )
  print(dropout_results$attrition_summary)

  scenario_outputs <- run_scenario_analysis(
    oracle_data = oracle_data,
    observed_data = dropout_results$observed_data,
    scenario_label = toupper(scenario_name),
    attrition_summary = dropout_results$attrition_summary,
    dropout_rates = dropout_results$dropout_rates,
    retention_summary = dropout_results$retention_summary
  )

  scenario_results[[scenario_name]] <- scenario_outputs
}

saveRDS(
  scenario_results,
  file = here::here("results", "objects", "test_3_improved_outputs.rds")
)

cat("\n\n=== SCENARIO SUMMARY (MEAN TRAJECTORIES) ===\n")
scenario_summary <- bind_rows(lapply(scenario_results, function(res) {
  res$continuous_change |>
    mutate(scenario = res$scenario)
}))
print(scenario_summary)

cat("\n\n=== SCENARIO SUMMARY (CATEGORY SHIFTS, PERCENTAGE POINTS) ===\n")
category_summary <- bind_rows(lapply(scenario_results, function(res) {
  res$cat_shift_summary_pct |>
    mutate(scenario = res$scenario)
}))
print(category_summary)

cat("\n\nAnalysis complete! Check scenario outputs.\n")

# suggested plots for reporting (commented out)
# scenario_results <- readRDS(here::here("results", "objects", "test_3_improved_outputs.rds"))
#
# for (scenario_name in names(scenario_results)) {
#   scenario_data <- scenario_results[[scenario_name]]
#
#   p_mean <- ggplot(
#     scenario_data$continuous_means,
#     aes(x = years, y = mean_trust, color = method)
#   ) +
#     geom_line(size = 1.1) +
#     geom_point(size = 2) +
#     scale_x_continuous(breaks = 0:4, labels = 1:5) +
#     labs(
#       x = "Year of Study",
#       y = "Mean Trust in Science (1-7)",
#       title = paste("Trust in Science: Mean Recovery -", scenario_data$scenario),
#       subtitle = "Year means with retention-driven attrition"
#     ) +
#     theme_minimal()
#
#   p_cat <- ggplot(
#     scenario_data$cat_prop_long,
#     aes(x = years, y = proportion, color = method, group = method)
#   ) +
#     geom_line(size = 1.2) +
#     geom_point(size = 2) +
#     scale_color_manual(values = c(
#       "Oracle" = "black",
#       "Complete Case" = "red",
#       "IPCW" = "blue",
#       "Amelia" = "darkgreen",
#       "MICE" = "orange"
#     )) +
#     labs(
#       x = "Year",
#       y = "Proportion",
#       title = paste("Category Proportions by Method -", scenario_data$scenario),
#       subtitle = "Low = ≤3, Medium = 4-5, High = ≥6"
#     ) +
#     facet_wrap(~category, nrow = 1) +
#     theme_minimal()
#
#   print(p_mean)
#   print(p_cat)
# }
