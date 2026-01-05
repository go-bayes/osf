# test_3_improved.R - Improved simulation with correlated baselines and current-trust dropout
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(MASS) # for mvrnorm
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
  here::i_am("keep/test_3_improved.R"),
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

# create trust groups based on education
participants$trust_group <- case_when(
  participants$education <= 2 ~ "low",
  participants$education <= 5 ~ "medium",
  participants$education >= 6 ~ "high"
)

# generate baseline trust from demographic predictors
cat("\nGenerating baseline trust from demographic predictors...\n")

# create demographic predictors for trust
participants <- participants %>%
  mutate(
    # calculate mean trust from demographics
    trust_science_mean = 4.0 +
      0.3 * education +
      0.01 * (age_baseline - 50) +
      0.2 * (gender == "Female") +
      -0.3 * (ethnicity == "Maori") +
      -0.2 * (ethnicity == "Pacific") +
      0.1 * (ethnicity == "Asian"),
    # trust in scientists slightly lower overall but same predictors
    trust_scientists_mean = 3.8 +
      0.3 * education +
      0.01 * (age_baseline - 50) +
      0.2 * (gender == "Female") +
      -0.3 * (ethnicity == "Maori") +
      -0.2 * (ethnicity == "Pacific") +
      0.1 * (ethnicity == "Asian")
  )

# generate correlated baseline trust scores
# correlation matrix (r = 0.7 - higher correlation)
Sigma <- matrix(c(
  0.5^2, 0.5 * 0.5 * 0.7,
  0.5 * 0.5 * 0.7, 0.5^2
), 2, 2)

# generate for each participant
trust_baselines <- matrix(NA, n_participants, 2)
for (i in 1:n_participants) {
  trust_baselines[i, ] <- mvrnorm(1,
    mu = c(
      participants$trust_science_mean[i],
      participants$trust_scientists_mean[i]
    ),
    Sigma = Sigma
  )
}

# assign and bound to 1-7 scale
participants$trust_science_baseline <- pmax(1, pmin(7, trust_baselines[, 1]))
participants$trust_scientists_baseline <- pmax(1, pmin(7, trust_baselines[, 2]))

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

# check demographic effects
cat("\nDemographic effects on trust:\n")
demo_model <- lm(trust_science_baseline ~ education + age_baseline + gender + ethnicity,
  data = participants
)
print(round(coef(demo_model), 3))

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

# generate trust trajectories using LINEAR SLOPES
cat("Generating trust outcomes with linear slopes...\n")

# define slopes for each group - moderate for realistic patterns
slopes <- c(low = -0.3, medium = 0.0, high = 0.2)

long_data <- long_data %>%
  mutate(
    # trust in science with linear trend
    trust_science = trust_science_baseline +
      slopes[trust_group] * years +
      ifelse(trust_group == "medium", 0, rnorm(n(), 0, 0.10)), # keep medium stable
    # trust in scientists with similar linear trend
    trust_scientists = trust_scientists_baseline +
      slopes[trust_group] * years +
      ifelse(trust_group == "medium", 0, rnorm(n(), 0, 0.10)) # keep medium stable
  ) |>
  # bound to 1-7 scale
  mutate(across(
    c(trust_science, trust_scientists),
    ~ pmin(7, pmax(1, .))
  ))

# add post-stratification weights (before missingness)
cat("\nAdding post-stratification weights...\n")

# realistic post-stratification weights to correct for NZAVS sampling design
long_data$weights <- 1

# age effects
long_data$weights[long_data$age_baseline < 35] <- long_data$weights[long_data$age_baseline < 35] * 1.15
long_data$weights[long_data$age_baseline >= 35 & long_data$age_baseline < 45] <- long_data$weights[long_data$age_baseline >= 35 & long_data$age_baseline < 45] * 1.05
long_data$weights[long_data$age_baseline >= 55] <- long_data$weights[long_data$age_baseline >= 55] * 0.85

# gender effects
long_data$weights[long_data$gender == "Female"] <- long_data$weights[long_data$gender == "Female"] * 0.90
long_data$weights[long_data$gender == "Male"] <- long_data$weights[long_data$gender == "Male"] * 1.10

# ethnicity effects
long_data$weights[long_data$ethnicity == "Maori"] <- long_data$weights[long_data$ethnicity == "Maori"] * 0.95
long_data$weights[long_data$ethnicity == "Pacific"] <- long_data$weights[long_data$ethnicity == "Pacific"] * 1.20
long_data$weights[long_data$ethnicity == "Asian"] <- long_data$weights[long_data$ethnicity == "Asian"] * 1.15
long_data$weights[long_data$ethnicity == "Other"] <- long_data$weights[long_data$ethnicity == "Other"] * 1.05

# interaction: young males
long_data$weights[long_data$age_baseline < 35 & long_data$gender == "Male"] <-
  long_data$weights[long_data$age_baseline < 35 & long_data$gender == "Male"] * 1.05

# normalize weights to average 1.0
long_data$weights <- long_data$weights / mean(long_data$weights, na.rm = TRUE)

# save oracle data (before missingness)
cat("\nSaving oracle data...\n")
oracle_data <- long_data %>%
  arrange(id, years)

# generate dropout with calibrated group retention targets
cat("\nGenerating dropout with calibrated group retention targets...\n")

target_retention <- c(high = 0.90, medium = 0.70, low = 0.30)
target_overall_attrition <- 0.275
drop_years <- 1:(n_waves - 1)

solve_group_intercept <- function(target, time_slope) {
  retention_fn <- function(intercept) {
    prod(1 - plogis(intercept + time_slope * drop_years)) - target
  }
  bracket_interval <- function(fn, lower = -10, upper = 10, step = 2, max_expand = 8) {
    f_lower <- fn(lower)
    f_upper <- fn(upper)
    expand_count <- 0
    while (f_lower * f_upper > 0 && expand_count < max_expand) {
      lower <- lower - step
      upper <- upper + step
      f_lower <- fn(lower)
      f_upper <- fn(upper)
      expand_count <- expand_count + 1
    }
    list(lower = lower, upper = upper, f_lower = f_lower, f_upper = f_upper)
  }

  bracket <- bracket_interval(retention_fn)
  if (bracket$f_lower * bracket$f_upper > 0) {
    stop("Failed to bracket retention target for intercept.", call. = FALSE)
  }
  uniroot(retention_fn, interval = c(bracket$lower, bracket$upper))$root
}

overall_attrition_for_slope <- function(time_slope, group_sizes) {
  group_intercepts <- c(
    high = solve_group_intercept(target_retention["high"], time_slope),
    medium = solve_group_intercept(target_retention["medium"], time_slope),
    low = solve_group_intercept(target_retention["low"], time_slope)
  )

  overall_attrition <- numeric(length(drop_years))
  group_survival <- rep(1, length(group_sizes))
  names(group_survival) <- names(group_sizes)

  for (i in seq_along(drop_years)) {
    year_value <- drop_years[i]
    hazards <- plogis(group_intercepts + time_slope * year_value)
    expected_events <- sum(group_sizes * group_survival * hazards)
    expected_at_risk <- sum(group_sizes * group_survival)
    overall_attrition[i] <- expected_events / expected_at_risk
    group_survival <- group_survival * (1 - hazards)
  }

  mean(overall_attrition)
}

group_sizes <- participants |>
  count(trust_group) |>
  tibble::deframe()

attrition_gap <- function(time_slope) {
  overall_attrition_for_slope(time_slope, group_sizes) - target_overall_attrition
}

time_slope_bounds <- c(-1, 2)
attrition_at_bounds <- sapply(time_slope_bounds, attrition_gap)

if (attrition_at_bounds[1] * attrition_at_bounds[2] < 0) {
  time_slope <- uniroot(attrition_gap, interval = time_slope_bounds)$root
} else {
  time_slope <- optimize(
    function(x) abs(attrition_gap(x)),
    interval = time_slope_bounds
  )$minimum
  cat("Warning: target overall attrition not bracketed; using best-fit time slope.\n")
}

group_intercepts <- c(
  high = solve_group_intercept(target_retention["high"], time_slope),
  medium = solve_group_intercept(target_retention["medium"], time_slope),
  low = solve_group_intercept(target_retention["low"], time_slope)
)

long_data <- long_data |>
  arrange(id, years) |>
  group_by(id) |>
  mutate(
    drop_prob = case_when(
      years == 0 ~ 0,
      trust_group == "high" ~ plogis(group_intercepts["high"] + time_slope * years),
      trust_group == "medium" ~ plogis(group_intercepts["medium"] + time_slope * years),
      trust_group == "low" ~ plogis(group_intercepts["low"] + time_slope * years),
      TRUE ~ 0
    ),
    dropped = cummax(rbinom(n(), 1, drop_prob))
  ) |>
  ungroup()

cat("Calibrated time slope:", round(time_slope, 3), "\n")
cat("Target retention (final wave):\n")
print(target_retention)

# apply missingness to time-varying variables
cat("\nApplying missingness to time-varying variables...\n")
long_data <- long_data %>%
  mutate(
    trust_science_obs = ifelse(dropped == 1, NA, trust_science),
    trust_scientists_obs = ifelse(dropped == 1, NA, trust_scientists)
  )

# prepare observed data
observed_data <- long_data %>%
  mutate(
    trust_science = trust_science_obs,
    trust_scientists = trust_scientists_obs
  ) %>%
  dplyr::select(-trust_science_obs, -trust_scientists_obs) %>%
  arrange(id, years)

# check attrition patterns
cat("\nAttrition by group and year:\n")
attrition_summary <- long_data %>%
  group_by(trust_group, years) %>%
  summarise(
    n_total = n(),
    n_dropped = sum(dropped == 1),
    pct_dropped = round(100 * n_dropped / n_total, 1),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = years, values_from = pct_dropped, names_prefix = "Year_")

print(attrition_summary)

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

# compare oracle vs observed
cat("\n\n=== COMPARING ORACLE VS OBSERVED ===\n")

# fit models to oracle data
cat("\nFitting model to oracle data...\n")
mod_oracle <- geepack::geeglm(
  trust_science ~ ns(years, 3),
  id = id,
  data = oracle_data,
  corstr = "exchangeable"
)

pred_oracle <- predict_response(mod_oracle, "years[all]")
cat("Oracle trajectory:\n")
print(round(pred_oracle$predicted, 3))

# fit models to observed data (complete cases only)
cat("\nFitting model to observed data (complete cases)...\n")
observed_complete <- observed_data[!is.na(observed_data$trust_science), ] %>%
  arrange(id, years)

mod_observed <- geepack::geeglm(
  trust_science ~ ns(years, 3),
  id = id,
  weights = weights,
  data = observed_complete,
  corstr = "exchangeable"
)

pred_observed <- predict_response(mod_observed, "years[all]")
cat("Observed trajectory:\n")
print(round(pred_observed$predicted, 3))

# ========================================================================
# IPCW ANALYSIS
# ========================================================================
cat("\n=== RUNNING IPCW ANALYSIS ===\n")

dat_ipcw <- observed_data |>
  arrange(id, years) |>
  group_by(id) |>
  mutate(
    trust_science_lag1 = lag(trust_science, 1),
    trust_scientists_lag1 = lag(trust_scientists, 1),
    observed = as.numeric(!is.na(trust_science)),
    at_risk = lag(observed, default = 1) == 1
  ) |>
  ungroup()

dat_at_risk <- dat_ipcw |>
  dplyr::filter(at_risk == 1 & years > 0)

baseline_data <- dat_ipcw |>
  dplyr::filter(years == 0) |>
  dplyr::select(id, age_baseline, gender, education) |>
  rename(
    age_b = age_baseline,
    gender_b = gender,
    education_b = education
  )

dat_model <- dat_at_risk |>
  left_join(baseline_data, by = "id")

dropout_model <- glm(
  observed ~ trust_science_lag1 + trust_scientists_lag1 +
    age_b + gender_b + education_b + factor(years),
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

dat_weighted <- dat_ipcw |>
  left_join(dat_weights, by = c("id", "years")) |>
  mutate(
    ipcw = ifelse(years == 0, 1, weight_cumulative),
    ipcw = ifelse(is.na(ipcw), 0, ipcw),
    combined_weight = ipcw * weights
  )

ipcw_complete <- dat_weighted |>
  dplyr::filter(observed == 1)

mod_ipcw <- geepack::geeglm(
  trust_science ~ ns(years, 3),
  id = id,
  weights = combined_weight,
  data = ipcw_complete,
  corstr = "exchangeable"
)

pred_ipcw <- predict_response(mod_ipcw, "years[all]")
cat("IPCW trajectory:\n")
print(round(pred_ipcw$predicted, 3))

# create visualizations
library(patchwork)
library(MASS) # for polr

cat("\n\n=== CREATING CONTINUOUS OUTCOME PLOTS ===\n")

# oracle plot
oracle_plot_science <- plot(
  pred_oracle,
  show_ci = TRUE,
  show_data = TRUE,
  ci_style = "dash",
  colors = "viridis",
  jitter = .5,
  dot_alpha = .01,
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "darkgreen",
    size = 3,
    alpha = 1
  ) +
  scale_x_continuous(breaks = 0:4, labels = 1:5) +
  labs(
    x = "Year of Study",
    y = "Trust in Science (1-7)",
    title = "Trust in Science - Oracle (Ground Truth)"
  ) +
  theme_bw()

# observed plot
observed_plot_science <- plot(
  pred_observed,
  show_ci = TRUE,
  show_data = TRUE,
  ci_style = "dash",
  colors = "metro",
  jitter = .5,
  dot_alpha = .01,
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "red",
    size = 3,
    alpha = 1
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
    title = "Selection Bias in Trust Trajectories: Continuous Outcomes",
    subtitle = "Low trust individuals drop out, creating artificial stability"
  )

# print(combined_continuous)
ggsave(
  here::here("results", "figures", "test_3_continuous_comparison.png"),
  combined_continuous,
  width = 12,
  height = 6,
  dpi = 300
)

# summary statistics
cat("\n\nSUMMARY:\n")
cat(
  "Oracle: Year 1 =", round(pred_oracle$predicted[1], 3),
  "Year 5 =", round(pred_oracle$predicted[5], 3),
  "Change =", round(pred_oracle$predicted[5] - pred_oracle$predicted[1], 3), "\n"
)
cat(
  "Observed: Year 1 =", round(pred_observed$predicted[1], 3),
  "Year 5 =", round(pred_observed$predicted[5], 3),
  "Change =", round(pred_observed$predicted[5] - pred_observed$predicted[1], 3), "\n"
)

# check who remains by year 5
cat("\n\nWho remains by year 5:\n")
year4_complete <- observed_complete %>%
  dplyr::filter(years == 4) %>%
  group_by(trust_group) %>%
  summarise(n = n(), mean_trust = round(mean(trust_science), 2), .groups = "drop")

year4_oracle <- oracle_data %>%
  dplyr::filter(years == 4) %>%
  group_by(trust_group) %>%
  summarise(n_oracle = n(), .groups = "drop")

year4_summary <- left_join(year4_complete, year4_oracle) %>%
  mutate(retention_pct = round(100 * n / n_oracle, 1))

print(year4_summary)

# show overall missingness pattern
cat("\n\nMissingness pattern in observed data:\n")
missing_summary <- observed_data %>%
  group_by(years) %>%
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
# prepare data for Amelia
library(Amelia)
library(mice)
head(observed_data)
cat("\nVariables in observed_data:", paste(names(observed_data), collapse = ", "), "\n\n")

# remove only simulation artifacts, keep demographic predictors
amelia_data <- observed_data %>%
  dplyr::select(
    -trust_group, -drop_prob, -dropped,
    -trust_science_baseline, -trust_scientists_baseline,
    -trust_science_mean, -trust_scientists_mean
  ) %>%
  arrange(id, years)

# convert wave to character for Amelia
amelia_data$wave <- as.character(amelia_data$wave)

cat("Variables in amelia_data:", paste(names(amelia_data), collapse = ", "), "\n\n")

# id variables (removed from imputation model)
id_vars <- c("wave", "weights")

# nominal variables
nominal_vars <- c("gender", "ethnicity")

# set up bounds for trust variables (1-7 scale)
bounds_matrix <- matrix(c(
  which(names(amelia_data) == "trust_science"), 1, 7,
  which(names(amelia_data) == "trust_scientists"), 1, 7
), nrow = 2, ncol = 3, byrow = TRUE)

# run Amelia
head(amelia_data)
cat("Running Amelia with m=10 imputations...\n")
amelia_out <- amelia(
  amelia_data,
  m = 10,
  idvars = id_vars,
  noms = nominal_vars,
  bounds = bounds_matrix,
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

# source functions
source(here::here("code/functions/margot_amelia_to_mice_fixed.R"))
source(here::here("code/functions/fix_mids_factors.R"))

# convert to MICE
mids_data <- margot_amelia_to_mice_fixed(
  amelia_out,
  original_data = amelia_data,
  verbose = TRUE
)

# add factor variables
mids_data <- fix_mids_factors(mids_data, c("trust_science_factor", "trust_scientists_factor"))

# ========================================================================
# RUN MICE IMPUTATION (WIDE FORMAT) FOR COMPARISON
# ========================================================================
cat("\n=== RUNNING MICE IMPUTATION (WIDE FORMAT) ===\n")

n_imputations_mice <- 5

mice_wide_data <- observed_data |>
  dplyr::select(
    id, years, trust_science, trust_scientists,
    age_baseline, gender, education, weights
  ) |>
  pivot_wider(
    id_cols = c(id, age_baseline, gender, education, weights),
    names_from = years,
    values_from = c(trust_science, trust_scientists),
    names_sep = "_"
  )

mice_obj <- mice::mice(
  mice_wide_data,
  m = n_imputations_mice,
  method = "pmm",
  printFlag = FALSE,
  seed = 2025
)

# fit GEE models for imputed data
cat("\n\n=== FITTING GEE MODELS TO IMPUTED DATA ===\n")

gee_science_imputed <- lapply(1:10, function(i) {
  dat_imp <- mice::complete(mids_data, i) %>%
    arrange(id, years)
  geepack::geeglm(
    trust_science ~ ns(years, 3),
    id = id,
    weights = weights,
    data = dat_imp,
    corstr = "exchangeable"
  )
})

# get predictions
pred_science_imputed <- lapply(gee_science_imputed, function(m) {
  predict_response(m, "years[all]")
})

# pool predictions
pooled_science <- pool_predictions(pred_science_imputed)
pooled_science
# compare results
cat("\nTrust in Science Results:\n")
cat(
  "Oracle:   Year 0 =", round(pred_oracle$predicted[1], 3),
  "Year 4 =", round(pred_oracle$predicted[5], 3),
  "Change =", round(pred_oracle$predicted[5] - pred_oracle$predicted[1], 3), "\n"
)
cat(
  "Observed: Year 0 =", round(pred_observed$predicted[1], 3),
  "Year 4 =", round(pred_observed$predicted[5], 3),
  "Change =", round(pred_observed$predicted[5] - pred_observed$predicted[1], 3), "\n"
)
cat(
  "IPCW:     Year 0 =", round(pred_ipcw$predicted[1], 3),
  "Year 4 =", round(pred_ipcw$predicted[5], 3),
  "Change =", round(pred_ipcw$predicted[5] - pred_ipcw$predicted[1], 3), "\n"
)
cat(
  "Imputed:  Year 0 =", round(pooled_science$predicted[1], 3),
  "Year 4 =", round(pooled_science$predicted[5], 3),
  "Change =", round(pooled_science$predicted[5] - pooled_science$predicted[1], 3), "\n"
)

# create comparison plot
p_gee_comparison <- plot(pred_oracle) +
  ggtitle("Oracle (Ground Truth)") +
  ylim(4.5, 5.5) +
  plot(pred_ipcw) +
  ggtitle("IPCW") +
  ylim(4.5, 5.5) +
  plot(pooled_science) +
  ggtitle("Imputed") +
  ylim(4.5, 5.5) +
  plot(pred_observed) +
  ggtitle("Observed (Complete Cases)") +
  ylim(4.5, 5.5) +
  plot_annotation(
    title = "Trust in Science: Comparing Oracle, IPCW, Imputed, and Observed",
    subtitle = "GEE models with natural splines"
  )

# print(p_gee_comparison)
ggsave(
  here::here("results", "figures", "test_3_imputation_comparison.png"),
  p_gee_comparison,
  width = 20,
  height = 5,
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

mice_cat_props_list <- lapply(seq_len(mids_data$m), function(i) {
  mice::complete(mice_obj, i) |>
    pivot_longer(
      cols = starts_with("trust_science_"),
      names_to = "year",
      values_to = "trust_science",
      names_prefix = "trust_science_"
    ) |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    mutate(years = as.numeric(year)) |>
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
    pivot_longer(
      cols = starts_with("trust_science_"),
      names_to = "year",
      values_to = "trust_science",
      names_prefix = "trust_science_"
    ) |>
    mutate(
      years = as.numeric(year),
      trust_cat = create_trust_categories(trust_science)
    ) |>
    fit_polr_thresholds()
}) |>
  pool_thresholds("MICE")

ordinal_thresholds <- bind_rows(
  oracle_thresholds,
  cc_thresholds,
  ipcw_thresholds,
  amelia_thresholds,
  mice_thresholds
)

cat("\nOrdinal model thresholds (pooled where applicable):\n")
# print(ordinal_thresholds)

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
    )
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
  group_by(years, trust_group, trust_cat) |>
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  ungroup() |>
  group_by(years, trust_group) |>
  mutate(prop = n / sum(n)) |>
  ungroup()

cat("\nObserved category proportions by trust_group and year:\n")
print(observed_by_group)

p_cat <- ggplot(
  cat_prop_long,
  aes(x = years, y = proportion, color = method, linetype = category)
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
  scale_linetype_manual(values = c(
    "Low" = "solid",
    "Medium" = "dotdash",
    "High" = "dashed"
  )) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Category Proportions by Method",
    subtitle = "Low = ≤3, Medium = 4-5, High = ≥6"
  ) +
  theme_minimal()

ggsave(
  here::here("results", "figures", "test_3_category_proportions.png"),
  p_cat,
  width = 10,
  height = 6,
  dpi = 300
)
p_cat
# ========================================================================
# SAVE OUTPUTS
# ========================================================================
output_dir <- here::here("results", "objects")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

analysis_outputs <- list(
  oracle_data = oracle_data,
  observed_data = observed_data,
  ipcw_data = dat_weighted,
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
  pooled_science = pooled_science
)

saveRDS(
  analysis_outputs,
  file = here::here("results", "objects", "test_3_improved_outputs.rds")
)
cat("\n\nAnalysis complete! Check if imputation better tracks the oracle data.\n")

# check demographic prediction in imputed data
cat("\n\n=== CHECKING DEMOGRAPHIC PREDICTION IN IMPUTED DATA ===\n")
imp1 <- mice::complete(mids_data, 1)
imp_model <- lm(trust_science ~ education + age_baseline + gender + ethnicity, data = imp1)
cat("R-squared of demographics predicting trust in imputed data:", round(summary(imp_model)$r.squared, 3), "\n")
cat("Coefficients:\n")
print(round(coef(imp_model), 3))
