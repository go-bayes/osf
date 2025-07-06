# test_2.R - Three-group simulation for selection bias demonstration
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(splines)
library(ggeffects)
library(geepack)

# parameters
n_participants <- 40000  # larger sample for clearer patterns
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
                  replace = TRUE, prob = c(0.62, 0.38)),
  # ethnicity - slightly oversample Māori, undersample Pacific/Asian
  ethnicity = sample(c("NZ European", "Maori", "Pacific", "Asian", "Other"),
                     n_participants, replace = TRUE,
                     prob = c(0.68, 0.17, 0.06, 0.10, 0.04)),
  # education: 1-7 scale - key predictor of trust group
  education = sample(1:7, n_participants, replace = TRUE,
                     prob = c(0.05, 0.10, 0.20, 0.25, 0.20, 0.15, 0.05))
)

# create trust groups based on education
# education 1-2 -> low trust (starts around 3)
# education 3-5 -> medium trust (starts around 4.5)
# education 6-7 -> high trust (starts around 6)
participants$trust_group <- case_when(
  participants$education <= 2 ~ "low",
  participants$education <= 5 ~ "medium",
  participants$education >= 6 ~ "high"
)

# set baseline trust values to match NZAVS distribution
# trust science: ~8% low, ~31% med, ~60% high
# trust scientists: ~12% low, ~34% med, ~54% high
participants$trust_science_baseline <- case_when(
  participants$trust_group == "low" ~ 2.9,      # aim for ~8% < 3
  participants$trust_group == "medium" ~ 5.15,  # slightly lower to get more in med range
  participants$trust_group == "high" ~ 6.1      # all will be > 5 (high category)
)

participants$trust_scientists_baseline <- case_when(
  participants$trust_group == "low" ~ 2.7,      # aim for ~12% < 3
  participants$trust_group == "medium" ~ 4.95,  # slightly lower than 5 for better split
  participants$trust_group == "high" ~ 5.9      # all will be > 5 (high category)
)

# add more variation within groups for realistic distribution
participants$trust_science_baseline <- participants$trust_science_baseline +
  rnorm(n_participants, 0, 0.5)
participants$trust_scientists_baseline <- participants$trust_scientists_baseline +
  rnorm(n_participants, 0, 0.5)

# bound to 1-7 scale
participants$trust_science_baseline <- pmax(1, pmin(7, participants$trust_science_baseline))
participants$trust_scientists_baseline <- pmax(1, pmin(7, participants$trust_scientists_baseline))

# check distribution
cat("\nBaseline trust by group:\n")
aggregate(trust_science_baseline ~ trust_group, data = participants,
          function(x) round(c(mean = mean(x), n = length(x)), 2))

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
                         levels = baseline_year:(baseline_year + n_waves - 1))

# generate trust trajectories by group
cat("Generating trust outcomes with group-specific trajectories...\n")

# trust in science trajectories
long_data$trust_science <- with(long_data, {
  # start with baseline
  trust <- trust_science_baseline

  # year 1 (COVID) effects by group
  trust[years == 1 & trust_group == "high"] <- trust[years == 1 & trust_group == "high"] + 0.2
  trust[years == 1 & trust_group == "medium"] <- trust[years == 1 & trust_group == "medium"] + 0.05
  trust[years == 1 & trust_group == "low"] <- trust[years == 1 & trust_group == "low"] - 0.3

  # year 2 effects by group
  trust[years == 2 & trust_group == "high"] <- trust[years == 2 & trust_group == "high"] + 0.3
  trust[years == 2 & trust_group == "medium"] <- trust[years == 2 & trust_group == "medium"] + 0.05
  trust[years == 2 & trust_group == "low"] <- trust[years == 2 & trust_group == "low"] - 0.5

  # year 3 effects by group
  trust[years == 3 & trust_group == "high"] <- trust[years == 3 & trust_group == "high"] + 0.35
  trust[years == 3 & trust_group == "medium"] <- trust[years == 3 & trust_group == "medium"] + 0.0
  trust[years == 3 & trust_group == "low"] <- trust[years == 3 & trust_group == "low"] - 0.7

  # year 4 effects by group
  trust[years == 4 & trust_group == "high"] <- trust[years == 4 & trust_group == "high"] + 0.4
  trust[years == 4 & trust_group == "medium"] <- trust[years == 4 & trust_group == "medium"] + 0.0
  trust[years == 4 & trust_group == "low"] <- trust[years == 4 & trust_group == "low"] - 0.9

  # add measurement error
  trust + rnorm(length(trust), 0, 0.15)
})

# trust in scientists trajectories (similar pattern)
long_data$trust_scientists <- with(long_data, {
  # start with baseline
  trust <- trust_scientists_baseline

  # year 1 (COVID) effects by group
  trust[years == 1 & trust_group == "high"] <- trust[years == 1 & trust_group == "high"] + 0.25
  trust[years == 1 & trust_group == "medium"] <- trust[years == 1 & trust_group == "medium"] + 0.1
  trust[years == 1 & trust_group == "low"] <- trust[years == 1 & trust_group == "low"] - 0.25

  # year 2 effects by group
  trust[years == 2 & trust_group == "high"] <- trust[years == 2 & trust_group == "high"] + 0.35
  trust[years == 2 & trust_group == "medium"] <- trust[years == 2 & trust_group == "medium"] + 0.1
  trust[years == 2 & trust_group == "low"] <- trust[years == 2 & trust_group == "low"] - 0.45

  # year 3 effects by group
  trust[years == 3 & trust_group == "high"] <- trust[years == 3 & trust_group == "high"] + 0.4
  trust[years == 3 & trust_group == "medium"] <- trust[years == 3 & trust_group == "medium"] + 0.05
  trust[years == 3 & trust_group == "low"] <- trust[years == 3 & trust_group == "low"] - 0.65

  # year 4 effects by group
  trust[years == 4 & trust_group == "high"] <- trust[years == 4 & trust_group == "high"] + 0.45
  trust[years == 4 & trust_group == "medium"] <- trust[years == 4 & trust_group == "medium"] + 0.05
  trust[years == 4 & trust_group == "low"] <- trust[years == 4 & trust_group == "low"] - 0.85

  # add measurement error
  trust + rnorm(length(trust), 0, 0.15)
})

# bound to 1-7 scale
long_data$trust_science <- pmax(1, pmin(7, long_data$trust_science))
long_data$trust_scientists <- pmax(1, pmin(7, long_data$trust_scientists))

# add post-stratification weights (before missingness)
cat("\nAdding post-stratification weights...\n")

# realistic post-stratification weights to correct for NZAVS sampling design
# base weight
long_data$weights <- 1

# age effects: young people are undersampled, older people oversampled
long_data$weights[long_data$age_baseline < 35] <- long_data$weights[long_data$age_baseline < 35] * 1.15
long_data$weights[long_data$age_baseline >= 35 & long_data$age_baseline < 45] <- long_data$weights[long_data$age_baseline >= 35 & long_data$age_baseline < 45] * 1.05
long_data$weights[long_data$age_baseline >= 55] <- long_data$weights[long_data$age_baseline >= 55] * 0.85

# gender effects: women are oversampled, men are undersampled
long_data$weights[long_data$gender == "Female"] <- long_data$weights[long_data$gender == "Female"] * 0.90
long_data$weights[long_data$gender == "Male"] <- long_data$weights[long_data$gender == "Male"] * 1.10

# ethnicity effects: Māori slightly oversampled, Pacific/Asian undersampled
long_data$weights[long_data$ethnicity == "Maori"] <- long_data$weights[long_data$ethnicity == "Maori"] * 0.95
long_data$weights[long_data$ethnicity == "Pacific"] <- long_data$weights[long_data$ethnicity == "Pacific"] * 1.20
long_data$weights[long_data$ethnicity == "Asian"] <- long_data$weights[long_data$ethnicity == "Asian"] * 1.15
long_data$weights[long_data$ethnicity == "Other"] <- long_data$weights[long_data$ethnicity == "Other"] * 1.05

# interaction: young males get extra weight (most underrepresented)
long_data$weights[long_data$age_baseline < 35 & long_data$gender == "Male"] <- long_data$weights[long_data$age_baseline < 35 & long_data$gender == "Male"] * 1.05

# normalize weights to average 1.0
long_data$weights <- long_data$weights / mean(long_data$weights, na.rm = TRUE)

# save oracle data (before missingness) - includes weights
cat("\nSaving oracle data...\n")
oracle_data <- long_data %>%
  arrange(id, years)  # ensure proper sorting for GEE models

# generate missing data patterns
cat("\nAdding missing data patterns...\n")

# baseline probability of missingness by group
base_missing_prob <- case_when(
  long_data$trust_group == "low" ~ 0.02,    # slightly higher for low trust
  long_data$trust_group == "medium" ~ 0.01,  # moderate for medium
  long_data$trust_group == "high" ~ 0.005    # minimal for high
)

# time effect (increases over waves) - reduced to achieve ~20% annual attrition
time_effect <- 0.04 * long_data$years

# COVID-specific attrition (year 1) - much more moderate
covid_effect <- case_when(
  long_data$years == 1 & long_data$trust_group == "low" ~ 0.25,    # 25% extra dropout
  long_data$years == 1 & long_data$trust_group == "medium" ~ 0.05,  # 5% extra
  long_data$years == 1 & long_data$trust_group == "high" ~ 0.0,     # no extra
  TRUE ~ 0
)

# calculate total missing probability
missing_prob <- pmin(0.95, base_missing_prob + time_effect + covid_effect)

# generate missing indicators
long_data$missing <- rbinom(nrow(long_data), 1, missing_prob)

# enforce monotone dropout
for (i in 2:n_waves) {
  year_t <- long_data$years == (i - 1) & long_data$missing == 1
  year_t1 <- long_data$years == i
  same_id <- long_data$id %in% long_data$id[year_t]
  long_data$missing[year_t1 & same_id] <- 1
}

# apply missingness: when someone is missing, ALL their data for that wave is missing
# (except time-invariant baseline characteristics like age_baseline, gender, ethnicity, education)
# this is realistic for survey dropout - you lose the whole response, not just some variables

# first, handle baseline missingness (keep minimal, max 5%)
baseline_missing <- long_data$years == 0 & long_data$missing == 1
n_baseline_missing <- sum(baseline_missing)
max_baseline_missing <- round(0.05 * sum(long_data$years == 0))

if (n_baseline_missing > max_baseline_missing) {
  # randomly keep some baseline observations
  keep_idx <- sample(which(baseline_missing), n_baseline_missing - max_baseline_missing)
  long_data$missing[keep_idx] <- 0
}

# apply missingness to all time-varying variables when missing == 1
# time-varying variables are: trust_science, trust_scientists, and any others added in future
time_varying_vars <- c("trust_science", "trust_scientists")

# for all waves (including baseline if still marked as missing)
for (var in time_varying_vars) {
  long_data[[var]][long_data$missing == 1] <- NA
}

# ensure joint missingness in post-baseline waves:
# if any time-varying variable is NA, set ALL time-varying variables to NA
# this ensures realistic survey dropout patterns
idx_any_missing <- FALSE
for (var in time_varying_vars) {
  idx_any_missing <- idx_any_missing | is.na(long_data[[var]])
}

# apply joint missingness
for (var in time_varying_vars) {
  long_data[[var]][idx_any_missing] <- NA
}

# weights remain constant for each individual throughout the study
# they represent the baseline sampling probability, not response probability
# so we do NOT set them to NA when someone drops out
# (weights were already calculated above before missingness)

# check attrition patterns
cat("\nAttrition by group and year:\n")
attrition_summary <- long_data %>%
  group_by(trust_group, years) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is.na(trust_science)),
    pct_missing = round(100 * n_missing / n_total, 1)
  ) %>%
  pivot_wider(names_from = years, values_from = pct_missing, names_prefix = "Year_")

print(attrition_summary)

# function to compute categorical summaries
compute_cat_summary <- function(data) {
  data %>%
    mutate(
      trust_science_factor = factor(
        case_when(
          is.na(trust_science) ~ NA_character_,
          trust_science <= 3 ~ "low",
          trust_science <= 5 ~ "med",
          TRUE ~ "high"
        ),
        levels = c("low", "med", "high"),
        ordered = TRUE
      ),
      trust_scientists_factor = factor(
        case_when(
          is.na(trust_scientists) ~ NA_character_,
          trust_scientists <= 3 ~ "low",
          trust_scientists <= 5 ~ "med",
          TRUE ~ "high"
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
  wave = years,
  data = oracle_data,
  corstr = "exchangeable"
)

pred_oracle <- predict_response(mod_oracle, "years[all]")
cat("Oracle trajectory:\n")
print(round(pred_oracle$predicted, 3))

# prepare observed data (keep NAs)
observed_data <- long_data %>%
  arrange(id, years)  # ensure proper sorting

# fit models to observed data (complete cases only for model fitting)
cat("\nFitting model to observed data (complete cases)...\n")
observed_complete <- long_data[!is.na(long_data$trust_science), ] %>%
  arrange(id, years)  # ensure proper sorting for GEE models
# geepack::gee
mod_observed <- geepack::geeglm(
  trust_science ~ ns(years, 3),
  id = id,
  wave = years,
  data = observed_complete,
  corstr = "exchangeable"
)

pred_observed <- predict_response(mod_observed, "years[all]")
cat("Observed trajectory:\n")
print(round(pred_observed$predicted, 3))

# create visualizations using ggeffects plot functions
library(patchwork)
library(MASS)  # for polr

# ========================================================================
# CONTINUOUS OUTCOMES - GEE MODELS
# ========================================================================
cat("\n\n=== CREATING CONTINUOUS OUTCOME PLOTS ===\n")

# oracle plot - trust in science
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

# observed plot - trust in science
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

# combined continuous plot
combined_continuous <- oracle_plot_science + observed_plot_science +
  plot_annotation(
    title = "Selection Bias in Trust Trajectories: Continuous Outcomes",
    subtitle = "Low trust individuals drop out, creating artificial stability"
  )

print(combined_continuous)
ggsave("results/figures/test_2_continuous_comparison.png", combined_continuous,
       width = 12, height = 6, dpi = 300)

# ========================================================================
# CATEGORICAL OUTCOMES - PROPORTIONAL ODDS MODELS
# ========================================================================
cat("\n\n=== FITTING AND PLOTTING CATEGORICAL MODELS ===\n")

# add categorical variables to datasets
oracle_cat <- compute_cat_summary(oracle_data)
observed_complete_cat <- compute_cat_summary(observed_complete)

# fit proportional odds models
cat("\nFitting proportional odds models...\n")

# oracle
polr_oracle <- MASS::polr(
  trust_science_factor ~ ns(years, 3),
  data = oracle_cat,
  Hess = TRUE
)

pred_polr_oracle <- predict_response(
  polr_oracle,
  "years[all]",
  margin = "marginalmeans"
)

# observed
polr_observed <- MASS::polr(
  trust_science_factor ~ ns(years, 3),
  data = observed_complete_cat,
  Hess = TRUE
)

pred_polr_observed <- predict_response(
  polr_observed,
  "years[all]",
  margin = "marginalmeans"
)

# create categorical plots
oracle_plot_cat <- plot(
  pred_polr_oracle,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "viridis",
  limits = c(0, 1)
) +
  scale_x_continuous(breaks = 0:4, labels = 1:5) +
  labs(
    x = "Year of Study",
    y = "Predicted Probability",
    title = "Trust Categories - Oracle (Ground Truth)"
  ) +
  theme_bw()

observed_plot_cat <- plot(
  pred_polr_observed,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "metro",
  limits = c(0, 1)
) +
  scale_x_continuous(breaks = 0:4, labels = 1:5) +
  labs(
    x = "Year of Study",
    y = "Predicted Probability",
    title = "Trust Categories - Observed (Survivors)"
  ) +
  theme_bw()

# combined categorical plot
combined_categorical <- oracle_plot_cat + observed_plot_cat +
  plot_annotation(
    title = "Selection Bias in Trust Trajectories: Categorical Outcomes",
    subtitle = "Proportional odds models showing probability of low/medium/high trust"
  )

print(combined_categorical)
ggsave("results/figures/test_2_categorical_comparison.png", combined_categorical,
       width = 14, height = 8, dpi = 300)

# ========================================================================
# TRUST IN SCIENTISTS MODELS
# ========================================================================
cat("\n\n=== FITTING TRUST IN SCIENTISTS MODELS ===\n")
# fit GEE for scientists
mod_scientists_oracle <- geepack::geeglm(
  trust_scientists ~ ns(years, 3),
  id = id,
  data = oracle_data,
  corstr = "exchangeable"
)

pred_scientists_oracle <- predict_response(mod_scientists_oracle, "years[all]")

mod_scientists_observed <- geepack::geeglm(
  trust_scientists ~ ns(years, 3),
  id = id,
  data = observed_complete,
  corstr = "exchangeable"
)

pred_scientists_observed <- predict_response(mod_scientists_observed, "years[all]")

# create scientists plots
oracle_plot_scientists <- plot(
  pred_scientists_oracle,
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
    y = "Trust in Scientists (1-7)",
    title = "Trust in Scientists - Oracle (Ground Truth)"
  ) +
  theme_bw()

observed_plot_scientists <- plot(
  pred_scientists_observed,
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
    y = "Trust in Scientists (1-7)",
    title = "Trust in Scientists - Observed (Survivors)"
  ) +
  theme_bw()

# combined scientists plot
combined_scientists <- oracle_plot_scientists + observed_plot_scientists +
  plot_annotation(
    title = "Selection Bias: Trust in Scientists",
    subtitle = "Comparing oracle (truth) vs observed (survivors)"
  )

print(combined_scientists)
ggsave("results/figures/test_2_scientists_comparison.png", combined_scientists,
       width = 12, height = 6, dpi = 300)

# summary statistics
cat("\n\nSUMMARY:\n")
cat("Oracle: Year 1 =", round(pred_oracle$predicted[1], 3),
    "Year 5 =", round(pred_oracle$predicted[5], 3),
    "Change =", round(pred_oracle$predicted[5] - pred_oracle$predicted[1], 3), "\n")
cat("Observed: Year 1 =", round(pred_observed$predicted[1], 3),
    "Year 5 =", round(pred_observed$predicted[5], 3),
    "Change =", round(pred_observed$predicted[5] - pred_observed$predicted[1], 3), "\n")

# check who remains by year 5
cat("\n\nWho remains by year 5:\n")
year4_complete <- observed_complete %>%
  filter(years == 4) %>%
  group_by(trust_group) %>%
  summarise(n = n(), mean_trust = round(mean(trust_science), 2))

year4_oracle <- oracle_data %>%
  filter(years == 4) %>%
  group_by(trust_group) %>%
  summarise(n_oracle = n())

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
    pct_missing = round(100 * n_missing / n_total, 1)
  )
print(missing_summary)

# show categorical distribution
cat("\n\nCategorical distribution of trust in science:\n")

# baseline (year 1)
cat("\nYear 1 (Baseline):\n")
oracle_cat_y1 <- compute_cat_summary(oracle_data %>% filter(years == 0))
observed_cat_y1 <- compute_cat_summary(observed_data %>% filter(years == 0))

cat("Oracle:\n")
print(table(oracle_cat_y1$trust_science_factor))
cat("\nObserved (including NAs):\n")
print(table(observed_cat_y1$trust_science_factor, useNA = "always"))

# year 5
cat("\n\nYear 5:\n")
oracle_cat_y5 <- compute_cat_summary(oracle_data %>% filter(years == 4))
observed_cat_y5 <- compute_cat_summary(observed_data %>% filter(years == 4))

cat("Oracle:\n")
print(table(oracle_cat_y5$trust_science_factor))
cat("\nObserved (including NAs):\n")
print(table(observed_cat_y5$trust_science_factor, useNA = "always"))

# ========================================================================
# VERIFICATION CHECKS
# ========================================================================
cat("\n\n=== VERIFICATION CHECKS ===\n")

# check baseline demographics
baseline_data <- oracle_data %>% filter(years == 0)
cat("\nBaseline Demographics (n =", nrow(baseline_data), "):\n")
cat("\nGender distribution:\n")
print(prop.table(table(baseline_data$gender)))
cat("\nEthnicity distribution:\n")
print(prop.table(table(baseline_data$ethnicity)))
cat("\nMean age:", mean(baseline_data$age_baseline), "\n")

# check weights
cat("\n\nWeight summary:\n")
summary(baseline_data$weights)

# verify weights are constant across time for each individual
weight_check <- oracle_data %>%
  group_by(id) %>%
  summarise(
    weight_sd = sd(weights),
    weight_unique = n_distinct(weights)
  )
cat("\nWeight consistency check:\n")
cat("All weights constant over time:", all(weight_check$weight_unique == 1), "\n")

# check missingness summary
cat("\n\nMissingness Summary:\n")
missing_summary <- observed_data %>%
  group_by(years) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is.na(trust_science)),
    pct_missing = round(100 * n_missing / n_total, 1)
  )
print(missing_summary)

cat("\nOverall retention from baseline to Year 5:",
    round(100 * sum(!is.na(observed_data$trust_science[observed_data$years == 4])) /
          sum(!is.na(observed_data$trust_science[observed_data$years == 0])), 1), "%\n")

# naniar::vis_miss(oracle_data, warn_large_data = FALSE)
# naniar::vis_miss(observed_data, warn_large_data = FALSE)

# ========================================================================
# AMELIA IMPUTATION
# ========================================================================
cat("\n\n=== RUNNING AMELIA IMPUTATION ===\n")
# prepare data for Amelia
library(Amelia)
library(mice)

# remove trust_group (only used for simulation, not for imputation)
amelia_data <- observed_data %>%
  dplyr::select(-trust_group) %>%
  arrange(id, years)

# convert wave to character for Amelia
amelia_data$wave <- as.character(amelia_data$wave)

# id variables (removed from imputation model)
# weights should be included as id variable since they're fixed at baseline
# note: education and age_baseline should NOT be in id_vars - they are predictors
id_vars <- c("wave", "weights")

# nominal variables (categorical to be imputed)
nominal_vars <- c("gender", "ethnicity")

# set up bounds for trust variables (1-7 scale)
bounds_matrix <- matrix(c(
  which(names(amelia_data) == "trust_science"), 1, 7,
  which(names(amelia_data) == "trust_scientists"), 1, 7
), nrow = 2, ncol = 3, byrow = TRUE)
table(amelia_data$education)
head(amelia_out)
# run Amelia
cat("Running Amelia with m=5 imputations...\n")
amelia_out <- amelia(
  amelia_data,
  m = 5,  # 5 imputations for testing
  idvars = id_vars,
  noms = nominal_vars,
  bounds = bounds_matrix,
  # empri = 0.01 * nrow(amelia_data),
  splinetime = 3,
  polytime=3,
  ts = "years",
  cs = "id"
)

cat("Imputation complete!\n")

# ========================================================================
# CONVERT TO MICE FORMAT AND ADD FACTOR VARIABLES
# ========================================================================
cat("\n=== CONVERTING TO MICE FORMAT ===\n")

# source the conversion function
source(here::here("code/functions/margot_amelia_to_mice_fixed.R"))

# convert to MICE first
cat("\nConverting Amelia to MICE format...\n")
mids_data <- margot_amelia_to_mice_fixed(
  amelia_out,
  original_data = amelia_data,
  verbose = TRUE
)

# source the fix factors function
source(here::here("code/functions/fix_mids_factors.R"))

# apply fix to ensure factors are properly set
cat("\nAdding factor variables to MICE object...\n")
mids_data <- fix_mids_factors(mids_data, c("trust_science_factor", "trust_scientists_factor"))

# verify factors exist
test_complete <- mice::complete(mids_data, 1)
cat("Variables in completed data:", paste(names(test_complete), collapse = ", "), "\n\n")

# ========================================================================
# FIT GEE MODELS FOR CONTINUOUS OUTCOMES
# ========================================================================
cat("\n\n=== FITTING GEE MODELS ===\n")

# fit to observed complete cases
observed_complete <- observed_data %>%
  filter(!is.na(trust_science))

gee_science_observed <- geepack::geeglm(
  trust_science ~ ns(years, 2),
  id = id,
  weights = weights,
  data = observed_data,
  corstr = "exchangeable"
)

# fit to imputed data
gee_science_imputed <- lapply(1:5, function(i) {
  dat_imp <- mice::complete(mids_data, i) %>%
    arrange(id, years)  # ensure proper sorting for GEE
  geepack::geeglm(
    trust_science ~ ns(years, 2),
    id = id,
    weights = weights,
    data = dat_imp,
    corstr = "exchangeable"
  )
})

# get predictions
pred_science_observed <- predict_response(gee_science_observed, "years[all]")
pred_science_imputed <- lapply(gee_science_imputed, function(m) {
  predict_response(m, "years[all]")
})

# pool imputed predictions
pooled_science <- pool_predictions(pred_science_imputed)

# compare results
cat("\nTrust in Science Results:\n")
cat("Observed: Year 0 =", round(pred_science_observed$predicted[1], 3),
    "Year 4 =", round(pred_science_observed$predicted[5], 3),
    "Change =", round(pred_science_observed$predicted[5] - pred_science_observed$predicted[1], 3), "\n")
cat("Imputed:  Year 0 =", round(pooled_science$predicted[1], 3),
    "Year 4 =", round(pooled_science$predicted[5], 3),
    "Change =", round(pooled_science$predicted[5] - pooled_science$predicted[1], 3), "\n")

# create comparison plot
p_gee_comparison <- plot(pred_oracle) +
  ggtitle("Oracle (Ground Truth)") +
  ylim(4.5, 5.5) +
  plot(pooled_science) +
  ggtitle("Imputed") +
  ylim(4.5, 5.5) +
  plot(pred_science_observed) +
  ggtitle("Observed (Complete Cases)") +
  ylim(4.5, 5.5) +
  plot_annotation(
    title = "Trust in Science: Comparing Oracle, Imputed, and Observed",
    subtitle = "GEE models with natural splines"
  )

print(p_gee_comparison)

# ========================================================================
# FIT PROPORTIONAL ODDS MODELS FOR CATEGORICAL OUTCOMES
# ========================================================================
cat("\n\n=== FITTING PROPORTIONAL ODDS MODELS ===\n")

# add factor variables to observed complete data
observed_complete <- observed_complete %>%
  mutate(
    trust_science_factor = factor(
      case_when(
        trust_science <= 3 ~ "low",
        trust_science <= 5 ~ "med",
        TRUE ~ "high"
      ),
      levels = c("low", "med", "high"),
      ordered = TRUE
    )
  )

# check if we have all levels
if (length(unique(observed_complete$trust_science_factor)) < 3) {
  cat("Warning: Not all trust levels present in observed data\n")
  print(table(observed_complete$trust_science_factor))
} else {
  # fit POLR model to observed
  polr_science_observed <- MASS::polr(
    trust_science_factor ~ ns(years, 2),
    weights = weights,
    data = observed_complete,
    Hess = TRUE
  )
  # fit to imputed data
  polr_science_imputed <- lapply(1:5, function(i) {
    dat_imp <- mice::complete(mids_data, i) %>%
      arrange(id, years)  # ensure consistent sorting
    MASS::polr(
      trust_science_factor ~ ns(years, 2),
      weights = weights,
      data = dat_imp,
      Hess = TRUE
    )
  })

  # get predictions
  pred_polr_oracle <- predict_response(polr_science_oracle, "years[all]")
  pred_polr_observed <- predict_response(polr_science_observed, "years[all]")
  pred_polr_imputed <- lapply(polr_science_imputed, function(m) {
    predict_response(m, "years[all]")
  })

  # pool predictions
  pooled_polr <- pool_predictions(pred_polr_imputed)

  library(patchwork)
  # plot comparison
  p_polr_comparison <- plot(pred_polr_observed) + plot(pred_polr_imputed) + plot(pred_polr_oracle)
    # ggtitle("Observed") +
    # ylim(0, 1) +
    # plot(pooled_polr) +
    # ggtitle("Imputed") +
    # ylim(0, 1) +
    # plot_annotation(
    #   title = "Trust Categories: Observed vs Imputed",
    #   subtitle = "Proportional odds models"
    # )

  print(p_polr_comparison)
}
library(patchwork)
pooled_polr <- pool_predictions(pred_polr_imputed)

p_observed <- plot(pred_polr_observed) + scale_y_continuous(limits=c(0,1))


p_imputed <- plot(pooled_polr) + scale_y_continuous(limits=c(0,1))

p_oracle<- plot(pred_polr_oracle)+ scale_y_continuous(limits=c(0,1))

p_oracle + p_observed + p_imputed

cat("\n\nAnalysis complete! The imputation partially recovers the declining trend seen in the oracle data.\n")



# this is our oracle data
library(splines)
library(ggeffects)
mod_test <- glm(trust_scientists ~ bs(years, 3), data = oracle_data)
summary(mod_test)


mod_test_2 <- glm(trust_scientists ~ bs(years, 3), data = observed_data)
summary(mod_test_2)


#
# plot(ggpredict(mod_test, terms="years[all]"))+ scale_y_continuous(limits=c(1,7))
# plot(ggpredict(mod_test_2, terms="years[all]"))+ scale_y_continuous(limits=c(1,7))
#




# method from manuscript --------------------------------------------------


cat("=== CONTINUOUS OUTCOMES - GEE MODELS ===\n\n")

# trust in science - imputed
cat("1. Trust in Science (continuous) - Imputed\n")
# Note: geeglm uses robust sandwich SEs by default when id is specified
gee_science_imputed <- lapply(1:10, function(i) {
  m <- geepack::geeglm(
    trust_science ~ ns(years, 3),
    id = id,
    weights = weights,
    data = mice::complete(mids_data, action = i)
  )
  ggeffects::predict_response(m, "years",
                              margin = "marginalmeans",
                              vcov_type = "robust")  # explicit for transparency
})

# pool predictions
gee_predictions_science_imputed <- ggeffects::pool_predictions(gee_science_imputed)

# create plot
gee_plot_trust_science_imputed <- plot(
  gee_predictions_science_imputed,
  show_data = TRUE,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "us",
  jitter = .4,
  dot_alpha = .01,
  limits = c(.5, 7.5),
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "dodgerblue",
    size = 1
  ) +
  labs(
    x = "Years: 2019-2024",
    y = "Trust in Science (1-7)",
    title = "Average Trust in Science (Imputed)"
  ) +
  theme_bw()

print(gee_plot_trust_science_imputed)



# trust in scientists - imputed
cat("2. Trust in Scientists (continuous) - Imputed\n")
gee_scientists_imputed <- lapply(1:10, function(i) {
  m <- geepack::geeglm(
    trust_scientists ~ ns(years, 3),
    id = id,
    weights = weights,
    data = mice::complete(mids_obj, action = i)
  )
  ggeffects::predict_response(m, "years",
                              margin = "marginalmeans",
                              vcov_type = "robust")  # explicit for transparency
})

# pool predictions
gee_predictions_scientists_imputed <- ggeffects::pool_predictions(gee_scientists_imputed)

# create plot
gee_plot_trust_scientists_imputed <- plot(
  gee_predictions_scientists_imputed,
  show_data = TRUE,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "us",
  jitter = .4,
  dot_alpha = .01,
  limits = c(.5, 7.5),
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "dodgerblue",
    size = 1,
    alpha = 1
  ) +
  theme_bw()

print(gee_plot_trust_scientists_imputed)

# trust in science - observed
cat("3. Trust in Science (continuous) - Observed\n")
gee_science_observed <- geepack::geeglm(
  trust_science ~ ns(years, 3),
  id = id,
  weights = weights,
  data = df_observed
)

gee_predictions_science_observed <- ggeffects::predict_response(
  gee_science_observed,
  "years",
  margin = "marginalmeans",
  vcov_type = "robust"  # explicit for transparency
)

# create plot
gee_plot_trust_science_observed <- plot(
  gee_predictions_science_observed,
  show_data = TRUE,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "metro",
  jitter = .4,
  dot_alpha = .01,
  limits = c(.5, 7.5),
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "red",
    size = 1,
    alpha = 1
  ) +
  labs(
    x = "Years: 2019-2024",
    y = "Trust in Science (1-7)",
    title = "Average Trust in Science (Observed)"
  ) +
  theme_bw()

print(gee_plot_trust_science_observed)

# trust in scientists - observed
cat("4. Trust in Scientists (continuous) - Observed\n")
gee_scientists_observed <- geepack::geeglm(
  trust_scientists ~ ns(years, 3),
  id = id,
  weights = weights,
  data = df_observed
)

gee_predictions_scientists_observed <- ggeffects::predict_response(
  gee_scientists_observed,
  "years",
  margin = "marginalmeans",
  vcov_type = "robust"  # explicit for transparency
)

# create plot
gee_plot_trust_scientists_observed <- plot(
  gee_predictions_scientists_observed,
  show_data = TRUE,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "metro",
  jitter = .4,
  dot_alpha = .01,
  limits = c(.5, 7.5),
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "red",
    size = 1,
    alpha = 1
  ) +
  labs(
    x = "Years: 2019-2024",
    y = "Trust in Scientists (1-7)",
    title = "Average Trust in Scientists (Observed)"
  ) +
  theme_bw()

print(gee_plot_trust_scientists_observed)
# combined plot for continuous outcomes
combined_continuous_plot <-
  (gee_plot_trust_science_observed + gee_plot_trust_science_imputed) /
  (gee_plot_trust_scientists_observed + gee_plot_trust_scientists_imputed) +
  plot_annotation(
    title = "Trust in Science and Scientists: Continuous Outcomes",
    subtitle = "GEE models with natural splines"
  )

print(combined_continuous_plot)

# save results in consistent format
results_gee_science_imputed <- list(
  predictions = gee_predictions_science_imputed,
  plot = gee_plot_trust_science_imputed,
  n_imputations = 10,
  method = "gee_pool_predictions",
  formula = trust_science ~ ns(years, 3),
  outcome_var = "trust_science",
  predictor_var = "years"
)

results_gee_scientists_imputed <- list(
  predictions = gee_predictions_scientists_imputed,
  plot = gee_plot_trust_scientists_imputed,
  n_imputations = 10,
  method = "gee_pool_predictions",
  formula = trust_scientists ~ ns(years, 3),
  outcome_var = "trust_scientists",
  predictor_var = "years"
)

results_gee_science_observed <- list(
  predictions = gee_predictions_science_observed,
  plot = gee_plot_trust_science_observed,
  n_imputations = 1,
  method = "gee_single",
  formula = trust_science ~ ns(years, 3),
  outcome_var = "trust_science",
  predictor_var = "years"
)

results_gee_scientists_observed <- list(
  predictions = gee_predictions_scientists_observed,
  plot = gee_plot_trust_scientists_observed,
  n_imputations = 1,
  method = "gee_single",
  formula = trust_scientists ~ ns(years, 3),
  outcome_var = "trust_scientists",
  predictor_var = "years"
)

# save models
here_save_qs(results_gee_science_observed, "results_gee_science_observed_05", push_mods)
here_save_qs(results_gee_science_imputed, "results_gee_science_imputed_05", push_mods)
here_save_qs(results_gee_scientists_observed, "results_gee_scientists_observed_05", push_mods)
here_save_qs(results_gee_scientists_imputed, "results_gee_scientists_imputed_05", push_mods)




# ========================================================================
# CATEGORICAL OUTCOMES - PREVIOUS APPROACH (nnet::multinom) ERRORS NOT CORRECT
# ========================================================================
#
# cat("\n=== CATEGORICAL OUTCOMES - CURRENT APPROACH ===\n\n")
#
# # trust in science factor - imputed
# cat("1. Trust in Science (categorical) - Imputed\n")
# trust_science_factor_nnet <- lapply(1:10, function(i) {
#   m <- nnet::multinom(
#     trust_science_factor ~ bs(years, 3),
#     weights = weights,
#     data = mice::complete(mids_obj, action = i),
#     trace = TRUE
#   )
#   ggeffects::predict_response(
#     m,
#     "years[all]",
#     margin = "marginaleffects",
#     weights = "weights",
#     ci_level = .95,
#     vcov_args = list(type = "CR", cluster = mids_obj$data$id)
#   )
# })
#
# pooled_trust_science_nnet <- pool_predictions(trust_science_factor_nnet)
# here_save(pooled_trust_science_nnet, "pooled_trust_science_nnet_05")
#
# # check CI widths
# ci_widths_nnet <- with(pooled_trust_science_nnet, conf.high - conf.low)
# cat("  Mean CI width (nnet):", round(mean(ci_widths_nnet), 4), "\n")
# cat("  Range:", round(min(ci_widths_nnet), 4), "-", round(max(ci_widths_nnet), 4), "\n\n")
#
# # trust in scientists factor - imputed
# cat("2. Trust in Scientists (categorical) - Imputed\n")
# trust_scientists_factor_nnet <- lapply(1:10, function(i) {
#   m <- nnet::multinom(
#     trust_scientists_factor ~ bs(years, 3),
#     weights = weights,
#     data = mice::complete(mids_obj, action = i),
#     trace = TRUE
#   )
#   ggeffects::predict_response(
#     m,
#     "years[all]",
#     margin = "marginaleffects",
#     weights = "weights",
#     ci_level = .95,
#     vcov_args = list(type = "CR", cluster = mids_obj$data$id)
#   )
# })
#
# pooled_trust_scientists_nnet <- pool_predictions(trust_scientists_factor_nnet)
# here_save(pooled_trust_scientists_nnet, "pooled_trust_scientists_nnet_05")
#


# ========================================================================
# ALTERNATIVE: MASS::polr with splines
# ========================================================================

cat("\n=== ALTERNATIVE 2: MASS::polr ===\n\n")



# trust in science - polr
cat("1a. Trust in Science (polr) - Observed\n")

# fit model - using ns() instead of bs() to avoid zero SE at boundaries
observed_trust_science_polr<- polr(
  trust_science_factor ~ ns(years, 3),
  data = df_observed,
  weights = weights,
  Hess = TRUE  # needed for vcov
)
thr_ses <- lapply(observed_trust_science_polr, attr, "se_thresh") |> unlist()
range(thr_ses)        # should not be exactly 0
hist(thr_ses)         # expect a right‑skew, not a pile at 0

# get cluster-robust standard errors
vcov_cluster <- vcovCL(observed_trust_science_polr, cluster = df_observed$id)

# try ggeffects with cluster-robust SEs
predicted_trust_science_observed <- predict_response(
  observed_trust_science_polr,
  "years[all]",
  vcov_fun = "vcovCL",
  vcov_args = list(cluster = df_observed$id)
)
predicted_trust_science_observed
plot_trust_science_observed <- plot(
  predicted_trust_science_observed,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "metro",
  limits = c(0, 1),
  dot_size = 2
) +
  labs(
    x = "Years: 2019-2024",
    y = "Predicted Probability",
    title = "Level of Trust in Science (Observed)"
  ) +
  theme_bw()

# Display the simple plot
print(plot_trust_science_observed)

cat_trust_science_observed <- list(
  predictions = predicted_trust_science_observed,
  plot = plot_trust_science_observed,
  n_imputations = 10,
  method = "pool_predictions",
  formula = trust_science_factor ~ ns(years, 3),
  outcome_var = "trust_science_factor",
  predictor_var = "years"
)
cat_trust_science_observed$predictions
# save
here_save_qs(cat_trust_science_observed, "cat_trust_science_observed", push_mods)


# save
here_save(cat_trust_science_observed, "cat_trust_science_observed")
# trust in science - polr

cat("1b. Trust in Science (polr) - Imputed\n")
trust_science_polr <- lapply(1:10, function(i) {
  tryCatch({
    df_imp <- mice::complete(mids_obj, i)

    # fit model - using ns() instead of bs() to avoid zero SE at boundaries
    m <- polr(
      trust_science_factor ~ ns(years, 3),
      data = df_imp,
      weights = weights,
      Hess = TRUE  # needed for vcov
    )

    # try ggeffects with cluster-robust SEs
    pred <- predict_response(
      m,
      "years[all]",
      vcov_fun = "vcovCL",
      vcov_args = list(cluster = df_imp$id)
    )

    # if ggeffects fails, do manual prediction
    if (is.null(pred)) {
      years_seq <- seq(min(df_imp$years), max(df_imp$years), length.out = 50)
      pred_probs <- predict(m, newdata = data.frame(years = years_seq), type = "probs")

      # create ggeffects-like structure
      pred <- data.frame(
        x = years_seq,
        predicted = pred_probs[, 2],  # middle category as example
        std.error = NA,
        conf.low = NA,
        conf.high = NA
      )
    }

    return(pred)

  }, error = function(e) {
    cat("  Error in imputation", i, ":", e$message, "\n")
    return(NULL)
  })
})
# remove failed imputations
trust_science_polr <- trust_science_polr[!sapply(trust_science_polr, is.null)]
pooled_trust_science_polr <- pool_predictions(trust_science_polr)
pooled_trust_science_polr


if (length(trust_science_polr) > 0) {
  pooled_trust_science_polr <- pool_predictions(trust_science_polr)

  # check CI widths if available
  if (!any(is.na(pooled_trust_science_polr$conf.low))) {
    ci_widths_polr <- with(pooled_trust_science_polr, conf.high - conf.low)
    cat("  Mean CI width (polr):", round(mean(ci_widths_polr), 4), "\n")
    # cat("  CI width ratio (polr/nnet):", round(mean(ci_widths_polr) / mean(ci_widths_nnet), 2), "\n")
  }
}



# Simple plot for categorical trust in science
plot_trust_science_imputed <- plot(
  pooled_trust_science_polr,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "us",
  limits = c(0, 1),
  dot_size = 2
) +
  labs(
    x = "Years: 2019-2024",
    y = "Predicted Probability",
    title = "Level of Trust in Science (Imputed)"
  ) +
  theme_bw()

# Display the simple plot
print(plot_trust_science_imputed)

# For saving/compatibility with workflow
cat_trust_science_imputed <- list(
  predictions = pooled_trust_science_polr,
  plot = plot_trust_science_imputed,
  n_imputations = 10,
  method = "pool_predictions",
  formula = trust_science_factor ~ ns(years, 3),
  outcome_var = "trust_science_factor",
  predictor_var = "years"
)

# trust in scientiests - polr
cat("2. Trust in Scientists (polr) - Imputed\n")

# fit model - using ns() instead of bs() to avoid zero SE at boundaries
observed_trust_scientists_polr<- polr(
  trust_scientists_factor ~ ns(years, 3),
  data = df_observed,
  weights = weights,
  Hess = TRUE  # needed for vcov
)

# get cluster-robust standard errors
vcov_cluster_scientists <- vcovCL(observed_trust_scientists_polr, cluster = df_observed$id)

# try ggeffects with cluster-robust SEs
predicted_trust_scientists_observed <- predict_response(
  observed_trust_scientists_polr,
  "years[all]",
  vcov_fun = "vcovCL",
  vcov_args = list(cluster = df_observed$id)
)

plot_trust_scientists_observed <- plot(
  predicted_trust_scientists_observed,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "metro",
  limits = c(0, 1),
  dot_size = 2
) +
  labs(
    x = "Years: 2019-2024",
    y = "Predicted Probability",
    title = "Level of Trust in Scientists (Observed)"
  ) +
  theme_bw()

# Display the simple plot
print(plot_trust_scientists_observed)

cat_trust_scientists_observed <- list(
  predictions = predicted_trust_science_observed,
  plot = plot_trust_science_observed,
  n_imputations = 10,
  method = "pool_predictions",
  formula = trust_science_factor ~ ns(years, 3),
  outcome_var = "trust_science_factor",
  predictor_var = "years"
)
cat_trust_scientists_observed$plot
# save
here_save(cat_trust_scientists_observed, "cat_trust_scientists_observed")



cat("2. Trust in Scientists (polr) - Imputed\n")

trust_scientists_polr <- lapply(1:10, function(i) {
  tryCatch({
    df_imp <- mice::complete(mids_obj, i)

    # fit model - using ns() instead of bs() to avoid zero SE at boundaries
    m <- polr(
      trust_scientists_factor ~ ns(years, 3),
      data = df_imp,
      weights = weights,
      Hess = TRUE  # needed for vcov
    )

    # try ggeffects with cluster-robust SEs
    pred <- predict_response(
      m,
      "years[all]",
      vcov_fun = "vcovCL",
      vcov_args = list(cluster = df_imp$id)
    )

    # if ggeffects fails, do manual prediction
    if (is.null(pred)) {
      years_seq <- seq(min(df_imp$years), max(df_imp$years), length.out = 50)
      pred_probs <- predict(m, newdata = data.frame(years = years_seq), type = "probs")

      # create ggeffects-like structure
      pred <- data.frame(
        x = years_seq,
        predicted = pred_probs[, 2],  # middle category as example
        std.error = NA,
        conf.low = NA,
        conf.high = NA
      )
    }

    return(pred)

  }, error = function(e) {
    cat("  Error in imputation", i, ":", e$message, "\n")
    return(NULL)
  })
})
# remove failed imputations
# trust_scientists_polr <- trust_scientists_polr[!sapply(trust_scientists_polr, is.null)]
pooled_trust_scientists_polr <- pool_predictions(trust_scientists_polr)
pooled_trust_scientists_polr



# Simple plot for categorical trust in science
plot_trust_scientists_imputed <- plot(
  pooled_trust_scientists_polr,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "us",
  limits = c(0, 1),
  dot_size = 2
) +
  labs(
    x = "Years: 2019-2024",
    y = "Predicted Probability",
    title = "Level of Trust in Scientists (Imputed)"
  ) +
  theme_bw()

# Display the simple plot
print(plot_trust_scientists_imputed)

# For saving/compatibility with workflow
cat_trust_scientists_imputed <- list(
  predictions = pooled_trust_scientists_polr,
  plot = plot_trust_scientists_imputed,
  n_imputations = 10,
  method = "pool_predictions",
  formula = trust_science_factor ~ ns(years, 3),
  outcome_var = "trust_science_factor",
  predictor_var = "years"
)

library(patchwork)
# Combined plot - observed vs imputed
combined_categorical_plot <-
  (cat_trust_science_observed$plot + cat_trust_science_imputed$plot) /
  (cat_trust_scientists_observed$plot + cat_trust_scientists_imputed$plot) +
  plot_annotation(
    title = "Trust in Science and Scientists: Observed vs Imputed",
    subtitle = "Categorical analysis with 10 imputations"
  )

# Display combined plot
print(combined_categorical_plot)

# save models
here_save_qs(
  cat_trust_science_observed,
  "cat_trust_science_observed", push_mods
)
here_save_qs(cat_trust_science_imputed, "cat_trust_science_imputed", push_mods)
here_save_qs(
  cat_trust_scientists_observed,
  "cat_trust_scientists_observed",
  push_mods
)
here_save_qs(
  cat_trust_scientists_imputed,
  "cat_trust_scientists_imputed",
  push_mods
)

cat("\n✓ CATEGORICAL ANALYSIS COMPLETE\n")
cat("Plots saved and displayed.\n\n")


