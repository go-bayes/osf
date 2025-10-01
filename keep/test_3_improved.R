# test_3_improved.R - Improved simulation with correlated baselines and current-trust dropout
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(MASS)  # for mvrnorm
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
Sigma <- matrix(c(0.5^2, 0.5*0.5*0.7,
                  0.5*0.5*0.7, 0.5^2), 2, 2)

# generate for each participant
trust_baselines <- matrix(NA, n_participants, 2)
for (i in 1:n_participants) {
  trust_baselines[i,] <- mvrnorm(1,
                                 mu = c(participants$trust_science_mean[i],
                                        participants$trust_scientists_mean[i]),
                                 Sigma = Sigma)
}

# assign and bound to 1-7 scale
participants$trust_science_baseline <- pmax(1, pmin(7, trust_baselines[,1]))
participants$trust_scientists_baseline <- pmax(1, pmin(7, trust_baselines[,2]))

# check correlation
cat("Baseline correlation between trust measures:",
    round(cor(participants$trust_science_baseline,
              participants$trust_scientists_baseline), 3), "\n")

# check distribution
cat("\nBaseline trust by group:\n")
aggregate(trust_science_baseline ~ trust_group, data = participants,
          function(x) round(c(mean = mean(x), n = length(x)), 2))

# check demographic effects
cat("\nDemographic effects on trust:\n")
demo_model <- lm(trust_science_baseline ~ education + age_baseline + gender + ethnicity,
                 data = participants)
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
                         levels = baseline_year:(baseline_year + n_waves - 1))

# generate trust trajectories using LINEAR SLOPES
cat("Generating trust outcomes with linear slopes...\n")

# define slopes for each group - moderate for realistic patterns
slopes <- c(low = -0.3, medium = 0.0, high = 0.2)

long_data <- long_data %>%
  mutate(
    # trust in science with linear trend
    trust_science = trust_science_baseline +
                    slopes[trust_group] * years +
                    rnorm(n(), 0, 0.10),  # reduced noise for clearer signal
    # trust in scientists with similar linear trend
    trust_scientists = trust_scientists_baseline +
                       slopes[trust_group] * years +
                       rnorm(n(), 0, 0.10)  # reduced noise for clearer signal
  ) %>%
  # bound to 1-7 scale
  mutate(across(c(trust_science, trust_scientists),
                ~ pmin(7, pmax(1, .))))

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

# generate DROPOUT based on CURRENT trust values
cat("\nGenerating dropout based on current trust values...\n")

long_data <- long_data %>%
  arrange(id, years) %>%
  group_by(id) %>%
  mutate(
    # logistic dropout probability based on current trust
    drop_prob = plogis(
      -2.0 +                               # base intercept
      0.4 * (4 - trust_science) +         # stronger effect: low trust → higher dropout
      0.15 * years                        # attrition increases over time
    ),
    # simulate dropout (monotone by cumulated indicator)
    dropped = cummax(rbinom(n(), 1, drop_prob))
  ) %>%
  ungroup()

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
  data = observed_complete,
  corstr = "exchangeable"
)

pred_observed <- predict_response(mod_observed, "years[all]")
cat("Observed trajectory:\n")
print(round(pred_observed$predicted, 3))

# create visualizations
library(patchwork)
library(MASS)  # for polr

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

print(combined_continuous)
ggsave("results/figures/test_3_continuous_comparison.png", combined_continuous,
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
cat("\nVariables in observed_data:", paste(names(observed_data), collapse=", "), "\n\n")

# remove only simulation artifacts, keep demographic predictors
amelia_data <- observed_data %>%
  dplyr::select(-trust_group, -drop_prob, -dropped,
                -trust_science_baseline, -trust_scientists_baseline,
                -trust_science_mean, -trust_scientists_mean) %>%
  arrange(id, years)

# convert wave to character for Amelia
amelia_data$wave <- as.character(amelia_data$wave)

cat("Variables in amelia_data:", paste(names(amelia_data), collapse=", "), "\n\n")

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
cat("Oracle:   Year 0 =", round(pred_oracle$predicted[1], 3),
    "Year 4 =", round(pred_oracle$predicted[5], 3),
    "Change =", round(pred_oracle$predicted[5] - pred_oracle$predicted[1], 3), "\n")
cat("Observed: Year 0 =", round(pred_observed$predicted[1], 3),
    "Year 4 =", round(pred_observed$predicted[5], 3),
    "Change =", round(pred_observed$predicted[5] - pred_observed$predicted[1], 3), "\n")
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
  plot(pred_observed) +
  ggtitle("Observed (Complete Cases)") +
  ylim(4.5, 5.5) +
  plot_annotation(
    title = "Trust in Science: Comparing Oracle, Imputed, and Observed",
    subtitle = "GEE models with natural splines"
  )

print(p_gee_comparison)
ggsave("results/figures/test_3_imputation_comparison.png", p_gee_comparison,
       width = 15, height = 5, dpi = 300)

cat("\n\nAnalysis complete! Check if imputation better tracks the oracle data.\n")

# check demographic prediction in imputed data
cat("\n\n=== CHECKING DEMOGRAPHIC PREDICTION IN IMPUTED DATA ===\n")
imp1 <- mice::complete(mids_data, 1)
imp_model <- lm(trust_science ~ education + age_baseline + gender + ethnicity, data = imp1)
cat("R-squared of demographics predicting trust in imputed data:", round(summary(imp_model)$r.squared, 3), "\n")
cat("Coefficients:\n")
print(round(coef(imp_model), 3))
