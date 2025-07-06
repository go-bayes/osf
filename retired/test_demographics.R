# test_demographics.R - Quick test of demographic predictors
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(MASS)
library(Amelia)
library(mice)
library(ggeffects)
library(geepack)

# smaller sample for faster testing
n_participants <- 5000
n_waves <- 5

# generate participant characteristics
participants <- data.frame(
  id = 1:n_participants,
  age_baseline = round(rnorm(n_participants, mean = 50, sd = 15)),
  gender = sample(c("Female", "Male"), n_participants,
                  replace = TRUE, prob = c(0.62, 0.38)),
  ethnicity = sample(c("NZ European", "Maori", "Pacific", "Asian", "Other"),
                     n_participants, replace = TRUE,
                     prob = c(0.68, 0.17, 0.06, 0.10, 0.04)),
  education = sample(1:7, n_participants, replace = TRUE,
                     prob = c(0.05, 0.10, 0.20, 0.25, 0.20, 0.15, 0.05))
)

# generate baseline trust from demographic predictors
participants <- participants %>%
  mutate(
    trust_science_mean = 4.0 + 
      0.3 * education +
      0.01 * (age_baseline - 50) +
      0.2 * (gender == "Female") +
      -0.3 * (ethnicity == "Maori") +
      -0.2 * (ethnicity == "Pacific") +
      0.1 * (ethnicity == "Asian"),
    
    trust_scientists_mean = 3.8 + 
      0.3 * education +
      0.01 * (age_baseline - 50) +
      0.2 * (gender == "Female") +
      -0.3 * (ethnicity == "Maori") +
      -0.2 * (ethnicity == "Pacific") +
      0.1 * (ethnicity == "Asian")
  )

# check demographic effects
cat("Demographic model R-squared:\n")
demo_model <- lm(trust_science_mean ~ education + age_baseline + gender + ethnicity, 
                 data = participants)
cat("R-squared:", round(summary(demo_model)$r.squared, 3), "\n\n")

# generate correlated baseline trust scores
Sigma <- matrix(c(0.5^2, 0.5*0.5*0.7, 0.5*0.5*0.7, 0.5^2), 2, 2)
trust_baselines <- matrix(NA, n_participants, 2)
for (i in 1:n_participants) {
  trust_baselines[i,] <- mvrnorm(1,
                                 mu = c(participants$trust_science_mean[i],
                                        participants$trust_scientists_mean[i]),
                                 Sigma = Sigma)
}

participants$trust_science_baseline <- pmax(1, pmin(7, trust_baselines[,1]))
participants$trust_scientists_baseline <- pmax(1, pmin(7, trust_baselines[,2]))

# create trust groups for time trends
participants$trust_group <- case_when(
  participants$education <= 2 ~ "low",
  participants$education <= 5 ~ "medium",
  participants$education >= 6 ~ "high"
)

# create long format
long_data <- expand.grid(id = participants$id, years = 0:4, stringsAsFactors = FALSE)
long_data <- merge(long_data, participants, by = "id")

# generate trust with time trends
slopes <- c(low = -0.3, medium = 0.0, high = 0.2)
long_data <- long_data %>%
  mutate(
    trust_science = trust_science_baseline + slopes[trust_group] * years + rnorm(n(), 0, 0.10),
    trust_scientists = trust_scientists_baseline + slopes[trust_group] * years + rnorm(n(), 0, 0.10)
  ) %>%
  mutate(across(c(trust_science, trust_scientists), ~ pmin(7, pmax(1, .))))

# save oracle
oracle_data <- long_data

# generate dropout
long_data <- long_data %>%
  arrange(id, years) %>%
  group_by(id) %>%
  mutate(
    drop_prob = plogis(-2.0 + 0.4 * (4 - trust_science) + 0.15 * years),
    dropped = cummax(rbinom(n(), 1, drop_prob))
  ) %>%
  ungroup()

# apply missingness
observed_data <- long_data %>%
  mutate(
    trust_science = ifelse(dropped == 1, NA, trust_science),
    trust_scientists = ifelse(dropped == 1, NA, trust_scientists)
  )

# prepare for Amelia
amelia_data <- observed_data %>%
  dplyr::select(id, years, age_baseline, gender, ethnicity, education, 
                trust_science, trust_scientists) %>%
  arrange(id, years)

# run Amelia
cat("\nRunning Amelia with demographics...\n")
amelia_out <- amelia(
  amelia_data,
  m = 5,
  idvars = c(),  # no id vars - use everything
  noms = c("gender", "ethnicity"),
  bounds = matrix(c(7, 1, 7, 8, 1, 7), nrow = 2, ncol = 3, byrow = TRUE),
  splinetime = 3,
  polytime = 3,
  ts = "years",
  cs = "id"
)

# check one imputation
imp1 <- amelia_out$imputations[[1]]
imp_model <- lm(trust_science ~ education + age_baseline + gender + ethnicity, data = imp1)
cat("\nPost-imputation R-squared:", round(summary(imp_model)$r.squared, 3), "\n")
cat("Coefficients:\n")
print(round(coef(imp_model), 3))

# compare trajectories
cat("\n\nTrajectory comparison:\n")

# oracle
mod_oracle <- geeglm(trust_science ~ poly(years, 2), id = id, data = oracle_data)
pred_oracle <- predict_response(mod_oracle, "years[all]")

# observed
observed_complete <- observed_data[!is.na(observed_data$trust_science),]
mod_observed <- geeglm(trust_science ~ poly(years, 2), id = id, data = observed_complete)
pred_observed <- predict_response(mod_observed, "years[all]")

# imputed (average across imputations)
pred_imputed_list <- lapply(1:5, function(i) {
  imp_data <- amelia_out$imputations[[i]]
  mod <- geeglm(trust_science ~ poly(years, 2), id = id, data = imp_data)
  predict_response(mod, "years[all]")
})

# simple average
pred_imputed_avg <- rowMeans(sapply(pred_imputed_list, function(p) p$predicted))

cat("Oracle:   Year 0 =", round(pred_oracle$predicted[1], 3),
    "Year 4 =", round(pred_oracle$predicted[5], 3),
    "Change =", round(pred_oracle$predicted[5] - pred_oracle$predicted[1], 3), "\n")
cat("Observed: Year 0 =", round(pred_observed$predicted[1], 3),
    "Year 4 =", round(pred_observed$predicted[5], 3),
    "Change =", round(pred_observed$predicted[5] - pred_observed$predicted[1], 3), "\n")
cat("Imputed:  Year 0 =", round(pred_imputed_avg[1], 3),
    "Year 4 =", round(pred_imputed_avg[5], 3),
    "Change =", round(pred_imputed_avg[5] - pred_imputed_avg[1], 3), "\n")