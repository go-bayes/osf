# test_mice_methods.R - Compare different MICE imputation methods
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(MASS)
library(mice)
library(splines)
library(ggeffects)
library(geepack)
# library(tictoc)  # for timing - use base R instead

# parameters (smaller sample for testing)
n_participants <- 5000
n_waves <- 5
baseline_year <- 0

# generate participant characteristics
cat("Creating participant characteristics...\n")
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

# create trust groups for time trends
participants$trust_group <- case_when(
  participants$education <= 2 ~ "low",
  participants$education <= 5 ~ "medium",
  participants$education >= 6 ~ "high"
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
slopes <- c(low = -0.3, medium = 0.0, high = 0.2)

long_data <- long_data %>%
  mutate(
    trust_science = trust_science_baseline +
                    slopes[trust_group] * years +
                    rnorm(n(), 0, 0.10),
    trust_scientists = trust_scientists_baseline +
                       slopes[trust_group] * years +
                       rnorm(n(), 0, 0.10)
  ) %>%
  mutate(across(c(trust_science, trust_scientists),
                ~ pmin(7, pmax(1, .))))

# add weights
long_data$weights <- 1

# save oracle data
oracle_data <- long_data %>%
  arrange(id, years)

# generate dropout based on current trust values
long_data <- long_data %>%
  arrange(id, years) %>%
  group_by(id) %>%
  mutate(
    drop_prob = plogis(
      -2.0 + 0.4 * (4 - trust_science) + 0.15 * years
    ),
    dropped = cummax(rbinom(n(), 1, drop_prob))
  ) %>%
  ungroup()

# apply missingness
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
  dplyr::select(-trust_science_obs, -trust_scientists_obs, 
                -trust_group, -drop_prob, -dropped,
                -trust_science_baseline, -trust_scientists_baseline,
                -trust_science_mean, -trust_scientists_mean) %>%
  arrange(id, years)

# check missingness
cat("\nMissingness by year:\n")
observed_data %>%
  group_by(years) %>%
  summarise(pct_missing = round(100 * mean(is.na(trust_science)), 1)) %>%
  print()

# ========================================================================
# PREPARE DATA FOR MICE
# ========================================================================
cat("\n\n=== PREPARING DATA FOR MICE ===\n")

# add COVID indicator (year 1 = 2020)
observed_data$covid <- as.integer(observed_data$years >= 1)

# add natural spline of time - create separate columns
ns_matrix <- ns(observed_data$years, df = 3)
observed_data$ns_time_1 <- ns_matrix[,1]
observed_data$ns_time_2 <- ns_matrix[,2]
observed_data$ns_time_3 <- ns_matrix[,3]

# convert factors to proper type
observed_data$gender <- as.factor(observed_data$gender)
observed_data$ethnicity <- as.factor(observed_data$ethnicity)

# ========================================================================
# METHOD 1: MICE with PMM (Predictive Mean Matching)
# ========================================================================
cat("\n=== METHOD 1: MICE with PMM ===\n")

# set up predictor matrix
predM <- make.predictorMatrix(observed_data)

# ensure covid and ns_time predict trust variables
predM[c("trust_science", "trust_scientists"), "covid"] <- 1
predM[c("trust_science", "trust_scientists"), c("ns_time_1", "ns_time_2", "ns_time_3")] <- 1

# don't use id or wave as predictors
predM[, c("id", "wave")] <- 0

# don't impute these variables
predM[c("id", "wave", "years", "covid", "ns_time_1", "ns_time_2", "ns_time_3"), ] <- 0

cat("Running MICE with PMM...\n")
start_time <- Sys.time()
mice_pmm <- mice(
  observed_data,
  predictorMatrix = predM,
  method = "pmm",
  m = 5,
  maxit = 10,
  printFlag = FALSE
)
end_time <- Sys.time()
cat("Time taken:", round(difftime(end_time, start_time, units = "secs"), 1), "seconds\n")

# ========================================================================
# METHOD 2: MICE with CART (Classification and Regression Trees)
# ========================================================================
cat("\n=== METHOD 2: MICE with CART ===\n")

cat("Running MICE with CART...\n")
start_time <- Sys.time()
mice_cart <- mice(
  observed_data,
  predictorMatrix = predM,
  method = "cart",
  m = 5,
  maxit = 10,
  printFlag = FALSE
)
end_time <- Sys.time()
cat("Time taken:", round(difftime(end_time, start_time, units = "secs"), 1), "seconds\n")

# ========================================================================
# METHOD 3: MICE with Random Forest (if miceRanger available)
# ========================================================================
cat("\n=== METHOD 3: MICE with Random Forest ===\n")

# check if miceRanger is available
if (requireNamespace("miceRanger", quietly = TRUE)) {
  library(miceRanger)
  
  cat("Running miceRanger...\n")
  start_time <- Sys.time()
  mice_rf <- miceRanger(
    observed_data,
    vars = c("trust_science", "trust_scientists"),
    num.trees = 100,
    m = 5,
    maxiter = 10,
    verbose = FALSE
  )
  end_time <- Sys.time()
  cat("Time taken:", round(difftime(end_time, start_time, units = "secs"), 1), "seconds\n")
} else {
  cat("miceRanger not available. Using mice with 'rf' method instead...\n")
  start_time <- Sys.time()
  mice_rf <- mice(
    observed_data,
    predictorMatrix = predM,
    method = "rf",
    m = 5,
    maxit = 10,
    printFlag = FALSE
  )
  end_time <- Sys.time()
  cat("Time taken:", round(difftime(end_time, start_time, units = "secs"), 1), "seconds\n")
}

# ========================================================================
# COMPARE RESULTS
# ========================================================================
cat("\n\n=== COMPARING IMPUTATION METHODS ===\n")

# function to get trajectory from imputed data
get_trajectory <- function(mice_obj, method_name) {
  # extract predictions for each imputation
  preds <- lapply(1:5, function(i) {
    if (inherits(mice_obj, "miceRanger")) {
      dat_imp <- completeRanger(mice_obj, i)
    } else {
      dat_imp <- complete(mice_obj, i)
    }
    dat_imp <- dat_imp %>% arrange(id, years)
    
    mod <- geeglm(
      trust_science ~ ns(years, 3),
      id = id,
      data = dat_imp,
      corstr = "exchangeable"
    )
    predict_response(mod, "years[all]")
  })
  
  # average predictions
  pred_avg <- rowMeans(sapply(preds, function(p) p$predicted))
  
  data.frame(
    method = method_name,
    year = 0:4,
    predicted = pred_avg,
    change = pred_avg[5] - pred_avg[1]
  )
}

# get oracle trajectory
mod_oracle <- geeglm(trust_science ~ ns(years, 3), id = id, data = oracle_data)
pred_oracle <- predict_response(mod_oracle, "years[all]")

# get observed trajectory
observed_complete <- observed_data[!is.na(observed_data$trust_science),]
mod_observed <- geeglm(trust_science ~ ns(years, 3), id = id, data = observed_complete)
pred_observed <- predict_response(mod_observed, "years[all]")

# get trajectories for each method
traj_pmm <- get_trajectory(mice_pmm, "PMM")
traj_cart <- get_trajectory(mice_cart, "CART")
if (exists("mice_rf")) {
  traj_rf <- get_trajectory(mice_rf, "Random Forest")
}

# print results
cat("\nTrust in Science Trajectories:\n")
cat("Oracle:   Year 0 =", round(pred_oracle$predicted[1], 3),
    "Year 4 =", round(pred_oracle$predicted[5], 3),
    "Change =", round(pred_oracle$predicted[5] - pred_oracle$predicted[1], 3), "\n")
cat("Observed: Year 0 =", round(pred_observed$predicted[1], 3),
    "Year 4 =", round(pred_observed$predicted[5], 3),
    "Change =", round(pred_observed$predicted[5] - pred_observed$predicted[1], 3), "\n")
cat("\nImputation methods:\n")
cat("PMM:      Year 0 =", round(traj_pmm$predicted[1], 3),
    "Year 4 =", round(traj_pmm$predicted[5], 3),
    "Change =", round(traj_pmm$change[1], 3), "\n")
cat("CART:     Year 0 =", round(traj_cart$predicted[1], 3),
    "Year 4 =", round(traj_cart$predicted[5], 3),
    "Change =", round(traj_cart$change[1], 3), "\n")
if (exists("traj_rf")) {
  cat("RF:       Year 0 =", round(traj_rf$predicted[1], 3),
      "Year 4 =", round(traj_rf$predicted[5], 3),
      "Change =", round(traj_rf$change[1], 3), "\n")
}

# ========================================================================
# DIAGNOSTIC: Check one imputation
# ========================================================================
cat("\n\n=== IMPUTATION DIAGNOSTICS ===\n")

# check demographic prediction in first PMM imputation
imp1_pmm <- complete(mice_pmm, 1)
demo_model_pmm <- lm(trust_science ~ education + age_baseline + gender + ethnicity, data = imp1_pmm)
cat("\nPMM - R² of demographics predicting trust:", round(summary(demo_model_pmm)$r.squared, 3), "\n")

# check if COVID and time are being used
cat("\nChecking if COVID variable affects imputed values:\n")
imp1_pmm %>%
  group_by(covid) %>%
  summarise(
    mean_trust = round(mean(trust_science), 3),
    n = n()
  ) %>%
  print()

cat("\nDone!")