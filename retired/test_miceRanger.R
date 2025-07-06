# test_miceRanger.R - Test random forest imputation with miceRanger
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(MASS)
library(mice)
library(splines)
library(ggeffects)
library(geepack)

# check if miceRanger is installed
if (!requireNamespace("miceRanger", quietly = TRUE)) {
  cat("Installing miceRanger...\n")
  install.packages("miceRanger")
}
library(miceRanger)

# parameters
n_participants <- 5000
n_waves <- 5

cat("=== TESTING MICERANGER WITH LONGITUDINAL DATA ===\n")
cat("Sample size:", n_participants, "participants\n")
cat("Waves:", n_waves, "\n\n")

# ========================================================================
# GENERATE DATA (same as before)
# ========================================================================
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

# create trust groups
participants$trust_group <- case_when(
  participants$education <= 2 ~ "low",
  participants$education <= 5 ~ "medium",
  participants$education >= 6 ~ "high"
)

# generate baseline trust from demographics
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

# generate correlated baseline trust
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

# create long format for data generation
long_data <- expand.grid(id = participants$id, years = 0:4, stringsAsFactors = FALSE)
long_data <- merge(long_data, participants, by = "id")

# generate trajectories
slopes <- c(low = -0.3, medium = 0.0, high = 0.2)
long_data <- long_data %>%
  mutate(
    trust_science = trust_science_baseline + slopes[trust_group] * years + rnorm(n(), 0, 0.10),
    trust_scientists = trust_scientists_baseline + slopes[trust_group] * years + rnorm(n(), 0, 0.10)
  ) %>%
  mutate(across(c(trust_science, trust_scientists), ~ pmin(7, pmax(1, .))))

# save oracle
oracle_long <- long_data %>% arrange(id, years)

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
long_data <- long_data %>%
  mutate(
    trust_science = ifelse(dropped == 1, NA, trust_science),
    trust_scientists = ifelse(dropped == 1, NA, trust_scientists)
  )

# ========================================================================
# PREPARE WIDE FORMAT DATA
# ========================================================================
cat("\nPreparing wide format data...\n")

wide_data <- long_data %>%
  dplyr::select(id, years, trust_science, trust_scientists, 
                age_baseline, gender, ethnicity, education) %>%
  pivot_wider(
    id_cols = c(id, age_baseline, gender, ethnicity, education),
    names_from = years,
    values_from = c(trust_science, trust_scientists),
    names_sep = "_"
  )

# add COVID indicator (year 1 = 2020 onwards)
covid_vars <- paste0("covid_", 0:4)
wide_data[covid_vars] <- 0
wide_data[, c("covid_1", "covid_2", "covid_3", "covid_4")] <- 1

# add time splines - create once for all waves
ns_matrix <- ns(0:4, df = 3)
for (wave in 0:4) {
  for (j in 1:3) {
    wide_data[paste0("ns_time_", wave, "_", j)] <- ns_matrix[wave + 1, j]
  }
}

# check structure
cat("Wide data dimensions:", nrow(wide_data), "x", ncol(wide_data), "\n")
cat("Variables:", paste(names(wide_data)[1:15], collapse = ", "), "...\n\n")

# ========================================================================
# METHOD 1: STANDARD MICE (for comparison)
# ========================================================================
cat("=== RUNNING STANDARD MICE ===\n")

# set up predictor matrix
predM <- make.predictorMatrix(wide_data)

# ensure time-ordering
trust_vars <- grep("trust_", names(wide_data))
for (i in trust_vars) {
  for (j in trust_vars) {
    wave_i <- as.numeric(sub(".*_", "", names(wide_data)[i]))
    wave_j <- as.numeric(sub(".*_", "", names(wide_data)[j]))
    if (!is.na(wave_i) && !is.na(wave_j) && wave_j > wave_i) {
      predM[i, j] <- 0
    }
  }
}

# don't use id as predictor
predM[, "id"] <- 0
predM["id", ] <- 0

# ensure COVID and time splines predict trust
covid_cols <- grep("covid_", names(wide_data))
ns_cols <- grep("ns_time_", names(wide_data))
predM[trust_vars, c(covid_cols, ns_cols)] <- 1

# don't impute COVID and time variables
predM[c(covid_cols, ns_cols), ] <- 0

start_time <- Sys.time()
mice_pmm <- mice(wide_data, predictorMatrix = predM, method = "pmm", 
                 m = 5, maxit = 10, printFlag = FALSE)
cat("MICE PMM time:", round(difftime(Sys.time(), start_time, units = "secs"), 1), "seconds\n")

# ========================================================================
# METHOD 2: MICERANGER
# ========================================================================
cat("\n=== RUNNING MICERANGER ===\n")

# prepare data for miceRanger
# miceRanger needs character factors
ranger_data <- wide_data
ranger_data$gender <- as.character(ranger_data$gender)
ranger_data$ethnicity <- as.character(ranger_data$ethnicity)

# specify which variables to impute
vars_to_impute <- names(ranger_data)[grep("trust_science_|trust_scientists_", names(ranger_data))]
cat("Variables to impute:", length(vars_to_impute), "\n")

# run miceRanger with custom predictor matrix
# set parallel = FALSE to avoid backend issues
start_time <- Sys.time()
mice_ranger <- miceRanger(
  ranger_data,
  vars = vars_to_impute,
  predictorMatrix = predM,
  num.trees = 100,
  m = 5,
  maxiter = 10,
  verbose = 1,
  parallel = FALSE  # changed to FALSE
)
cat("\nmiceRanger time:", round(difftime(Sys.time(), start_time, units = "secs"), 1), "seconds\n")

# ========================================================================
# COMPARE RESULTS
# ========================================================================
cat("\n\n=== COMPARING RESULTS ===\n")

# oracle trajectory
mod_oracle <- geeglm(trust_science ~ ns(years, 3), id = id, data = oracle_long)
pred_oracle <- predict_response(mod_oracle, "years[all]")

# observed trajectory
observed_complete <- long_data[!is.na(long_data$trust_science),]
mod_observed <- geeglm(trust_science ~ ns(years, 3), id = id, data = observed_complete)
pred_observed <- predict_response(mod_observed, "years[all]")

# function to get trajectory from wide imputed data
get_trajectory_wide <- function(imp_obj, method_name, is_ranger = FALSE) {
  preds <- lapply(1:5, function(i) {
    if (is_ranger) {
      # miceRanger returns all datasets at once
      all_datasets <- completeData(imp_obj)
      imp_wide <- all_datasets[[i]]
    } else {
      imp_wide <- complete(imp_obj, i)
    }
    
    # convert to long
    imp_long <- imp_wide %>%
      pivot_longer(
        cols = matches("trust_science_[0-9]|trust_scientists_[0-9]"),
        names_to = c(".value", "years"),
        names_pattern = "(.*)_(\\d+)"
      ) %>%
      mutate(years = as.numeric(years)) %>%
      arrange(id, years)
    
    mod <- geeglm(trust_science ~ ns(years, 3), id = id, data = imp_long)
    predict_response(mod, "years[all]")
  })
  
  pred_avg <- rowMeans(sapply(preds, function(p) p$predicted))
  
  data.frame(
    method = method_name,
    year = 0:4,
    predicted = pred_avg,
    change = pred_avg[5] - pred_avg[1]
  )
}

# get trajectories
traj_mice <- get_trajectory_wide(mice_pmm, "MICE PMM")
traj_ranger <- get_trajectory_wide(mice_ranger, "miceRanger", is_ranger = TRUE)

# print results
cat("\nTrust in Science Trajectories (Year 0 → Year 4):\n")
cat("─────────────────────────────────────────────────\n")

cat(sprintf("%-15s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    "Oracle:",
    pred_oracle$predicted[1],
    pred_oracle$predicted[5],
    pred_oracle$predicted[5] - pred_oracle$predicted[1]))

cat(sprintf("%-15s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    "Observed:",
    pred_observed$predicted[1],
    pred_observed$predicted[5],
    pred_observed$predicted[5] - pred_observed$predicted[1]))

cat("─────────────────────────────────────────────────\n")

cat(sprintf("%-15s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    paste0(traj_mice$method[1], ":"),
    traj_mice$predicted[1],
    traj_mice$predicted[5],
    traj_mice$change[1]))

cat(sprintf("%-15s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    paste0(traj_ranger$method[1], ":"),
    traj_ranger$predicted[1],
    traj_ranger$predicted[5],
    traj_ranger$change[1]))

# ========================================================================
# DIAGNOSTIC: Check imputation quality
# ========================================================================
cat("\n\n=== IMPUTATION DIAGNOSTICS ===\n")

# check one imputation
imp1_ranger <- completeData(mice_ranger)[[1]]  # miceRanger returns a list of datasets
imp1_ranger_long <- imp1_ranger %>%
  pivot_longer(
    cols = matches("trust_science_[0-9]"),
    names_to = c(".value", "years"),
    names_pattern = "(.*)_(\\d+)"
  ) %>%
  mutate(years = as.numeric(years))

# correlation between waves
cat("\nmiceRanger - Correlation between waves:\n")
cor_matrix <- imp1_ranger %>%
  dplyr::select(starts_with("trust_science_")) %>%
  dplyr::select(-contains("scientists")) %>%
  cor(use = "complete.obs")
print(round(cor_matrix, 3))

# check if demographics predict trust
demo_model <- lm(trust_science ~ education + age_baseline + gender + ethnicity, 
                 data = imp1_ranger_long)
cat("\nR² of demographics predicting trust (miceRanger):", 
    round(summary(demo_model)$r.squared, 3), "\n")

cat("\nDone! miceRanger provides another powerful option for longitudinal imputation.\n")