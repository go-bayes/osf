# test_mice_wide_format.R - Proper MICE implementation with wide format
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(MASS)
library(mice)
library(Amelia)
library(splines)
library(ggeffects)
library(geepack)

# parameters
n_participants <- 5000
n_waves <- 5

cat("=== MICE WITH PROPER WIDE FORMAT ===\n")
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

# save oracle (long format)
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
# CONVERT TO WIDE FORMAT FOR MICE
# ========================================================================
cat("\nConverting to wide format for MICE...\n")

# create wide format with trust_science_0, trust_science_1, etc.
wide_data <- long_data %>%
  dplyr::select(id, years, trust_science, trust_scientists, 
                age_baseline, gender, ethnicity, education) %>%
  pivot_wider(
    id_cols = c(id, age_baseline, gender, ethnicity, education),
    names_from = years,
    values_from = c(trust_science, trust_scientists),
    names_sep = "_"
  )

# check structure
cat("Wide data structure:\n")
cat("Dimensions:", nrow(wide_data), "x", ncol(wide_data), "\n")
cat("Variables:", paste(names(wide_data)[1:10], collapse = ", "), "...\n")

# check missingness pattern
missing_pattern <- colMeans(is.na(wide_data))
cat("\nMissingness by wave:\n")
print(round(missing_pattern[grep("trust_science_", names(missing_pattern))], 3))

# ========================================================================
# MICE ON WIDE FORMAT
# ========================================================================
cat("\n=== RUNNING MICE ON WIDE FORMAT ===\n")

# create predictor matrix for proper longitudinal imputation
predM <- make.predictorMatrix(wide_data)

# ensure time-ordering: later waves can't predict earlier waves
trust_vars <- grep("trust_", names(wide_data))
for (i in trust_vars) {
  for (j in trust_vars) {
    # extract wave numbers
    wave_i <- as.numeric(sub(".*_", "", names(wide_data)[i]))
    wave_j <- as.numeric(sub(".*_", "", names(wide_data)[j]))
    
    # later waves can't predict earlier waves
    if (wave_j > wave_i) {
      predM[i, j] <- 0
    }
  }
}

# don't use id as predictor
predM[, "id"] <- 0
predM["id", ] <- 0

# run different MICE methods
methods <- c("pmm", "cart", "norm.predict")
mice_results_wide <- list()

for (method in methods) {
  cat("\nRunning MICE", method, "on wide format...\n")
  start_time <- Sys.time()
  
  mice_obj <- mice(
    wide_data,
    predictorMatrix = predM,
    method = method,
    m = 5,
    maxit = 10,
    printFlag = FALSE
  )
  
  cat("Time taken:", round(difftime(Sys.time(), start_time, units = "secs"), 1), "seconds\n")
  
  # convert back to long format for analysis
  imputed_long_list <- lapply(1:5, function(m) {
    imp_wide <- complete(mice_obj, m)
    
    # first check what variables we have
    if (m == 1 && method == "pmm") {
      cat("Variables in imputed wide data:", paste(names(imp_wide)[1:10], collapse=", "), "...\n")
    }
    
    # convert to long - need to handle both trust_science and trust_scientists
    imp_long <- imp_wide %>%
      pivot_longer(
        cols = matches("trust_science_[0-9]|trust_scientists_[0-9]"),
        names_to = c(".value", "years"),
        names_pattern = "(.*)_(\\d+)"
      ) %>%
      mutate(years = as.numeric(years)) %>%
      arrange(id, years)
    
    # check result
    if (m == 1 && method == "pmm") {
      cat("Variables in imputed long data:", paste(names(imp_long), collapse=", "), "\n")
      cat("First few rows:\n")
      print(head(imp_long, 3))
    }
    
    return(imp_long)
  })
  
  mice_results_wide[[method]] <- imputed_long_list
}

# ========================================================================
# COMPARE WIDE VS LONG FORMAT
# ========================================================================
cat("\n\n=== COMPARING RESULTS ===\n")

# get oracle trajectory
mod_oracle <- geeglm(trust_science ~ ns(years, 3), id = id, data = oracle_long)
pred_oracle <- predict_response(mod_oracle, "years[all]")

# get observed trajectory
observed_complete <- long_data[!is.na(long_data$trust_science),]
mod_observed <- geeglm(trust_science ~ ns(years, 3), id = id, data = observed_complete)
pred_observed <- predict_response(mod_observed, "years[all]")

# function to get trajectory
get_trajectory_from_list <- function(imp_list, method_name) {
  preds <- lapply(imp_list, function(dat_imp) {
    mod <- geeglm(trust_science ~ ns(years, 3), id = id, data = dat_imp)
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

# results summary
cat("\nTrust in Science Trajectories (Year 0 → Year 4):\n")
cat("─────────────────────────────────────────────────\n")

cat(sprintf("%-20s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    "Oracle (Truth):",
    pred_oracle$predicted[1],
    pred_oracle$predicted[5],
    pred_oracle$predicted[5] - pred_oracle$predicted[1]))

cat(sprintf("%-20s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    "Observed:",
    pred_observed$predicted[1],
    pred_observed$predicted[5],
    pred_observed$predicted[5] - pred_observed$predicted[1]))

cat("─────────────────────────────────────────────────\n")
cat("MICE with WIDE format (proper longitudinal):\n")

for (method in names(mice_results_wide)) {
  traj <- get_trajectory_from_list(mice_results_wide[[method]], paste("MICE", method))
  cat(sprintf("%-20s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
      paste0(traj$method[1], ":"),
      traj$predicted[1],
      traj$predicted[5],
      traj$change[1]))
}

# ========================================================================
# ALSO RUN AMELIA FOR COMPARISON
# ========================================================================
cat("\n─────────────────────────────────────────────────\n")
cat("Running Amelia (handles long format natively)...\n")

amelia_data <- long_data %>%
  dplyr::select(id, years, age_baseline, gender, ethnicity, education, 
                trust_science, trust_scientists) %>%
  arrange(id, years)

amelia_out <- amelia(
  amelia_data,
  m = 5,
  noms = c("gender", "ethnicity"),
  bounds = matrix(c(7, 1, 7, 8, 1, 7), nrow = 2, ncol = 3, byrow = TRUE),
  splinetime = 3,
  polytime = 3,
  ts = "years",
  cs = "id"
)

amelia_preds <- lapply(1:5, function(i) {
  imp_data <- amelia_out$imputations[[i]]
  mod <- geeglm(trust_science ~ ns(years, 3), id = id, data = imp_data)
  predict_response(mod, "years[all]")
})

pred_amelia <- rowMeans(sapply(amelia_preds, function(p) p$predicted))

cat(sprintf("%-20s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    "Amelia:",
    pred_amelia[1],
    pred_amelia[5],
    pred_amelia[5] - pred_amelia[1]))

# ========================================================================
# DIAGNOSTIC: Check imputation quality
# ========================================================================
cat("\n\n=== IMPUTATION DIAGNOSTICS ===\n")

# check one MICE wide imputation
imp1 <- mice_results_wide[["cart"]][[1]]
cat("\nMICE CART (wide format) - Correlation between waves:\n")
cor_matrix <- imp1 %>%
  dplyr::select(id, years, trust_science) %>%
  pivot_wider(names_from = years, values_from = trust_science, names_prefix = "wave_") %>%
  dplyr::select(-id) %>%
  cor(use = "complete.obs")
print(round(cor_matrix, 3))

cat("\nDone! Wide format MICE properly accounts for longitudinal structure.\n")