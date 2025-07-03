# Generate Synthetic Data for Trust in Science Analysis
# This creates realistic example data that preserves key statistical properties
# while protecting individual privacy
# joseph.bulbulia@gmail.com

# function to generate synthetic trust data
generate_synthetic_trust_data <- function(
    n_participants = 42681,
    n_waves = 4,
    baseline_year = 2019,
    seed = 123) {
  
  set.seed(seed)
  
  cat("Generating synthetic NZAVS-like data...\n")
  cat("Participants:", n_participants, "\n")
  cat("Waves:", n_waves, "\n\n")
  
  # generate participant characteristics
  cat("Creating participant characteristics...\n")
  participants <- data.frame(
    id = 1:n_participants,
    # age distribution similar to nzavs
    age_baseline = round(rnorm(n_participants, mean = 48, sd = 15)),
    # gender (slightly more females as in nzavs)
    gender = sample(c("Female", "Male"), n_participants, 
                   replace = TRUE, prob = c(0.58, 0.42)),
    # ethnicity (approximate nz proportions)
    ethnicity = sample(c("NZ European", "Maori", "Pacific", "Asian", "Other"),
                      n_participants, replace = TRUE,
                      prob = c(0.70, 0.15, 0.08, 0.12, 0.05)),
    # education: 1-7 scale (1=no high school, 7=postgraduate)
    # realistic distribution: most people 3-5, fewer at extremes
    education = sample(1:7, n_participants, replace = TRUE,
                      prob = c(0.05, 0.10, 0.20, 0.25, 0.20, 0.15, 0.05))
  )
  
  # generate correlated baseline trust values
  # load MASS for multivariate normal simulation
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required for generating correlated data")
  }
  
  # generate trust values that are influenced by education
  # higher education -> higher baseline trust
  education_effect <- 0.3  # each unit of education adds 0.3 to trust
  
  # base trust levels (before education effect)
  base_trust_science <- 3.5      # lower base to allow education effect
  base_trust_scientists <- 3.4
  
  # create correlated random components
  sigma_science <- 0.8            # reduced SD since education adds variance
  sigma_scientists <- 0.7
  rho <- 0.7                      # correlation between the two measures
  
  # construct covariance matrix for random components
  Sigma <- matrix(c(
    sigma_science^2,
    rho * sigma_science * sigma_scientists,
    rho * sigma_science * sigma_scientists,
    sigma_scientists^2
  ), nrow = 2, byrow = TRUE)
  
  # draw correlated random components
  random_components <- MASS::mvrnorm(n_participants, mu = c(0, 0), Sigma = Sigma)
  
  # create baseline trust = base + education effect + random component
  participants$trust_science_baseline <- base_trust_science + 
                                       education_effect * participants$education +
                                       random_components[, 1]
  
  participants$trust_scientists_baseline <- base_trust_scientists + 
                                          education_effect * participants$education +
                                          random_components[, 2]
  
  # bound to 1-7 scale
  participants$trust_science_baseline <- pmax(1, pmin(7, participants$trust_science_baseline))
  participants$trust_scientists_baseline <- pmax(1, pmin(7, participants$trust_scientists_baseline))
  
  # individual-level random effects - CORRELATED slopes
  # people whose trust in science changes tend to have similar changes in trust in scientists
  slope_rho <- 0.6  # correlation between individual slopes
  slope_sd <- 0.1   # standard deviation of slopes
  
  # construct covariance matrix for slopes
  slope_sigma <- matrix(c(
    slope_sd^2,
    slope_rho * slope_sd * slope_sd,
    slope_rho * slope_sd * slope_sd,
    slope_sd^2
  ), nrow = 2, byrow = TRUE)
  
  # draw correlated slopes
  slope_mat <- MASS::mvrnorm(n_participants, 
                            mu = c(0, 0),  # mean slope of 0 for both
                            Sigma = slope_sigma)
  
  participants$science_slope <- slope_mat[, 1]
  participants$scientists_slope <- slope_mat[, 2]
  
  # create long format data
  cat("Creating longitudinal structure...\n")
  long_data <- expand.grid(
    id = participants$id,
    years = 0:(n_waves - 1),
    stringsAsFactors = FALSE
  )
  
  # merge with participant data
  long_data <- merge(long_data, participants, by = "id")
  
  # add wave variable as factor with character levels
  long_data$wave <- factor(baseline_year + long_data$years, 
                           levels = baseline_year:(baseline_year + n_waves - 1))
  
  # generate outcomes with realistic temporal patterns
  # based on observed trends: initial increase, then decline
  # education moderates the temporal trajectory
  cat("Generating trust outcomes with temporal trends...\n")
  
  # education effect on trajectories: lower education -> steeper decline
  # education 1-3 = steeper decline, 4-5 = moderate, 6-7 = shallower decline
  education_trajectory_effect <- (long_data$education - 4) * 0.05  # centered at 4
  
  # trust in science trajectory - STRONGER pattern
  # Year effects: positive = increase, negative = decrease
  long_data$trust_science <- with(long_data,
    trust_science_baseline +
    science_slope * years +
    0.30 * (years == 1) +    # COVID bump: +0.30
    0.10 * (years == 2) +    # Still elevated: +0.10  
    -0.25 * (years == 3) +   # Decline below baseline: -0.25
    education_trajectory_effect * years +  # education moderates change
    rnorm(nrow(long_data), 0, 0.4)  # measurement error
  )
  
  # trust in scientists trajectory - STRONGER pattern  
  long_data$trust_scientists <- with(long_data,
    trust_scientists_baseline +
    scientists_slope * years +
    0.35 * (years == 1) +    # COVID bump: +0.35
    0.05 * (years == 2) +    # Slight elevation: +0.05
    -0.30 * (years == 3) +   # Bigger decline: -0.30
    education_trajectory_effect * years +  # education moderates change
    rnorm(nrow(long_data), 0, 0.4)  # measurement error
  )
  
  # bound to 1-7 scale
  long_data$trust_science <- pmax(1, pmin(7, long_data$trust_science))
  long_data$trust_scientists <- pmax(1, pmin(7, long_data$trust_scientists))
  
  # DON'T create categorical versions here - they will be created after imputation
  # This avoids issues with factor variables in Amelia
  
  # generate realistic missing data patterns (revised to include trust in scientists)
  cat("Adding missing data patterns...\n")
  
  # baseline probability of missingness
  base_missing_prob <- 0.02  # reduced to ensure enough baseline data
  
  # increased missingness over time
  time_effect <- 0.10 * long_data$years
  
  # selective attrition: lower trust AND lower education -> higher dropout
  # high trust individuals tend to stay in the study
  # effect of trust in science (inverted: high trust = lower dropout)
  trust_science_effect <- 0.10 * (6 - long_data$trust_science)
  # effect of trust in scientists (inverted: high trust = lower dropout)
  trust_scientists_effect <- 0.10 * (6 - long_data$trust_scientists)
  # combined trust effect - both contribute to dropout
  trust_effect <- trust_science_effect + trust_scientists_effect
  
  # education effect: lower education -> higher dropout
  education_effect <- 0.08 * (5 - long_data$education)
  
  # age effect: younger more likely to drop out
  age_effect <- 0.002 * (50 - long_data$age_baseline)
  
  # COVID-SPECIFIC ATTRITION: Strong selection during wave 1 (2020)
  # Low trust people were VERY likely to drop out during COVID
  # High education (and thus high trust) people stayed in
  covid_wave <- (long_data$years == 1)  # year 1 is 2020
  
  # during COVID: low trust -> very high dropout
  # trust < 4 = 50% extra dropout, trust < 3 = 70% extra
  covid_trust_penalty <- covid_wave * pmax(0, 
    0.7 * (long_data$trust_science < 3) +
    0.5 * (long_data$trust_science >= 3 & long_data$trust_science < 4)
  )
  
  # during COVID: high education -> much more likely to stay
  # education 6-7 = 30% less dropout
  covid_education_bonus <- covid_wave * 0.3 * (long_data$education >= 6)
  
  # calculate missingness probability including trust and education effects
  missing_prob <- pmax(0, pmin(0.95,
    base_missing_prob + time_effect + trust_effect + education_effect + age_effect +
    covid_trust_penalty - covid_education_bonus
  ))
  
  # generate missing indicators
  long_data$missing <- rbinom(nrow(long_data), 1, missing_prob)
  
  # enforce monotone dropout - if missing at year t, also missing at t+1
  for (i in 2:n_waves) {
    year_t  <- long_data$years == (i - 1) & long_data$missing == 1
    year_t1 <- long_data$years == i
    same_id <- long_data$id %in% long_data$id[year_t]
    long_data$missing[year_t1 & same_id] <- 1
  }
  
  # apply missingness: blank trust variables when missing
  # but keep baseline year mostly complete for stability
  long_data$trust_science[long_data$missing == 1 & long_data$years > 0] <- NA
  long_data$trust_scientists[long_data$missing == 1 & long_data$years > 0] <- NA
  
  # apply minimal baseline missingness (max 5%)
  baseline_missing <- long_data$years == 0 & long_data$missing == 1
  n_baseline_missing <- sum(baseline_missing)
  max_baseline_missing <- round(0.05 * sum(long_data$years == 0))
  
  if (n_baseline_missing > max_baseline_missing) {
    # randomly keep some baseline observations
    keep_idx <- sample(which(baseline_missing), n_baseline_missing - max_baseline_missing)
    long_data$missing[keep_idx] <- 0
  }
  
  # now apply baseline missingness
  long_data$trust_science[long_data$missing == 1 & long_data$years == 0] <- NA
  long_data$trust_scientists[long_data$missing == 1 & long_data$years == 0] <- NA
  
  # ensure joint missingness in post-baseline years:
  # if either trust var is NA, set both to NA (realistic for survey dropout)
  idx_post <- long_data$years > 0
  idx_joint_missing <- idx_post & (
    is.na(long_data$trust_science) | 
    is.na(long_data$trust_scientists)
  )
  long_data[idx_joint_missing, c("trust_science", "trust_scientists")] <- NA
  
  # add post-stratification weights
  cat("Adding post-stratification weights...\n")
  
  # realistic post-stratification weights based on age × gender × ethnicity
  # base weight
  long_data$weights <- 1
  
  # age effects: young people are undersampled
  long_data$weights[long_data$age_baseline < 35] <- long_data$weights[long_data$age_baseline < 35] * 1.3
  long_data$weights[long_data$age_baseline >= 35 & long_data$age_baseline < 45] <- long_data$weights[long_data$age_baseline >= 35 & long_data$age_baseline < 45] * 1.1
  long_data$weights[long_data$age_baseline >= 55] <- long_data$weights[long_data$age_baseline >= 55] * 0.9
  
  # gender effects: males are undersampled
  long_data$weights[long_data$gender == "Male"] <- long_data$weights[long_data$gender == "Male"] * 1.2
  
  # ethnicity effects: maori slightly oversampled, pacific/asian undersampled
  long_data$weights[long_data$ethnicity == "Maori"] <- long_data$weights[long_data$ethnicity == "Maori"] * 0.95
  long_data$weights[long_data$ethnicity == "Pacific"] <- long_data$weights[long_data$ethnicity == "Pacific"] * 1.2
  long_data$weights[long_data$ethnicity == "Asian"] <- long_data$weights[long_data$ethnicity == "Asian"] * 1.15
  
  # interaction: young males get extra weight
  long_data$weights[long_data$age_baseline < 35 & long_data$gender == "Male"] <- long_data$weights[long_data$age_baseline < 35 & long_data$gender == "Male"] * 1.1
  
  # normalize weights to average 1.0
  long_data$weights <- long_data$weights / mean(long_data$weights, na.rm = TRUE)
  
  # save oracle data (complete data before missingness)
  # NOTE: Must be done AFTER creating all variables including weights
  oracle_data <- long_data[, c(
    "id", "wave", "years",
    "age_baseline", "gender", "ethnicity", "education", "weights",
    "trust_science", "trust_scientists"
  )]
  
  # save oracle data for comparison
  write.csv(oracle_data, 
            file = "oracle_trust_data.csv",
            row.names = FALSE)
  cat("Oracle data saved (before applying missingness)\n")
  
  # sort by id and years
  long_data <- long_data[order(long_data$id, long_data$years), ]
  
  # ensure no individual has ALL missing values (Amelia requirement)
  # check each person and ensure at least one observation
  for (person_id in unique(long_data$id)) {
    person_rows <- which(long_data$id == person_id)
    if (all(is.na(long_data$trust_science[person_rows]))) {
      # keep at least the baseline observation
      baseline_row <- person_rows[1]
      long_data$trust_science[baseline_row] <- oracle_data$trust_science[baseline_row]
      long_data$trust_scientists[baseline_row] <- oracle_data$trust_scientists[baseline_row]
    }
  }
  
  # calculate summary statistics
  cat("\nSummary of generated data:\n")
  cat("Total observations:", nrow(long_data), "\n")
  cat("Missing at year 0 (2019):", round(mean(is.na(long_data$trust_science[long_data$years == 0])), 3), "\n")
  cat("Missing at year 1 (2020/COVID):", round(mean(is.na(long_data$trust_science[long_data$years == 1])), 3), "\n")
  cat("Missing at year 2 (2021):", round(mean(is.na(long_data$trust_science[long_data$years == 2])), 3), "\n")
  cat("Missing at year 3 (2022):", round(mean(is.na(long_data$trust_science[long_data$years == (n_waves-1)])), 3), "\n")
  
  # show who drops out during COVID
  year0_complete <- long_data$years == 0 & !is.na(long_data$trust_science)
  year1_missing <- long_data$years == 1 & is.na(long_data$trust_science)
  covid_dropouts <- long_data$id[year1_missing] %in% long_data$id[year0_complete]
  
  if (any(covid_dropouts)) {
    dropout_ids <- unique(long_data$id[year1_missing][covid_dropouts])
    dropout_chars <- long_data[long_data$wave == 0 & long_data$id %in% dropout_ids, ]
    
    cat("\nCOVID wave dropouts (wave 0->1):", length(dropout_ids), "participants\n")
    cat("  Mean trust (dropouts):", round(mean(dropout_chars$trust_science, na.rm = TRUE), 2), "\n")
    cat("  Mean education (dropouts):", round(mean(dropout_chars$education), 2), "\n")
    
    # compare to those who stayed
    stayer_ids <- unique(long_data$id[long_data$wave == 1 & !is.na(long_data$trust_science)])
    stayer_chars <- long_data[long_data$wave == 0 & long_data$id %in% stayer_ids, ]
    cat("  Mean trust (stayers):", round(mean(stayer_chars$trust_science, na.rm = TRUE), 2), "\n")
    cat("  Mean education (stayers):", round(mean(stayer_chars$education), 2), "\n")
  }
  
  # return key columns - simplified structure
  # include education as a predictor variable
  synthetic_data <- long_data[, c(
    "id", "wave", "years",
    "age_baseline", "gender", "ethnicity", "education", "weights",
    "trust_science", "trust_scientists"
  )]
  
  return(synthetic_data)
}

# generate and save synthetic data
if (interactive()) {
  cat("\nGenerating synthetic dataset...\n")
  synthetic_data <- generate_synthetic_trust_data()
  
  # save as csv
  write.csv(synthetic_data, 
            file = "synthetic_trust_data.csv",
            row.names = FALSE)
  
  cat("\nSynthetic data saved to: synthetic_trust_data.csv\n")
  
  # show first few rows
  cat("\nFirst 10 rows:\n")
  print(head(synthetic_data, 10))
}