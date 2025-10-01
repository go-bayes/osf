# Generate Synthetic Data for Trust in Science Analysis
# This creates realistic example data that preserves key statistical properties
# while protecting individual privacy
# joseph.bulbulia@gmail.com

# function to generate synthetic trust data
generate_synthetic_trust_data <- function(
    n_participants = 40000,
    n_waves = 5,  # changed to 5 waves like test_2.R
    baseline_year = 2019,
    seed = 2025) {  # use same seed as test_2.R

  set.seed(seed)

  # load required libraries
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data generation")
  }
  library(dplyr)

  cat("Generating synthetic NZAVS-like data...\n")
  cat("Participants:", n_participants, "\n")
  cat("Waves:", n_waves, "\n\n")

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

  # create trust groups based on education (like test_2.R)
  # education 1-2 -> low trust
  # education 3-5 -> medium trust
  # education 6-7 -> high trust
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
  print(aggregate(trust_science_baseline ~ trust_group, data = participants,
                  function(x) round(c(mean = mean(x), n = length(x)), 2)))

  # create long format data
  cat("Creating longitudinal structure...\n")
  long_data <- expand.grid(
    id = participants$id,
    years = 0:(n_waves - 1),
    stringsAsFactors = FALSE
  )

  # (note: already merged above to access trust_group)

  # merge with participant data (need trust_group for trajectories)
  long_data <- merge(long_data, participants, by = "id")

  # add wave variable as factor with character levels
  long_data$wave <- factor(baseline_year + long_data$years,
                           levels = baseline_year:(baseline_year + n_waves - 1))

  # generate trust trajectories by group (like test_2.R)
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

  # DON'T create categorical versions here - they will be created after imputation
  # This avoids issues with factor variables in Amelia

  # SAVE ORACLE DATA BEFORE APPLYING ANY MISSINGNESS
  # This is the ground truth - what we would see if there was no missing data
  oracle_data <- long_data[, c(
    "id", "wave", "years",
    "age_baseline", "gender", "ethnicity", "education", "trust_group",
    "trust_science", "trust_scientists"
  )]

  # Add weights to oracle data (weights are created after data generation)
  oracle_data$weights <- 1  # will be updated later

  # generate missing data patterns
  cat("Adding missing data patterns...\n")

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

  # enforce monotone dropout - if missing at year t, also missing at t+1
  for (i in 2:n_waves) {
    year_t  <- long_data$years == (i - 1) & long_data$missing == 1
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

  # define time-varying variables (these become NA when participant drops out)
  # note: weights are NOT time-varying - they're fixed at baseline
  time_varying_vars <- c("trust_science", "trust_scientists")

  # apply missingness to all time-varying variables when missing == 1
  for (var in time_varying_vars) {
    long_data[[var]][long_data$missing == 1] <- NA
  }

  # ensure joint missingness:
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

  # add post-stratification weights
  cat("Adding post-stratification weights...\n")

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

  # weights remain constant for each individual throughout the study
  # they represent the baseline sampling probability, not response probability
  # so we do NOT set them to NA when someone drops out

  # UPDATE ORACLE DATA WITH CORRECT WEIGHTS (before setting missing to NA)
  # oracle data should have complete weights
  oracle_data$weights <- long_data$weights
  oracle_data$weights[is.na(oracle_data$weights)] <- 1  # ensure oracle has complete weights

  # save oracle data for comparison (has complete data before missingness)
  write.csv(oracle_data,
            file = "data/synthetic/oracle_trust_data.csv",
            row.names = FALSE)
  cat("Oracle data saved (complete data before applying missingness)\n")

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
  cat("Missing at year 3 (2022):", round(mean(is.na(long_data$trust_science[long_data$years == 3])), 3), "\n")
  cat("Missing at year 4 (2023):", round(mean(is.na(long_data$trust_science[long_data$years == 4])), 3), "\n")

  # show who drops out during COVID
  year0_complete <- long_data$years == 0 & !is.na(long_data$trust_science)
  year1_missing <- long_data$years == 1 & is.na(long_data$trust_science)
  covid_dropouts <- long_data$id[year1_missing] %in% long_data$id[year0_complete]

  if (any(covid_dropouts)) {
    dropout_ids <- unique(long_data$id[year1_missing][covid_dropouts])
    dropout_chars <- long_data[long_data$years == 0 & long_data$id %in% dropout_ids, ]

    cat("\nCOVID wave dropouts (year 0->1):", length(dropout_ids), "participants\n")
    cat("  Mean trust (dropouts):", round(mean(dropout_chars$trust_science, na.rm = TRUE), 2), "\n")
    cat("  Mean education (dropouts):", round(mean(dropout_chars$education), 2), "\n")

    # compare to those who stayed
    stayer_ids <- unique(long_data$id[long_data$years == 1 & !is.na(long_data$trust_science)])
    stayer_chars <- long_data[long_data$years == 0 & long_data$id %in% stayer_ids, ]
    cat("  Mean trust (stayers):", round(mean(stayer_chars$trust_science, na.rm = TRUE), 2), "\n")
    cat("  Mean education (stayers):", round(mean(stayer_chars$education), 2), "\n")
  }

  # return key columns - simplified structure
  # NOTE: trust_group is excluded from synthetic data (only used for simulation)
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

