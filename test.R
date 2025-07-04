# always set a seed
set.seed(2025)


n_participants = 40000
baseline_year = 0
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
    education_effect * participants$education #+
   # random_components[, 1]

  participants$trust_scientists_baseline <- base_trust_scientists +
    education_effect * participants$education # +
    #random_components[, 2]

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

  n_waves = 5
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


###################################################

  ## THIS IS OUR ORACLE DATA
  long_data_oracle <- long_data

###################################################

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

  # post-stratification weights based on age × gender × ethnicity, but for simplicity
  # base weight
  long_data$weights <- 1
  long_data


# this is our oracle data
  library(splines)
  library(ggeffects)
  mod_test <- glm(trust_scientists ~ bs(years, 3), data = oracle_data)
  summary(mod_test)


  mod_test_2 <- glm(trust_scientists ~ bs(years, 3), data = observed_data)
  summary(mod_test_2)



plot(ggpredict(mod_test, terms="years[all]"))
plot(ggpredict(mod_test_2, terms="years[all]"))



## however

mod_observed <- geepack::geeglm(
  trust_science ~ ns(years, 3),
  id = id,
  data = observed_data,
  corstr = "exchangeable"
)

pred_observed <- predict_response(mod_observed, "years[all]")
cat("Observed trajectory:\n")
print(round(pred_observed$predicted, 3))


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




library(Amelia)
library(tidyverse)
head(long_data)

long_data<- long_data |> arrange(id, years)
head(long_data)
# id variables (removed from imputation model)
amelia_data <- long_data |> select(-c( "trust_scientists_baseline", "trust_science_baseline", "missing", "trust_science_baseline",'trust_scientists_baseline', "science_slope", "scientists_slope", "wave"))
# REMOVE tscore, tscore_i
id_vars <- c("id",  "years", "weights")

# nominal variables (categorical to be imputed)
nominal_vars <- c("gender", "ethnicity")

# run amelia (bounds for ordinal variables)
cat("Running Amelia with bounds for ordinal variables...\n")
bounds_matrix <- matrix(c(1, 1, 7, 2, 1, 7), nrow = 2, ncol = 3, byrow = TRUE)
rownames(bounds_matrix) <- c('trust_science', "trust_scientists")
nominal_vars
amelia_bounds <- Amelia::amelia(
  amelia_data,
  m = 3,
  idvars = id_vars,
  noms = nominal_vars,
  bounds = bounds_matrix
)


amelia_bounds
source(here::here("code/functions/margot_amelia_to_mice_fixed.R"))

cat("\nConverting Amelia log object to MICE...\n")
mids_data <- margot_amelia_to_mice_fixed(
  amelia_bounds,
  original_data = amelia_data,
  verbose = TRUE
)
mids_data$data$trust_scientists

gee_scientists_imputed <- lapply(1:3, function(i) {
  if (i == 1) cat("    Imputation", i)
  else if (i %% 5 == 0) cat("...", i)

  m <- geepack::geeglm(
    trust_scientists ~ ns(years, 3),
    id = id,
    weights = weights,
    data = mice::complete(mids_data, action = i),
    corstr = "exchangeable"
  )

  ggeffects::predict_response(
    m,
    "years[all]",
    margin = "marginalmeans",
    vcov_type = "robust"
  )
})
cat("\n  ✓ Trust in scientists GEE models fitted\n")



gee_predictions_scientists_imputed <- ggeffects::pool_predictions(gee_scientists_imputed)
gee_predictions_scientists_imputed
p_impute <- plot(gee_predictions_scientists_imputed) + scale_y_continuous(limits = c(1,7))


gee_scientists_oracle  <- geepack::geeglm(
    trust_scientists ~ ns(years, 3),
    id = id,
    # weights = weights,
    data = long_data_oracle,
    corstr = "exchangeable"
  )
gee_scientists_oracle
gee_predictions_scientists_oracle <- ggeffects::predict_response(gee_scientists_oracle,
    "years[all]",
    margin = "marginalmeans",
    vcov_type = "robust"
  )
gee_predictions_scientists_oracle


# observed
gee_scientists_observed  <-glm(
  trust_scientists ~ ns(years, 3),
 # id = id,
  # weights = weights,
  data = long_data#,
 # corstr = "exchangeable"
)

gee_predictions_scientists_observed <- ggeffects::predict_response(gee_scientists_observed,
                                                                 "years[all]",
                                                                 margin = "marginalmeans",
                                                                 vcov_type = "robust"
)
gee_predictions_scientists_observed


p_observed <- plot(gee_predictions_scientists_observed) + scale_y_continuous(limits = c(1,7))
p_observed
library(patchwork)
p_oracle + p_impute + p_observed


