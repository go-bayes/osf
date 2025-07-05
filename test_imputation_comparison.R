# test_imputation_comparison.R - Compare Amelia vs MICE methods
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
library(patchwork)

# parameters (moderate sample for reasonable speed)
n_participants <- 10000
n_waves <- 5
baseline_year <- 0

cat("=== IMPUTATION METHOD COMPARISON ===\n")
cat("Sample size:", n_participants, "participants\n")
cat("Waves:", n_waves, "\n\n")

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
long_data <- expand.grid(
  id = participants$id,
  years = 0:(n_waves - 1),
  stringsAsFactors = FALSE
)

long_data <- merge(long_data, participants, by = "id")
long_data$wave <- factor(baseline_year + long_data$years)

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
oracle_data <- long_data %>% arrange(id, years)

# generate dropout based on current trust values
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

# ========================================================================
# ORACLE AND OBSERVED TRAJECTORIES
# ========================================================================
cat("Fitting oracle and observed models...\n")

# oracle
mod_oracle <- geeglm(trust_science ~ ns(years, 3), id = id, data = oracle_data)
pred_oracle <- predict_response(mod_oracle, "years[all]")

# observed
observed_complete <- observed_data[!is.na(observed_data$trust_science),]
mod_observed <- geeglm(trust_science ~ ns(years, 3), id = id, data = observed_complete)
pred_observed <- predict_response(mod_observed, "years[all]")

# ========================================================================
# METHOD 1: AMELIA
# ========================================================================
cat("\n=== METHOD 1: AMELIA ===\n")

# prepare for Amelia
amelia_data <- observed_data
amelia_data$wave <- as.character(amelia_data$wave)

# run Amelia
start_time <- Sys.time()
amelia_out <- amelia(
  amelia_data,
  m = 5,
  idvars = c("wave", "weights"),
  noms = c("gender", "ethnicity"),
  bounds = matrix(c(
    which(names(amelia_data) == "trust_science"), 1, 7,
    which(names(amelia_data) == "trust_scientists"), 1, 7
  ), nrow = 2, ncol = 3, byrow = TRUE),
  splinetime = 3,
  polytime = 3,
  ts = "years",
  cs = "id"
)
cat("Time taken:", round(difftime(Sys.time(), start_time, units = "secs"), 1), "seconds\n")

# get Amelia trajectory
amelia_preds <- lapply(1:5, function(i) {
  imp_data <- amelia_out$imputations[[i]]
  mod <- geeglm(trust_science ~ ns(years, 3), id = id, data = imp_data)
  predict_response(mod, "years[all]")
})
pred_amelia <- data.frame(
  predicted = rowMeans(sapply(amelia_preds, function(p) p$predicted))
)

# ========================================================================
# METHOD 2-4: MICE VARIANTS
# ========================================================================
cat("\n=== MICE METHODS ===\n")

# add COVID and time predictors
observed_data$covid <- as.integer(observed_data$years >= 1)
ns_matrix <- ns(observed_data$years, df = 3)
observed_data$ns_time_1 <- ns_matrix[,1]
observed_data$ns_time_2 <- ns_matrix[,2]
observed_data$ns_time_3 <- ns_matrix[,3]

# convert factors
observed_data$gender <- as.factor(observed_data$gender)
observed_data$ethnicity <- as.factor(observed_data$ethnicity)

# set up predictor matrix
predM <- make.predictorMatrix(observed_data)
predM[c("trust_science", "trust_scientists"), "covid"] <- 1
predM[c("trust_science", "trust_scientists"), c("ns_time_1", "ns_time_2", "ns_time_3")] <- 1
predM[, c("id", "wave")] <- 0
predM[c("id", "wave", "years", "covid", "ns_time_1", "ns_time_2", "ns_time_3"), ] <- 0

# run different MICE methods
methods <- c("pmm", "cart", "norm.predict")
mice_results <- list()

for (method in methods) {
  cat("\nRunning MICE with", method, "...\n")
  start_time <- Sys.time()
  
  mice_obj <- mice(
    observed_data,
    predictorMatrix = predM,
    method = method,
    m = 5,
    maxit = 10,
    printFlag = FALSE
  )
  
  cat("Time taken:", round(difftime(Sys.time(), start_time, units = "secs"), 1), "seconds\n")
  
  # get predictions
  preds <- lapply(1:5, function(i) {
    dat_imp <- complete(mice_obj, i) %>% arrange(id, years)
    mod <- geeglm(trust_science ~ ns(years, 3), id = id, data = dat_imp)
    predict_response(mod, "years[all]")
  })
  
  mice_results[[method]] <- data.frame(
    method = method,
    predicted = rowMeans(sapply(preds, function(p) p$predicted))
  )
}

# ========================================================================
# RESULTS SUMMARY
# ========================================================================
cat("\n\n=== RESULTS SUMMARY ===\n")
cat("\nTrust in Science Trajectories (Year 0 → Year 4):\n")
cat("─────────────────────────────────────────────────\n")

# oracle
cat(sprintf("%-15s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    "Oracle (Truth):",
    pred_oracle$predicted[1],
    pred_oracle$predicted[5],
    pred_oracle$predicted[5] - pred_oracle$predicted[1]))

# observed
cat(sprintf("%-15s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    "Observed:",
    pred_observed$predicted[1],
    pred_observed$predicted[5],
    pred_observed$predicted[5] - pred_observed$predicted[1]))

cat("─────────────────────────────────────────────────\n")

# amelia
cat(sprintf("%-15s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
    "Amelia:",
    pred_amelia$predicted[1],
    pred_amelia$predicted[5],
    pred_amelia$predicted[5] - pred_amelia$predicted[1]))

# mice methods
for (method in names(mice_results)) {
  result <- mice_results[[method]]
  cat(sprintf("%-15s Year 0: %.3f  Year 4: %.3f  Change: %+.3f\n",
      paste0("MICE ", method, ":"),
      result$predicted[1],
      result$predicted[5],
      result$predicted[5] - result$predicted[1]))
}

# ========================================================================
# VISUALIZATION
# ========================================================================
cat("\n\nCreating comparison plot...\n")

# combine all results
all_results <- rbind(
  data.frame(method = "Oracle", years = 0:4, predicted = pred_oracle$predicted),
  data.frame(method = "Observed", years = 0:4, predicted = pred_observed$predicted),
  data.frame(method = "Amelia", years = 0:4, predicted = pred_amelia$predicted)
)

for (m in names(mice_results)) {
  all_results <- rbind(all_results,
    data.frame(method = paste("MICE", m), years = 0:4, predicted = mice_results[[m]]$predicted)
  )
}

# create plot
p <- ggplot(all_results, aes(x = years, y = predicted, color = method, linetype = method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Oracle" = "darkgreen",
    "Observed" = "red",
    "Amelia" = "blue",
    "MICE pmm" = "purple",
    "MICE cart" = "orange",
    "MICE norm.predict" = "brown"
  )) +
  scale_linetype_manual(values = c(
    "Oracle" = "solid",
    "Observed" = "dashed",
    "Amelia" = "solid",
    "MICE pmm" = "solid",
    "MICE cart" = "solid",
    "MICE norm.predict" = "solid"
  )) +
  labs(
    title = "Comparison of Imputation Methods",
    subtitle = "Recovery of population trend under selective attrition",
    x = "Year",
    y = "Trust in Science (1-7)",
    color = "Method",
    linetype = "Method"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))

print(p)
ggsave("results/figures/imputation_comparison.png", p, width = 10, height = 6, dpi = 300)

cat("\nPlot saved to results/figures/imputation_comparison.png\n")
cat("\nAnalysis complete!\n")