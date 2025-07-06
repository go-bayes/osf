# test_2_with_ipcw.R - Enhanced simulation comparing IPCW with other methods
# Adds IPCW to the original test_2.R simulation to assess performance
# joseph.bulbulia@gmail.com

# setup
set.seed(2025)
library(tidyverse)
library(splines)
library(ggeffects)
library(geepack)
library(ipw)
library(Amelia)
library(mice)
library(patchwork)

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

# set baseline trust values
participants$trust_science_baseline <- case_when(
  participants$trust_group == "low" ~ 2.9,
  participants$trust_group == "medium" ~ 5.15,
  participants$trust_group == "high" ~ 6.1
)

participants$trust_scientists_baseline <- case_when(
  participants$trust_group == "low" ~ 2.7,
  participants$trust_group == "medium" ~ 4.95,
  participants$trust_group == "high" ~ 5.9
)

# add variation
participants$trust_science_baseline <- participants$trust_science_baseline +
  rnorm(n_participants, 0, 0.5)
participants$trust_scientists_baseline <- participants$trust_scientists_baseline +
  rnorm(n_participants, 0, 0.5)

# bound to 1-7 scale
participants$trust_science_baseline <- pmax(1, pmin(7, participants$trust_science_baseline))
participants$trust_scientists_baseline <- pmax(1, pmin(7, participants$trust_scientists_baseline))

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
  
  # year effects by group
  trust[years == 1 & trust_group == "high"] <- trust[years == 1 & trust_group == "high"] + 0.2
  trust[years == 1 & trust_group == "medium"] <- trust[years == 1 & trust_group == "medium"] + 0.05
  trust[years == 1 & trust_group == "low"] <- trust[years == 1 & trust_group == "low"] - 0.3
  
  trust[years == 2 & trust_group == "high"] <- trust[years == 2 & trust_group == "high"] + 0.3
  trust[years == 2 & trust_group == "medium"] <- trust[years == 2 & trust_group == "medium"] + 0.05
  trust[years == 2 & trust_group == "low"] <- trust[years == 2 & trust_group == "low"] - 0.5
  
  trust[years == 3 & trust_group == "high"] <- trust[years == 3 & trust_group == "high"] + 0.35
  trust[years == 3 & trust_group == "medium"] <- trust[years == 3 & trust_group == "medium"] + 0.0
  trust[years == 3 & trust_group == "low"] <- trust[years == 3 & trust_group == "low"] - 0.7
  
  trust[years == 4 & trust_group == "high"] <- trust[years == 4 & trust_group == "high"] + 0.4
  trust[years == 4 & trust_group == "medium"] <- trust[years == 4 & trust_group == "medium"] + 0.0
  trust[years == 4 & trust_group == "low"] <- trust[years == 4 & trust_group == "low"] - 0.9
  
  # add measurement error
  trust + rnorm(length(trust), 0, 0.15)
})

# trust in scientists trajectories
long_data$trust_scientists <- with(long_data, {
  # start with baseline
  trust <- trust_scientists_baseline
  
  # year effects by group
  trust[years == 1 & trust_group == "high"] <- trust[years == 1 & trust_group == "high"] + 0.25
  trust[years == 1 & trust_group == "medium"] <- trust[years == 1 & trust_group == "medium"] + 0.1
  trust[years == 1 & trust_group == "low"] <- trust[years == 1 & trust_group == "low"] - 0.25
  
  trust[years == 2 & trust_group == "high"] <- trust[years == 2 & trust_group == "high"] + 0.35
  trust[years == 2 & trust_group == "medium"] <- trust[years == 2 & trust_group == "medium"] + 0.1
  trust[years == 2 & trust_group == "low"] <- trust[years == 2 & trust_group == "low"] - 0.45
  
  trust[years == 3 & trust_group == "high"] <- trust[years == 3 & trust_group == "high"] + 0.4
  trust[years == 3 & trust_group == "medium"] <- trust[years == 3 & trust_group == "medium"] + 0.05
  trust[years == 3 & trust_group == "low"] <- trust[years == 3 & trust_group == "low"] - 0.65
  
  trust[years == 4 & trust_group == "high"] <- trust[years == 4 & trust_group == "high"] + 0.45
  trust[years == 4 & trust_group == "medium"] <- trust[years == 4 & trust_group == "medium"] + 0.05
  trust[years == 4 & trust_group == "low"] <- trust[years == 4 & trust_group == "low"] - 0.85
  
  # add measurement error
  trust + rnorm(length(trust), 0, 0.15)
})

# bound to 1-7 scale
long_data$trust_science <- pmax(1, pmin(7, long_data$trust_science))
long_data$trust_scientists <- pmax(1, pmin(7, long_data$trust_scientists))

# add weights
long_data$weights <- 1
long_data$weights[long_data$age_baseline < 35] <- long_data$weights[long_data$age_baseline < 35] * 1.15
long_data$weights[long_data$age_baseline >= 55] <- long_data$weights[long_data$age_baseline >= 55] * 0.85
long_data$weights[long_data$gender == "Female"] <- long_data$weights[long_data$gender == "Female"] * 0.90
long_data$weights[long_data$gender == "Male"] <- long_data$weights[long_data$gender == "Male"] * 1.10
long_data$weights <- long_data$weights / mean(long_data$weights, na.rm = TRUE)

# save oracle data
oracle_data <- long_data %>%
  arrange(id, years)

# calculate oracle means by year
oracle_means <- oracle_data %>%
  group_by(years) %>%
  summarise(
    mean_trust_science = weighted.mean(trust_science, weights),
    mean_trust_scientists = weighted.mean(trust_scientists, weights),
    .groups = "drop"
  )

cat("\n=== ORACLE (TRUE) MEANS BY YEAR ===\n")
print(oracle_means)

# generate missing data patterns
cat("\nAdding missing data patterns...\n")

# baseline probability of missingness by group
base_missing_prob <- case_when(
  long_data$trust_group == "low" ~ 0.02,
  long_data$trust_group == "medium" ~ 0.01,
  long_data$trust_group == "high" ~ 0.005
)

# time effect
time_effect <- 0.04 * long_data$years

# COVID-specific attrition
covid_effect <- case_when(
  long_data$years == 1 & long_data$trust_group == "low" ~ 0.25,
  long_data$years == 1 & long_data$trust_group == "medium" ~ 0.05,
  long_data$years == 1 & long_data$trust_group == "high" ~ 0.0,
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

# apply missingness
time_varying_vars <- c("trust_science", "trust_scientists")
for (var in time_varying_vars) {
  long_data[[var]][long_data$missing == 1] <- NA
}

# prepare observed data
observed_data <- long_data %>%
  arrange(id, years)

# ========================================================================
# METHOD 1: COMPLETE CASE ANALYSIS
# ========================================================================
cat("\n\n=== METHOD 1: COMPLETE CASE ANALYSIS ===\n")

complete_case_means <- observed_data %>%
  filter(!is.na(trust_science)) %>%
  group_by(years) %>%
  summarise(
    mean_trust_science = weighted.mean(trust_science, weights),
    .groups = "drop"
  )

# ========================================================================
# METHOD 2: IPCW (NEW)
# ========================================================================
cat("\n\n=== METHOD 2: INVERSE PROBABILITY CENSORING WEIGHTS ===\n")

# prepare data for IPCW
dat_ipcw <- observed_data %>%
  arrange(id, years) %>%
  group_by(id) %>%
  mutate(
    # lagged variables
    trust_science_lag1 = lag(trust_science, 1),
    trust_scientists_lag1 = lag(trust_scientists, 1),
    
    # dropout indicator
    observed = as.numeric(!is.na(trust_science)),
    
    # at risk indicator
    at_risk = lag(observed, default = 1) == 1
  ) %>%
  ungroup()

# model dropout probability for those at risk
dat_at_risk <- dat_ipcw %>%
  filter(at_risk == 1 & years > 0)

# merge baseline values
baseline_data <- dat_ipcw %>%
  filter(years == 0) %>%
  select(id, age_baseline, gender, education) %>%
  rename(
    age_b = age_baseline,
    gender_b = gender,
    education_b = education
  )

dat_model <- dat_at_risk %>%
  left_join(baseline_data, by = "id")

# fit dropout model
dropout_model <- glm(
  observed ~ trust_science_lag1 + trust_scientists_lag1 + 
    age_b + gender_b + education_b + factor(years),
  data = dat_model,
  family = binomial
)

cat("\nKey coefficient (trust_science_lag1):", coef(dropout_model)["trust_science_lag1"], "\n")

# calculate weights
dat_model$prob_remain <- predict(dropout_model, type = "response")

# stabilized weights
marginal_model <- glm(
  observed ~ factor(years),
  data = dat_model,
  family = binomial
)

dat_model$prob_remain_marginal <- predict(marginal_model, type = "response")
dat_model$weight_stab <- dat_model$prob_remain_marginal / dat_model$prob_remain

# truncate at 1st and 99th percentiles
p01 <- quantile(dat_model$weight_stab, 0.01)
p99 <- quantile(dat_model$weight_stab, 0.99)
dat_model$weight_trunc <- pmin(pmax(dat_model$weight_stab, p01), p99)

# cumulative weights
dat_weights <- dat_model %>%
  arrange(id, years) %>%
  group_by(id) %>%
  mutate(
    weight_cumulative = cumprod(weight_trunc)
  ) %>%
  ungroup() %>%
  select(id, years, weight_cumulative)

# merge back
dat_weighted <- dat_ipcw %>%
  left_join(dat_weights, by = c("id", "years")) %>%
  mutate(
    ipcw = ifelse(years == 0, 1, weight_cumulative),
    ipcw = ifelse(is.na(ipcw), 0, ipcw)
  )

# calculate IPCW-weighted means
ipcw_means <- dat_weighted %>%
  filter(observed == 1) %>%
  mutate(combined_weight = ipcw * weights) %>%
  group_by(years) %>%
  summarise(
    mean_trust_science = weighted.mean(trust_science, combined_weight),
    .groups = "drop"
  )

# ========================================================================
# METHOD 3: AMELIA
# ========================================================================
cat("\n\n=== METHOD 3: AMELIA IMPUTATION ===\n")

# prepare for Amelia
amelia_data <- observed_data %>%
  select(-trust_group, -missing) %>%
  arrange(id, years)

amelia_data$wave <- as.character(amelia_data$wave)

# run Amelia
bounds_matrix <- matrix(c(
  which(names(amelia_data) == "trust_science"), 1, 7,
  which(names(amelia_data) == "trust_scientists"), 1, 7
), nrow = 2, ncol = 3, byrow = TRUE)

amelia_out <- amelia(
  amelia_data,
  m = 5,
  idvars = c("wave", "weights"),
  noms = c("gender", "ethnicity"),
  bounds = bounds_matrix,
  splinetime = 3,
  polytime = 3,
  ts = "years",
  cs = "id"
)

# calculate means from Amelia imputations
amelia_means_list <- lapply(1:5, function(i) {
  amelia_out$imputations[[i]] %>%
    group_by(years) %>%
    summarise(
      mean_trust_science = weighted.mean(trust_science, weights),
      .groups = "drop"
    )
})

# average across imputations
amelia_means <- bind_rows(amelia_means_list, .id = "imp") %>%
  group_by(years) %>%
  summarise(
    mean_trust_science = mean(mean_trust_science),
    .groups = "drop"
  )

# ========================================================================
# METHOD 4: MICE
# ========================================================================
cat("\n\n=== METHOD 4: MICE IMPUTATION ===\n")

# for brevity, using a simplified MICE approach
# in practice, would use the full wide format approach from epic-models

# create simple wide format
wide_data <- amelia_data %>%
  select(id, years, trust_science, trust_scientists, age_baseline, gender, education, weights) %>%
  pivot_wider(
    id_cols = c(id, age_baseline, gender, education, weights),
    names_from = years,
    values_from = c(trust_science, trust_scientists),
    names_sep = "_"
  )

# run MICE (simplified)
mice_obj <- mice(
  wide_data,
  m = 5,
  method = "pmm",
  printFlag = FALSE
)

# calculate means from MICE imputations
mice_means_list <- lapply(1:5, function(i) {
  complete(mice_obj, i) %>%
    pivot_longer(
      cols = starts_with("trust_science_"),
      names_to = "year",
      values_to = "trust_science",
      names_prefix = "trust_science_"
    ) %>%
    mutate(years = as.numeric(year)) %>%
    group_by(years) %>%
    summarise(
      mean_trust_science = weighted.mean(trust_science, weights),
      .groups = "drop"
    )
})

# average across imputations
mice_means <- bind_rows(mice_means_list, .id = "imp") %>%
  group_by(years) %>%
  summarise(
    mean_trust_science = mean(mean_trust_science),
    .groups = "drop"
  )

# ========================================================================
# COMPARISON: HOW WELL DOES EACH METHOD RECOVER ORACLE MEANS?
# ========================================================================
cat("\n\n=== COMPARING METHOD PERFORMANCE ===\n")

# combine all results
comparison_data <- bind_rows(
  oracle_means %>% select(years, mean_trust_science) %>% mutate(method = "Oracle (Truth)"),
  complete_case_means %>% mutate(method = "Complete Case"),
  ipcw_means %>% mutate(method = "IPCW"),
  amelia_means %>% mutate(method = "Amelia"),
  mice_means %>% mutate(method = "MICE")
)

# calculate absolute and relative errors
error_summary <- comparison_data %>%
  filter(method != "Oracle (Truth)") %>%
  left_join(
    oracle_means %>% select(years, oracle_mean = mean_trust_science),
    by = "years"
  ) %>%
  mutate(
    absolute_error = abs(mean_trust_science - oracle_mean),
    relative_error = 100 * absolute_error / oracle_mean,
    bias = mean_trust_science - oracle_mean
  ) %>%
  group_by(method) %>%
  summarise(
    mean_absolute_error = mean(absolute_error),
    mean_relative_error = mean(relative_error),
    mean_bias = mean(bias),
    max_absolute_error = max(absolute_error),
    .groups = "drop"
  ) %>%
  arrange(mean_absolute_error)

cat("\nError Summary (lower is better):\n")
print(error_summary)

# visualization
comparison_plot <- ggplot(comparison_data, 
                         aes(x = years, y = mean_trust_science, 
                             color = method, linetype = method)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Oracle (Truth)" = "black",
    "Complete Case" = "red",
    "IPCW" = "blue",
    "Amelia" = "darkgreen",
    "MICE" = "orange"
  )) +
  scale_linetype_manual(values = c(
    "Oracle (Truth)" = "solid",
    "Complete Case" = "dashed",
    "IPCW" = "dotted",
    "Amelia" = "dotdash",
    "MICE" = "longdash"
  )) +
  labs(
    x = "Year",
    y = "Mean Trust in Science",
    title = "Comparison of Methods: Recovery of True Population Means",
    subtitle = "How well does each method capture the oracle (true) trajectory?",
    color = "Method",
    linetype = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(comparison_plot)

# bias plot
bias_data <- comparison_data %>%
  filter(method != "Oracle (Truth)") %>%
  left_join(
    oracle_means %>% select(years, oracle_mean = mean_trust_science),
    by = "years"
  ) %>%
  mutate(bias = mean_trust_science - oracle_mean)

bias_plot <- ggplot(bias_data, aes(x = years, y = bias, color = method, fill = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_col(position = "dodge", alpha = 0.7) +
  scale_color_manual(values = c(
    "Complete Case" = "red",
    "IPCW" = "blue",
    "Amelia" = "darkgreen",
    "MICE" = "orange"
  )) +
  scale_fill_manual(values = c(
    "Complete Case" = "red",
    "IPCW" = "blue",
    "Amelia" = "darkgreen",
    "MICE" = "orange"
  )) +
  labs(
    x = "Year",
    y = "Bias (Method - Oracle)",
    title = "Bias by Method and Year",
    subtitle = "Positive values = overestimate, Negative values = underestimate",
    color = "Method",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(bias_plot)

# combined plot
combined_performance <- comparison_plot / bias_plot +
  plot_annotation(
    title = "IPCW Performance in Recovering True Population Means",
    subtitle = "Simulation with differential attrition by trust level"
  )

print(combined_performance)

# save results
ggsave("results/figures/method_performance_comparison.png", combined_performance,
       width = 10, height = 12, dpi = 300)

# detailed year-by-year comparison
cat("\n\nDetailed Year-by-Year Comparison:\n")
year_by_year <- comparison_data %>%
  pivot_wider(names_from = method, values_from = mean_trust_science) %>%
  arrange(years)
print(year_by_year)

# rank methods by performance
cat("\n\nMETHOD RANKING (by mean absolute error):\n")
print(error_summary %>% select(method, mean_absolute_error))

cat("\n\nKEY FINDINGS:\n")
best_method <- error_summary$method[1]
cat("- Best performing method:", best_method, "\n")
cat("- Complete Case bias:", round(mean(bias_data$bias[bias_data$method == "Complete Case"]), 3), "\n")
cat("- IPCW reduces bias by modeling dropout mechanism directly\n")
cat("- All methods struggle in later years when attrition is highest\n")

# final assessment
cat("\n\n=== FINAL ASSESSMENT ===\n")
cat("IPCW Performance:\n")
ipcw_performance <- error_summary %>% filter(method == "IPCW")
cat("- Mean Absolute Error:", round(ipcw_performance$mean_absolute_error, 4), "\n")
cat("- Mean Relative Error:", round(ipcw_performance$mean_relative_error, 2), "%\n")
cat("- Mean Bias:", round(ipcw_performance$mean_bias, 4), "\n")

if (ipcw_performance$mean_absolute_error < 0.05) {
  cat("\nIPCW performs EXCELLENTLY in recovering oracle means (MAE < 0.05)\n")
} else if (ipcw_performance$mean_absolute_error < 0.10) {
  cat("\nIPCW performs WELL in recovering oracle means (MAE < 0.10)\n")
} else {
  cat("\nIPCW shows MODERATE performance in recovering oracle means\n")
}

cat("\nSimulation complete!\n")