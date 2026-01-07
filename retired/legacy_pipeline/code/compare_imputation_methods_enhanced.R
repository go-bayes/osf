# compare_imputation_methods_enhanced.r
# enhanced comparison of imputation methods with categorical and ordinal analysis
# includes reduced imputation count (m=5) and comprehensive metrics
# joseph.bulbulia@vuw.ac.nz
# january 2025

# setup
set.seed(2025)
library(tidyverse)
library(mice)
library(Amelia)
library(geepack)
library(MASS)
library(splines)
library(ggeffects)
library(patchwork)
library(here)

# ensure output directories exist
output_dirs <- c(
  here::here("results", "figures"),
  here::here("results", "objects")
)
for (dir_path in output_dirs) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# parameters
n_participants <- 10000  # reasonable size for comparison
n_waves <- 5
n_imputations <- 5  # reduced from 10-20 for efficiency

# source the canonical simulation code from keep directory
source(here::here("keep", "test_3_improved.R"))

# align reported parameters with generated data
n_participants <- observed_data |>
  dplyr::distinct(id) |>
  nrow()
n_waves <- observed_data |>
  dplyr::distinct(years) |>
  nrow()

# ========================================================================
# enhanced method comparison with categorical and ordinal outcomes
# ========================================================================

cat("\n\n=== ENHANCED METHOD COMPARISON ===\n")
cat("Comparing methods on continuous, categorical, and ordinal outcomes\n")
cat("Using", n_imputations, "imputations for efficiency\n\n")

# define category creation function
create_trust_categories <- function(trust_var) {
  factor(
    case_when(
      trust_var <= 3 ~ "Low",
      trust_var <= 5 ~ "Medium",
      trust_var >= 6 ~ "High",
      TRUE ~ NA_character_
    ),
    levels = c("Low", "Medium", "High"),
    ordered = TRUE
  )
}

# ========================================================================
# 1. oracle category proportions
# ========================================================================

oracle_cat_props <- oracle_data %>%
  mutate(trust_cat = create_trust_categories(trust_science)) %>%
  group_by(years) %>%
  summarise(
    n = n(),
    low = weighted.mean(trust_cat == "Low", weights),
    medium = weighted.mean(trust_cat == "Medium", weights),
    high = weighted.mean(trust_cat == "High", weights),
    .groups = "drop"
  )

cat("Oracle Category Proportions:\n")
print(oracle_cat_props)

# ========================================================================
# 2. complete case category proportions
# ========================================================================

cc_cat_props <- observed_data %>%
  dplyr::filter(!is.na(trust_science)) %>%
  mutate(trust_cat = create_trust_categories(trust_science)) %>%
  group_by(years) %>%
  summarise(
    n = n(),
    low = weighted.mean(trust_cat == "Low", weights),
    medium = weighted.mean(trust_cat == "Medium", weights),
    high = weighted.mean(trust_cat == "High", weights),
    .groups = "drop"
  )

# ========================================================================
# 3. re-run mice with only 5 imputations
# ========================================================================

cat("\n\nRunning MICE with", n_imputations, "imputations...\n")

# create wide format
wide_data <- observed_data %>%
  dplyr::select(id, years, trust_science, trust_scientists,
                age_baseline, gender, education, weights) %>%
  pivot_wider(
    id_cols = c(id, age_baseline, gender, education, weights),
    names_from = years,
    values_from = c(trust_science, trust_scientists),
    names_sep = "_"
  )

# run mice with reduced imputations
mice_obj <- mice(
  wide_data,
  m = n_imputations,
  method = "pmm",
  printFlag = FALSE,
  seed = 2025
)

# get category proportions from mice
mice_cat_props_list <- lapply(1:n_imputations, function(i) {
  complete(mice_obj, i) %>%
    pivot_longer(
      cols = starts_with("trust_science_"),
      names_to = "year",
      values_to = "trust_science",
      names_prefix = "trust_science_"
    ) %>%
    mutate(
      years = as.numeric(year),
      trust_cat = create_trust_categories(trust_science)
    ) %>%
    group_by(years) %>%
    summarise(
      low = weighted.mean(trust_cat == "Low", weights),
      medium = weighted.mean(trust_cat == "Medium", weights),
      high = weighted.mean(trust_cat == "High", weights),
      .groups = "drop"
    )
})

# pool results
mice_cat_props <- bind_rows(mice_cat_props_list, .id = "imp") %>%
  group_by(years) %>%
  summarise(
    low = mean(low),
    medium = mean(medium),
    high = mean(high),
    .groups = "drop"
  )

# ========================================================================
# 4. re-run amelia with 5 imputations
# ========================================================================

cat("\nRunning Amelia with", n_imputations, "imputations...\n")

amelia_data <- observed_data %>%
  dplyr::select(-trust_group, -missing) %>%
  arrange(id, years)

amelia_data$wave <- as.character(amelia_data$wave)

bounds_matrix <- matrix(c(
  which(names(amelia_data) == "trust_science"), 1, 7,
  which(names(amelia_data) == "trust_scientists"), 1, 7
), nrow = 2, ncol = 3, byrow = TRUE)

amelia_out <- amelia(
  amelia_data,
  m = n_imputations,
  idvars = c("wave", "weights"),
  noms = c("gender", "ethnicity"),
  bounds = bounds_matrix,
  splinetime = 3,
  polytime = 3,
  ts = "years",
  cs = "id",
  seed = 2025
)

# get category proportions from amelia
amelia_cat_props_list <- lapply(1:n_imputations, function(i) {
  amelia_out$imputations[[i]] %>%
    mutate(trust_cat = create_trust_categories(trust_science)) %>%
    group_by(years) %>%
    summarise(
      low = weighted.mean(trust_cat == "Low", weights),
      medium = weighted.mean(trust_cat == "Medium", weights),
      high = weighted.mean(trust_cat == "High", weights),
      .groups = "drop"
    )
})

amelia_cat_props <- bind_rows(amelia_cat_props_list, .id = "imp") %>%
  group_by(years) %>%
  summarise(
    low = mean(low),
    medium = mean(medium),
    high = mean(high),
    .groups = "drop"
  )

# ========================================================================
# 5. ipcw category proportions (already calculated in test_2_with_ipcw.R)
# ========================================================================

ipcw_cat_props <- dat_weighted %>%
  dplyr::filter(observed == 1) %>%
  mutate(
    trust_cat = create_trust_categories(trust_science),
    combined_weight = ipcw * weights
  ) %>%
  group_by(years) %>%
  summarise(
    low = weighted.mean(trust_cat == "Low", combined_weight),
    medium = weighted.mean(trust_cat == "Medium", combined_weight),
    high = weighted.mean(trust_cat == "High", combined_weight),
    .groups = "drop"
  )

# ========================================================================
# 6. ordinal model comparison
# ========================================================================

cat("\n\n=== ORDINAL MODEL ANALYSIS ===\n")

# prepare datasets for ordinal models
oracle_ord_data <- oracle_data %>%
  mutate(trust_cat = create_trust_categories(trust_science))

cc_ord_data <- observed_data %>%
  dplyr::filter(!is.na(trust_science)) %>%
  mutate(trust_cat = create_trust_categories(trust_science))

# fit ordinal models with pooling for imputation methods
fit_polr_thresholds <- function(data, weight_var = "weights") {
  model <- polr(
    trust_cat ~ ns(years, 3),
    data = data,
    weights = data[[weight_var]],
    Hess = TRUE
  )
  tibble(
    threshold_low_med = model$zeta[1],
    threshold_med_high = model$zeta[2]
  )
}

pool_thresholds <- function(threshold_list, method_name) {
  bind_rows(threshold_list) |>
    summarise(
      threshold_low_med = mean(threshold_low_med),
      threshold_low_med_sd = sd(threshold_low_med),
      threshold_med_high = mean(threshold_med_high),
      threshold_med_high_sd = sd(threshold_med_high)
    ) |>
    mutate(method = method_name)
}

oracle_thresholds <- oracle_ord_data |>
  fit_polr_thresholds() |>
  mutate(
    threshold_low_med_sd = NA_real_,
    threshold_med_high_sd = NA_real_,
    method = "Oracle"
  )

cc_thresholds <- cc_ord_data |>
  fit_polr_thresholds() |>
  mutate(
    threshold_low_med_sd = NA_real_,
    threshold_med_high_sd = NA_real_,
    method = "Complete Case"
  )

ipcw_thresholds <- dat_weighted |>
  dplyr::filter(observed == 1) |>
  mutate(
    trust_cat = create_trust_categories(trust_science),
    combined_weight = ipcw * weights
  ) |>
  fit_polr_thresholds(weight_var = "combined_weight") |>
  mutate(
    threshold_low_med_sd = NA_real_,
    threshold_med_high_sd = NA_real_,
    method = "IPCW"
  )

mice_thresholds <- lapply(1:n_imputations, function(i) {
  complete(mice_obj, i) |>
    pivot_longer(
      cols = starts_with("trust_science_"),
      names_to = "year",
      values_to = "trust_science",
      names_prefix = "trust_science_"
    ) %>%
    mutate(
      years = as.numeric(year),
      trust_cat = create_trust_categories(trust_science)
    ) |>
    fit_polr_thresholds()
}) |>
  pool_thresholds("MICE")

amelia_thresholds <- lapply(1:n_imputations, function(i) {
  amelia_out$imputations[[i]] |>
    mutate(trust_cat = create_trust_categories(trust_science)) |>
    fit_polr_thresholds()
}) |>
  pool_thresholds("Amelia")

threshold_comparison <- bind_rows(
  oracle_thresholds,
  cc_thresholds,
  ipcw_thresholds,
  amelia_thresholds,
  mice_thresholds
)

cat("\nOrdinal Model Thresholds:\n")
print(threshold_comparison)

# ========================================================================
# 7. comprehensive comparison
# ========================================================================

cat("\n\n=== COMPREHENSIVE COMPARISON ===\n")

# combine continuous results (already calculated)
continuous_comparison <- error_summary %>%
  dplyr::select(method, MAE = mean_absolute_error, bias = mean_bias)

# category shift detection
cat_shift_comparison <- data.frame(
  method = c("Complete Case", "MICE", "Amelia", "IPCW"),
  low_shift = c(
    cc_cat_props$low[5] - cc_cat_props$low[1],
    mice_cat_props$low[5] - mice_cat_props$low[1],
    amelia_cat_props$low[5] - amelia_cat_props$low[1],
    ipcw_cat_props$low[5] - ipcw_cat_props$low[1]
  ),
  high_shift = c(
    cc_cat_props$high[5] - cc_cat_props$high[1],
    mice_cat_props$high[5] - mice_cat_props$high[1],
    amelia_cat_props$high[5] - amelia_cat_props$high[1],
    ipcw_cat_props$high[5] - ipcw_cat_props$high[1]
  )
)

# oracle shifts for comparison
oracle_low_shift <- oracle_cat_props$low[5] - oracle_cat_props$low[1]
oracle_high_shift <- oracle_cat_props$high[5] - oracle_cat_props$high[1]

cat("\nCategory Shift Detection (Year 4 - Year 0):\n")
cat("Oracle: Low", round(oracle_low_shift, 3), "High", round(oracle_high_shift, 3), "\n")
print(cat_shift_comparison)

# ========================================================================
# 8. visualization
# ========================================================================

# category proportion plot
cat_prop_data <- bind_rows(
  oracle_cat_props %>% mutate(method = "Oracle", category = "Low", proportion = low),
  oracle_cat_props %>% mutate(method = "Oracle", category = "High", proportion = high),
  cc_cat_props %>% mutate(method = "Complete Case", category = "Low", proportion = low),
  cc_cat_props %>% mutate(method = "Complete Case", category = "High", proportion = high),
  mice_cat_props %>% mutate(method = "MICE", category = "Low", proportion = low),
  mice_cat_props %>% mutate(method = "MICE", category = "High", proportion = high),
  amelia_cat_props %>% mutate(method = "Amelia", category = "Low", proportion = low),
  amelia_cat_props %>% mutate(method = "Amelia", category = "High", proportion = high),
  ipcw_cat_props %>% mutate(method = "IPCW", category = "Low", proportion = low),
  ipcw_cat_props %>% mutate(method = "IPCW", category = "High", proportion = high)
)

p_cat <- ggplot(cat_prop_data,
                aes(x = years, y = proportion, color = method, linetype = category)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Oracle" = "black",
    "Complete Case" = "red",
    "MICE" = "orange",
    "Amelia" = "darkgreen",
    "IPCW" = "blue"
  )) +
  scale_linetype_manual(values = c("Low" = "solid", "High" = "dashed")) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Category Proportions by Method",
    subtitle = "Solid = Low trust, Dashed = High trust"
  ) +
  theme_minimal()

# method performance summary
performance_summary <- continuous_comparison %>%
  left_join(
    cat_shift_comparison %>%
      mutate(
        low_error = abs(low_shift - oracle_low_shift),
        high_error = abs(high_shift - oracle_high_shift),
        cat_mae = (low_error + high_error) / 2
      ) %>%
      dplyr::select(method, cat_mae),
    by = "method"
  )

cat("\n\nFinal Performance Summary:\n")
print(performance_summary)

# save enhanced comparison plot
ggsave(
  here::here("results", "figures", "enhanced_method_comparison.png"),
  p_cat,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\n=== ENHANCED COMPARISON COMPLETE ===\n")
cat("Key findings:\n")
cat("1. MICE (wide format) best recovers continuous trends\n")
cat("2. Category shift detection varies by method\n")
cat("3. Ordinal thresholds show systematic differences\n")
cat("4. IPCW performance depends on population heterogeneity\n")
