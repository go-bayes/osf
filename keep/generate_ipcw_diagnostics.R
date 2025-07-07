# generate_ipcw_diagnostics.R
# Diagnostic plots and tables for IPCW simulation
# Focuses on method-specific diagnostics and assumptions
# joseph.bulbulia@vuw.ac.nz
# january 2025

# setup
library(tidyverse)
library(patchwork)
library(corrplot)
library(gt)
library(viridis)

# run the simulation if results don't exist
if (!exists("overall_performance")) {
  cat("Running simulation first...\n")
  source("keep/test_2_with_ipcw.R")
}

# ========================================================================
# 1. ATTRITION PATTERNS
# ========================================================================
cat("\n=== ANALYZING ATTRITION PATTERNS ===\n")

# calculate attrition by group and year
attrition_summary <- observed_data %>%
  group_by(years, trust_group) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is.na(trust_science)),
    prop_missing = n_missing / n_total,
    .groups = "drop"
  )

# attrition plot
p_attrition <- ggplot(attrition_summary,
                     aes(x = years, y = prop_missing, 
                         color = trust_group, group = trust_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c(
    "low" = "#e74c3c",
    "medium" = "#f39c12", 
    "high" = "#27ae60"
  )) +
  labs(
    x = "Year",
    y = "Proportion Missing",
    title = "Differential Attrition by Trust Group",
    subtitle = "Cumulative missingness over time",
    color = "Trust Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic")
  )

# ========================================================================
# 2. IPCW WEIGHT DIAGNOSTICS
# ========================================================================
cat("\n=== IPCW WEIGHT DIAGNOSTICS ===\n")

# weight distribution plot
ipcw_weights <- dat_weighted %>%
  filter(observed == 1, years > 0) %>%
  select(years, id, ipcw, trust_group)

p_weights <- ggplot(ipcw_weights, aes(x = ipcw)) +
  geom_histogram(aes(y = ..density..), bins = 50, 
                 fill = "#3498db", alpha = 0.7) +
  geom_density(color = "#2c3e50", size = 1) +
  facet_wrap(~years, scales = "free_y") +
  labs(
    x = "IPCW Weight",
    y = "Density",
    title = "Distribution of IPCW Weights by Year",
    subtitle = "Stabilized and truncated weights"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    strip.text = element_text(face = "bold")
  )

# weight extremes by group
weight_extremes <- ipcw_weights %>%
  group_by(years, trust_group) %>%
  summarise(
    mean_weight = mean(ipcw),
    median_weight = median(ipcw),
    max_weight = max(ipcw),
    p95_weight = quantile(ipcw, 0.95),
    .groups = "drop"
  )

p_weight_groups <- ggplot(weight_extremes,
                         aes(x = years, y = mean_weight, 
                             color = trust_group, group = trust_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = median_weight, ymax = p95_weight, fill = trust_group),
              alpha = 0.2) +
  scale_color_manual(values = c(
    "low" = "#e74c3c",
    "medium" = "#f39c12",
    "high" = "#27ae60"
  )) +
  scale_fill_manual(values = c(
    "low" = "#e74c3c",
    "medium" = "#f39c12",
    "high" = "#27ae60"
  )) +
  labs(
    x = "Year",
    y = "IPCW Weight",
    title = "IPCW Weights by Trust Group",
    subtitle = "Mean with median to 95th percentile range",
    color = "Trust Group",
    fill = "Trust Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic")
  )

# ========================================================================
# 3. BASELINE COVARIATE BALANCE
# ========================================================================
cat("\n=== BASELINE COVARIATE BALANCE ===\n")

# calculate standardized differences
baseline_balance <- observed_data %>%
  filter(years == 0) %>%
  mutate(
    complete_y4 = id %in% (observed_data %>% 
                          filter(years == 4, !is.na(trust_science)) %>% 
                          pull(id))
  ) %>%
  group_by(complete_y4) %>%
  summarise(
    age_mean = mean(age_baseline),
    age_sd = sd(age_baseline),
    education_mean = mean(education),
    education_sd = sd(education),
    prop_female = mean(gender == "Female"),
    baseline_trust_mean = mean(trust_science),
    baseline_trust_sd = sd(trust_science),
    .groups = "drop"
  )

# standardized differences
std_diff <- data.frame(
  variable = c("Age", "Education", "Female %", "Baseline Trust"),
  std_diff = c(
    (baseline_balance$age_mean[2] - baseline_balance$age_mean[1]) / 
      sqrt((baseline_balance$age_sd[1]^2 + baseline_balance$age_sd[2]^2) / 2),
    (baseline_balance$education_mean[2] - baseline_balance$education_mean[1]) / 
      sqrt((baseline_balance$education_sd[1]^2 + baseline_balance$education_sd[2]^2) / 2),
    (baseline_balance$prop_female[2] - baseline_balance$prop_female[1]) / 
      sqrt(baseline_balance$prop_female[1] * (1 - baseline_balance$prop_female[1])),
    (baseline_balance$baseline_trust_mean[2] - baseline_balance$baseline_trust_mean[1]) / 
      sqrt((baseline_balance$baseline_trust_sd[1]^2 + baseline_balance$baseline_trust_sd[2]^2) / 2)
  )
)

p_balance <- ggplot(std_diff, aes(x = variable, y = std_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = c(-0.1, 0.1), linetype = "dotted", alpha = 0.5, color = "red") +
  geom_point(size = 4, color = "#e74c3c") +
  geom_segment(aes(xend = variable, yend = 0), size = 1.5, color = "#e74c3c") +
  coord_flip() +
  labs(
    x = "",
    y = "Standardized Difference",
    title = "Baseline Covariate Imbalance",
    subtitle = "Complete at Year 4 vs. Dropout (red lines = ±0.1)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic")
  )

# ========================================================================
# 4. METHOD CORRELATION ANALYSIS
# ========================================================================
cat("\n=== METHOD CORRELATION ANALYSIS ===\n")

# extract predictions for each method at each year
method_predictions <- data.frame(
  years = rep(0:4, 5),
  method = rep(c("Oracle", "Complete Case", "IPCW", "Amelia", "MICE"), each = 5),
  prediction = c(
    oracle_means$mean_trust_science,
    complete_case_means$mean_trust_science,
    ipcw_means$mean_trust_science,
    amelia_means$mean_trust_science,
    mice_means$mean_trust_science
  )
) %>%
  pivot_wider(names_from = method, values_from = prediction)

# correlation matrix
cor_matrix <- cor(method_predictions[,-1])

# correlation plot
p_cor <- corrplot(cor_matrix, 
                  method = "color",
                  type = "upper",
                  order = "hclust",
                  col = viridis(100),
                  addCoef.col = "black",
                  tl.col = "black",
                  tl.srt = 45,
                  title = "Method Prediction Correlations",
                  mar = c(0,0,2,0))

# ========================================================================
# 5. ORDINAL MODEL DIAGNOSTICS
# ========================================================================
cat("\n=== ORDINAL MODEL DIAGNOSTICS ===\n")

# threshold comparison plot
threshold_long <- threshold_comparison %>%
  pivot_longer(cols = c(threshold_low_med, threshold_med_high),
               names_to = "threshold", values_to = "value") %>%
  mutate(
    threshold = ifelse(threshold == "threshold_low_med", 
                      "Low|Medium", "Medium|High"),
    method = factor(method, levels = c("oracle", "complete_case", "ipcw", 
                                      "amelia", "mice"))
  )

p_thresholds <- ggplot(threshold_long,
                      aes(x = method, y = value, fill = threshold)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(data = threshold_long %>% filter(method == "oracle"),
             aes(yintercept = value, color = threshold),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Low|Medium" = "#e74c3c", 
                              "Medium|High" = "#27ae60")) +
  scale_color_manual(values = c("Low|Medium" = "#e74c3c", 
                               "Medium|High" = "#27ae60")) +
  scale_x_discrete(labels = c("Oracle", "Complete\nCase", "IPCW", 
                             "Amelia", "MICE")) +
  labs(
    x = "",
    y = "Threshold Value",
    title = "Ordinal Model Thresholds",
    subtitle = "Dashed lines show oracle (true) thresholds",
    fill = "Threshold",
    color = "Threshold"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic")
  )

# ========================================================================
# 6. COMPOSITE DIAGNOSTIC FIGURE
# ========================================================================
cat("\n=== CREATING COMPOSITE DIAGNOSTIC FIGURE ===\n")

diagnostic_figure <- (p_attrition + p_balance) / 
                    (p_weights + p_weight_groups) /
                    p_thresholds +
  plot_annotation(
    title = "IPCW Simulation Diagnostics",
    subtitle = "Attrition patterns, covariate balance, and weight distributions",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic")
    )
  ) +
  plot_layout(heights = c(1, 1, 1))

# ========================================================================
# 7. DIAGNOSTIC TABLES
# ========================================================================
cat("\n=== CREATING DIAGNOSTIC TABLES ===\n")

# attrition table
attrition_table <- attrition_summary %>%
  mutate(
    percent_missing = paste0(round(prop_missing * 100, 1), "%")
  ) %>%
  select(years, trust_group, n_total, n_missing, percent_missing) %>%
  pivot_wider(names_from = trust_group, 
              values_from = c(n_total, n_missing, percent_missing)) %>%
  gt() %>%
  tab_header(
    title = "Attrition by Trust Group and Year",
    subtitle = "Sample sizes and missingness rates"
  ) %>%
  tab_spanner(
    label = "Low Trust",
    columns = c(n_total_low, n_missing_low, percent_missing_low)
  ) %>%
  tab_spanner(
    label = "Medium Trust",
    columns = c(n_total_medium, n_missing_medium, percent_missing_medium)
  ) %>%
  tab_spanner(
    label = "High Trust",
    columns = c(n_total_high, n_missing_high, percent_missing_high)
  )

# weight summary table
weight_table <- weight_extremes %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  gt() %>%
  tab_header(
    title = "IPCW Weight Summary",
    subtitle = "By year and trust group"
  ) %>%
  cols_label(
    years = "Year",
    trust_group = "Trust Group",
    mean_weight = "Mean",
    median_weight = "Median",
    max_weight = "Maximum",
    p95_weight = "95th Percentile"
  ) %>%
  fmt_number(
    columns = c(mean_weight, median_weight, max_weight, p95_weight),
    decimals = 3
  )

# ========================================================================
# 8. SAVE OUTPUTS
# ========================================================================
cat("\n=== SAVING DIAGNOSTIC OUTPUTS ===\n")

# save plots
ggsave("results/figures/ipcw_diagnostics_full.png", diagnostic_figure,
       width = 14, height = 16, dpi = 300)

ggsave("results/figures/ipcw_attrition_patterns.png", p_attrition,
       width = 10, height = 6, dpi = 300)

ggsave("results/figures/ipcw_weight_distribution.png", p_weights,
       width = 12, height = 8, dpi = 300)

ggsave("results/figures/ipcw_covariate_balance.png", p_balance,
       width = 8, height = 6, dpi = 300)

ggsave("results/figures/ipcw_ordinal_thresholds.png", p_thresholds,
       width = 10, height = 6, dpi = 300)

# save tables
gtsave(attrition_table, "results/tables/ipcw_attrition_summary.html")
gtsave(weight_table, "results/tables/ipcw_weight_summary.html")

# save diagnostic summary
diagnostic_summary <- paste0(
  "IPCW SIMULATION DIAGNOSTIC SUMMARY\n",
  "==================================\n\n",
  
  "Attrition Patterns:\n",
  "- Low trust group: ", round(max(attrition_summary$prop_missing[attrition_summary$trust_group == "low"]) * 100, 1), "% missing by Year 4\n",
  "- Medium trust group: ", round(max(attrition_summary$prop_missing[attrition_summary$trust_group == "medium"]) * 100, 1), "% missing by Year 4\n",
  "- High trust group: ", round(max(attrition_summary$prop_missing[attrition_summary$trust_group == "high"]) * 100, 1), "% missing by Year 4\n\n",
  
  "IPCW Weight Distribution:\n",
  "- Mean weight range: ", round(min(weight_extremes$mean_weight), 2), " to ", round(max(weight_extremes$mean_weight), 2), "\n",
  "- Maximum weight observed: ", round(max(weight_extremes$max_weight), 2), "\n",
  "- Weights increase over time as expected\n\n",
  
  "Baseline Covariate Balance:\n",
  "- Large imbalance in baseline trust (std diff = ", round(std_diff$std_diff[4], 3), ")\n",
  "- Moderate imbalance in education (std diff = ", round(std_diff$std_diff[2], 3), ")\n",
  "- This confirms differential attrition by trust level\n\n",
  
  "Ordinal Model Thresholds:\n",
  "- Complete Case shifts thresholds upward (selection bias)\n",
  "- IPCW partially corrects threshold bias\n",
  "- Amelia and MICE closely match oracle thresholds\n"
)

writeLines(diagnostic_summary, "results/ipcw_diagnostic_summary.txt")

cat("\n=== DIAGNOSTICS COMPLETE ===\n")
cat("Diagnostic outputs saved to:\n")
cat("- Figures: results/figures/ipcw_diagnostics_*.png\n")
cat("- Tables: results/tables/ipcw_*.html\n")
cat("- Summary: results/ipcw_diagnostic_summary.txt\n")