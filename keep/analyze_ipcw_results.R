# analyze_ipcw_results.R
# Analysis and visualization of IPCW simulation results
# Creates publication-ready figures and tables
# joseph.bulbulia@vuw.ac.nz
# january 2025

# setup
library(tidyverse)
library(patchwork)
library(gt)
library(gtsummary)
library(scales)
library(viridis)

# run the simulation if results don't exist
if (!exists("overall_performance")) {
  cat("Running simulation first...\n")
  cat("This will take a few minutes...\n\n")
  source("test_2_with_ipcw.R")
}

# ========================================================================
# 1. CONTINUOUS OUTCOME ANALYSIS
# ========================================================================
cat("\n=== CONTINUOUS OUTCOME ANALYSIS ===\n")

# create publication-ready continuous results plot
continuous_results <- comparison_data %>%
  mutate(method = factor(method,
                        levels = c("Oracle (Truth)", "Complete Case", "IPCW",
                                  "MICE", "Amelia")))

p_continuous <- ggplot(continuous_results,
                      aes(x = years, y = mean_trust_science,
                          color = method, linetype = method)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Oracle (Truth)" = "black",
    "Complete Case" = "#e74c3c",
    "IPCW" = "#3498db",
    "MICE" = "#f39c12",
    "Amelia" = "#27ae60"
  )) +
  scale_linetype_manual(values = c(
    "Oracle (Truth)" = "solid",
    "Complete Case" = "dotted",
    "IPCW" = "dashed",
    "MICE" = "dotdash",
    "Amelia" = "longdash"
  )) +
  labs(
    x = "Year",
    y = "Mean Trust in Science",
    title = "Recovery of Population Means by Method",
    subtitle = "Trust in Science (1-7 scale)",
    color = "Method",
    linetype = "Method"
  ) +
  scale_y_continuous(limits = c(4.8, 5.2), breaks = seq(4.8, 5.2, 0.1)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# bias comparison
p_bias <- ggplot(bias_data,
                 aes(x = years, y = bias, fill = method, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_line(aes(group = method), size = 1) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = 0, ymax = bias, group = method), alpha = 0.2) +
  scale_color_manual(values = c(
    "Complete Case" = "#e74c3c",
    "IPCW" = "#3498db",
    "MICE" = "#f39c12",
    "Amelia" = "#27ae60"
  )) +
  scale_fill_manual(values = c(
    "Complete Case" = "#e74c3c",
    "IPCW" = "#3498db",
    "MICE" = "#f39c12",
    "Amelia" = "#27ae60"
  )) +
  labs(
    x = "Year",
    y = "Bias (Method - Oracle)",
    title = "Bias in Mean Estimates",
    subtitle = "Deviation from true population mean",
    color = "Method",
    fill = "Method"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# ========================================================================
# 2. CATEGORICAL OUTCOME ANALYSIS
# ========================================================================
cat("\n=== CATEGORICAL OUTCOME ANALYSIS ===\n")

# enhanced category proportion plot
cat_prop_long <- bind_rows(
  oracle_cat_props %>%
    dplyr::select(years, Low = prop_low, Medium = prop_medium, High = prop_high) %>%
    pivot_longer(cols = -years, names_to = "category", values_to = "proportion") %>%
    mutate(method = "Oracle"),
  cc_cat_props %>%
    dplyr::select(years, Low = prop_low, Medium = prop_medium, High = prop_high) %>%
    pivot_longer(cols = -years, names_to = "category", values_to = "proportion") %>%
    mutate(method = "Complete Case"),
  ipcw_cat_props %>%
    dplyr::select(years, Low = prop_low, Medium = prop_medium, High = prop_high) %>%
    pivot_longer(cols = -years, names_to = "category", values_to = "proportion") %>%
    mutate(method = "IPCW"),
  amelia_cat_props %>%
    dplyr::select(years, Low = prop_low, Medium = prop_medium, High = prop_high) %>%
    pivot_longer(cols = -years, names_to = "category", values_to = "proportion") %>%
    mutate(method = "Amelia"),
  mice_cat_props %>%
    dplyr::select(years, Low = prop_low, Medium = prop_medium, High = prop_high) %>%
    pivot_longer(cols = -years, names_to = "category", values_to = "proportion") %>%
    mutate(method = "MICE")
) %>%
  mutate(
    method = factor(method, levels = c("Oracle", "Complete Case", "IPCW", "MICE", "Amelia")),
    category = factor(category, levels = c("Low", "Medium", "High"))
  )

# faceted category plot
p_categories <- ggplot(cat_prop_long,
                      aes(x = years, y = proportion, fill = category)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap(~method, ncol = 5) +
  scale_fill_manual(values = c(
    "Low" = "#e74c3c",
    "Medium" = "#f39c12",
    "High" = "#27ae60"
  )) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(
    x = "Year",
    y = "Proportion",
    title = "Trust Category Distribution by Method",
    subtitle = "Low (≤3), Medium (4-5), High (≥6)",
    fill = "Trust Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

# diverging bar chart for category shifts
shift_data <- cat_shift_comparison %>%
  dplyr::select(method, low_shift, high_shift) %>%
  pivot_longer(cols = c(low_shift, high_shift),
               names_to = "category", values_to = "shift") %>%
  mutate(
    category = ifelse(category == "low_shift", "Low Trust", "High Trust"),
    method = factor(method, levels = rev(c("Oracle", "Amelia", "MICE", "IPCW", "Complete Case")))
  )

p_shifts <- ggplot(shift_data,
                  aes(x = shift, y = method, fill = category)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(values = c("Low Trust" = "#e74c3c", "High Trust" = "#27ae60")) +
  scale_x_continuous(labels = percent_format()) +
  labs(
    x = "Change in Proportion (Year 4 - Year 0)",
    y = "",
    title = "Category Shift Detection",
    subtitle = "How well each method captures trust category changes",
    fill = "Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )

# ========================================================================
# 3. PERFORMANCE SUMMARY TABLES
# ========================================================================
cat("\n=== CREATING SUMMARY TABLES ===\n")

# continuous performance table
continuous_table <- error_summary %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  gt() %>%
  tab_header(
    title = "Continuous Outcome Performance",
    subtitle = "Comparison of methods in recovering population means"
  ) %>%
  cols_label(
    method = "Method",
    mean_absolute_error = "Mean Absolute Error",
    mean_relative_error = "Mean Relative Error (%)",
    mean_bias = "Mean Bias",
    max_absolute_error = "Max Absolute Error"
  ) %>%
  tab_style(
    style = cell_fill(color = "#e3f2fd"),
    locations = cells_body(rows = method == "Amelia")
  ) %>%
  tab_footnote(
    footnote = "Best performing method highlighted",
    locations = cells_column_labels(columns = mean_absolute_error)
  )

# categorical performance table
categorical_table <- cat_shift_comparison %>%
  arrange(mean_error) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  gt() %>%
  tab_header(
    title = "Categorical Outcome Performance",
    subtitle = "Accuracy in detecting trust category shifts"
  ) %>%
  cols_label(
    method = "Method",
    low_shift = "Low Trust Shift",
    high_shift = "High Trust Shift",
    low_error = "Low Trust Error",
    high_error = "High Trust Error",
    mean_error = "Mean Error"
  ) %>%
  tab_style(
    style = cell_fill(color = "#e3f2fd"),
    locations = cells_body(rows = 1)
  )

# overall performance table
overall_table <- overall_performance %>%
  dplyr::select(method, continuous_mae, category_shift_error, baseline_kl_divergence, mean_rank) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  gt() %>%
  tab_header(
    title = "Overall Method Performance",
    subtitle = "Combined metrics across continuous and categorical outcomes"
  ) %>%
  cols_label(
    method = "Method",
    continuous_mae = "Continuous MAE",
    category_shift_error = "Category Shift Error",
    baseline_kl_divergence = "Baseline KL Divergence",
    mean_rank = "Mean Rank"
  ) %>%
  tab_style(
    style = cell_fill(color = "#e3f2fd"),
    locations = cells_body(rows = 1)
  ) %>%
  tab_footnote(
    footnote = "Lower values indicate better performance",
    locations = cells_column_labels(columns = mean_rank)
  )

# ========================================================================
# 4. COMPOSITE VISUALIZATIONS
# ========================================================================
cat("\n=== CREATING COMPOSITE VISUALIZATIONS ===\n")

# main results figure
main_results <- (p_continuous + p_bias) / p_categories +
  plot_annotation(
    title = "Comparison of Missing Data Methods in Longitudinal Trust Analysis",
    subtitle = "Simulation with differential attrition by trust level (N = 40,000)",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic")
    )
  ) +
  plot_layout(heights = c(1, 1.2))

# categorical results figure
categorical_results <- p_shifts / p_baseline +
  plot_annotation(
    title = "Categorical Outcome Performance",
    subtitle = "Shift detection and baseline preservation",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic")
    )
  )

# ========================================================================
# 5. SAVE OUTPUTS
# ========================================================================
cat("\n=== SAVING OUTPUTS ===\n")

# save plots
ggsave("results/figures/ipcw_main_results.png", main_results,
       width = 14, height = 12, dpi = 300)

ggsave("results/figures/ipcw_categorical_results.png", categorical_results,
       width = 12, height = 10, dpi = 300)

# save individual plots for presentations
ggsave("results/figures/ipcw_continuous_recovery.png", p_continuous,
       width = 10, height = 6, dpi = 300)

ggsave("results/figures/ipcw_bias_comparison.png", p_bias,
       width = 10, height = 6, dpi = 300)

ggsave("results/figures/ipcw_category_distributions.png", p_categories,
       width = 14, height = 6, dpi = 300)

ggsave("results/figures/ipcw_category_shifts.png", p_shifts,
       width = 10, height = 6, dpi = 300)

# save tables
gtsave(continuous_table, "results/tables/ipcw_continuous_performance.html")
gtsave(categorical_table, "results/tables/ipcw_categorical_performance.html")
gtsave(overall_table, "results/tables/ipcw_overall_performance.html")

# ========================================================================
# 6. GENERATE SUMMARY REPORT
# ========================================================================
cat("\n=== GENERATING SUMMARY REPORT ===\n")

summary_text <- paste0(
  "MISSING DATA METHOD COMPARISON SUMMARY\n",
  "=====================================\n\n",

  "Simulation Parameters:\n",
  "- N = 40,000 participants\n",
  "- 5 waves (Years 0-4)\n",
  "- Differential attrition by trust level\n",
  "- COVID shock at Year 1\n\n",

  "Best Performing Method: ", best_method, "\n\n",

  "Continuous Outcome Results:\n",
  "- ", overall_performance$method[overall_performance$rank_continuous == 1],
  " had lowest MAE (", round(min(overall_performance$continuous_mae), 4), ")\n",
  "- Complete Case showed largest bias (",
  round(error_summary$mean_bias[error_summary$method == "Complete Case"], 4), ")\n\n",

  "Categorical Outcome Results:\n",
  "- ", overall_performance$method[overall_performance$rank_category == 1],
  " best captured category shifts\n",
  "- ", overall_performance$method[overall_performance$rank_baseline == 1],
  " best preserved baseline distribution\n\n",

  "Key Findings:\n",
  "1. Amelia's time-series approach handles monotone dropout well\n",
  "2. MICE may introduce baseline distribution shifts\n",
  "3. IPCW provides partial correction but struggles with complex patterns\n",
  "4. Complete Case analysis severely biased due to differential attrition\n\n",

  "Recommendations:\n",
  "- Primary analysis: Amelia imputation\n",
  "- Sensitivity analysis: MICE (standard, not enhanced)\n",
  "- Report both continuous and categorical outcomes\n",
  "- Avoid complete case analysis\n"
)

writeLines(summary_text, "results/ipcw_analysis_summary.txt")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to:\n")
cat("- Figures: results/figures/ipcw_*.png\n")
cat("- Tables: results/tables/ipcw_*.html\n")
cat("- Summary: results/ipcw_analysis_summary.txt\n")

# display summary
cat("\n", summary_text)
