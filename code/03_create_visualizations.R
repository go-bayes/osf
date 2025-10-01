# Create Visualizations for Trust in Science Analysis
# This script generates all figures for the manuscript
# joseph.bulbulia@gmail.com

# setup
cat("==========================================================================\n")
cat("STEP 3: CREATE VISUALIZATIONS\n")
cat("==========================================================================\n\n")

# load required packages
library(tidyverse)
library(here)
library(ggplot2)
library(ggeffects)
library(patchwork)
library(glue)

# source configuration
source("code/config/config.R")

# load predictions
cat("Loading model predictions...\n")
predictions_all <- readRDS("results/model_outputs/predictions_all.rds")
cat("  ✓ Predictions loaded\n\n")

# set plot theme
theme_set(theme_minimal(base_size = 12))

# ========================================================================
# CONTINUOUS OUTCOMES - GEE MODELS
# ========================================================================
cat("=== CREATING CONTINUOUS OUTCOME PLOTS ===\n\n")

# trust in science - observed
cat("1. Trust in Science (continuous) - Observed\n")
gee_plot_trust_science_observed <- plot(
  predictions_all$observed$gee_science,
  show_ci = TRUE,
  show_data = TRUE,
  ci_style = "dash",
  colors = "metro",
  jitter = .5,
  dot_alpha = .01,
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "red",
    size = 2,
    alpha = 1
  ) +
  labs(
    x = "Years: 2019-2023",
    y = "Trust in Science (1-7)",
    title = "Average Trust in Science (Observed)"
  ) +
  theme_bw() + scale_y_continuous(limits = (c(0,7)))

print(gee_plot_trust_science_observed)

# trust in science - imputed
cat("2. Trust in Science (continuous) - Imputed\n")
gee_plot_trust_science_imputed <- plot(
  predictions_all$imputed$gee_science,
  show_ci = TRUE,
  show_data = TRUE,
  ci_style = "dash",
  colors = "us",
  jitter = .5,
  dot_alpha = .01,
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "dodgerblue",
    size = 2,
    alpha = 1
  ) +
  labs(
    x = "Years: 2019-2023",
    y = "Trust in Science (1-7)",
    title = "Average Trust in Science (Imputed)"
  ) +
  theme_bw() + scale_y_continuous(limits = (c(0,7)))

print(gee_plot_trust_science_imputed)

# view both
library(patchwork)
gee_plot_trust_science_observed + gee_plot_trust_science_imputed

# trust in scientists - observed
cat("3. Trust in Scientists (continuous) - Observed\n")
gee_plot_trust_scientists_observed <- plot(
  predictions_all$observed$gee_scientists,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "metro",
  limits = c(4.5, 5.5),
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "red",
    size = 2,
    alpha = 1
  ) +
  labs(
    x = "Years: 2019-2023",
    y = "Trust in Scientists (1-7)",
    title = "Average Trust in Scientists (Observed)"
  ) +
  theme_bw()   + scale_y_continuous(limits = (c(0,7)))

print(gee_plot_trust_scientists_observed)

# trust in scientists - imputed
cat("4. Trust in Scientists (continuous) - Imputed\n")
gee_plot_trust_scientists_imputed <- plot(
  predictions_all$imputed$gee_scientists,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "us",
  limits = c(4.5, 5.5),
  dot_size = 2
) +
  geom_point(
    aes(x = x, y = predicted),
    color = "dodgerblue",
    size = 2,
    alpha = 1
  ) +
  labs(
    x = "Years: 2019-2023",
    y = "Trust in Scientists (1-7)",
    title = "Average Trust in Scientists (Imputed)"
  ) +
  theme_bw() + scale_y_continuous(limits = (c(0,7)))

print(gee_plot_trust_scientists_imputed) + scale_y_continuous(limits = (c(0,7)))

# combined plot for continuous outcomes
cat("\nCreating combined continuous plot...\n")
combined_continuous_plot <-
  (gee_plot_trust_science_observed + gee_plot_trust_science_imputed) /
  (gee_plot_trust_scientists_observed + gee_plot_trust_scientists_imputed) +
  plot_annotation(
    title = "Trust in Science and Scientists: Continuous Outcomes",
    subtitle = glue("GEE models with natural splines (N = {N_PARTICIPANTS})")
  )  + scale_y_continuous(limits = (c(0,7)))

print(combined_continuous_plot)
cat("  ✓ Combined continuous plot created\n")

# ========================================================================
# CATEGORICAL OUTCOMES - POLR MODELS
# ========================================================================
cat("\n=== CREATING CATEGORICAL OUTCOME PLOTS ===\n\n")

# trust in science categories - observed
cat("1. Trust in Science (categorical) - Observed\n")
cat_plot_trust_science_observed <- plot(
  predictions_all$observed$polr_science,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "metro",
  limits = c(0, 1),
  dot_size = 2
) +
  labs(
    x = "Years: 2019-2023",
    y = "Predicted Probability",
    title = "Level of Trust in Science (Observed)"
  ) +
  theme_bw()

print(cat_plot_trust_science_observed)

# trust in science categories - imputed
cat("2. Trust in Science (categorical) - Imputed\n")
cat_plot_trust_science_imputed <- plot(
  predictions_all$imputed$polr_science,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "us",
  limits = c(0, 1),
  dot_size = 2
) +
  labs(
    x = "Years: 2019-2023",
    y = "Predicted Probability",
    title = "Level of Trust in Science (Imputed)"
  ) +
  theme_bw() + scale_y_continuous(limits = (c(0,7)))

print(cat_plot_trust_science_imputed)

# trust in scientists categories - observed
cat("3. Trust in Scientists (categorical) - Observed\n")
cat_plot_trust_scientists_observed <- plot(
  predictions_all$observed$polr_scientists,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "metro",
  limits = c(0, 1),
  dot_size = 2
) +
  labs(
    x = "Years: 2019-2023",
    y = "Predicted Probability",
    title = "Level of Trust in Scientists (Observed)"
  ) +
  theme_bw() + scale_y_continuous(limits = (c(0,7)))

print(cat_plot_trust_scientists_observed)

# trust in scientists categories - imputed
cat("4. Trust in Scientists (categorical) - Imputed\n")
cat_plot_trust_scientists_imputed <- plot(
  predictions_all$imputed$polr_scientists,
  show_ci = TRUE,
  ci_style = "dash",
  colors = "us",
  limits = c(0, 1),
  dot_size = 2
) +
  labs(
    x = "Years: 2019-2023",
    y = "Predicted Probability",
    title = "Level of Trust in Scientists (Imputed)"
  ) +
  theme_bw() + scale_y_continuous(limits = (c(0,7)))

print(cat_plot_trust_scientists_imputed)

# combined plot for categorical outcomes
cat("\nCreating combined categorical plot...\n")
combined_categorical_plot <-
  (cat_plot_trust_science_observed + cat_plot_trust_science_imputed) /
  (cat_plot_trust_scientists_observed + cat_plot_trust_scientists_imputed) +
  plot_annotation(
    title = "Trust in Science and Scientists: Categorical Outcomes",
    subtitle = glue("Proportional odds models with {N_IMPUTATIONS} imputations")
  )  + scale_y_continuous(limits = (c(0,7)))

print(combined_categorical_plot)
cat("  ✓ Combined categorical plot created\n")

# ========================================================================
# MAIN FIGURE - COMBINED ALL PLOTS
# ========================================================================
cat("\n=== CREATING MAIN FIGURE ===\n")

# note: categorical plots are faceted, so combining them all is complex
# create a simpler main figure with just the continuous outcomes
main_figure_continuous <- combined_continuous_plot +
  plot_annotation(
    title = "Trust in Science Over Time: Observed vs Imputed Data",
    subtitle = glue("New Zealand Attitudes and Values Study (N = {N_PARTICIPANTS})"),
    caption = "Note: Shaded areas represent 95% confidence intervals. Red = observed data, Blue = imputed data.",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  )

print(main_figure_continuous)
cat("  ✓ Main figure (continuous) created\n")

# the categorical plots are best viewed separately due to faceting
cat("  Note: Categorical plots saved separately due to faceted structure\n")

# ========================================================================
# SAVE FIGURES
# ========================================================================
cat("\n=== SAVING FIGURES ===\n")

# create figure directory if needed
fig_dir <- "results/figures"
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# save individual continuous plots
ggsave(
  filename = file.path(fig_dir, "gee_trust_science_observed.png"),
  plot = gee_plot_trust_science_observed,
  width = 8, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = file.path(fig_dir, "gee_trust_science_imputed.png"),
  plot = gee_plot_trust_science_imputed,
  width = 8, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = file.path(fig_dir, "gee_trust_scientists_observed.png"),
  plot = gee_plot_trust_scientists_observed,
  width = 8, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = file.path(fig_dir, "gee_trust_scientists_imputed.png"),
  plot = gee_plot_trust_scientists_imputed,
  width = 8, height = 6, dpi = FIGURE_DPI
)

cat("  ✓ Individual continuous plots saved\n")

# save individual categorical plots
ggsave(
  filename = file.path(fig_dir, "cat_trust_science_observed.png"),
  plot = cat_plot_trust_science_observed,
  width = 8, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = file.path(fig_dir, "cat_trust_science_imputed.png"),
  plot = cat_plot_trust_science_imputed,
  width = 8, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = file.path(fig_dir, "cat_trust_scientists_observed.png"),
  plot = cat_plot_trust_scientists_observed,
  width = 8, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = file.path(fig_dir, "cat_trust_scientists_imputed.png"),
  plot = cat_plot_trust_scientists_imputed,
  width = 8, height = 6, dpi = FIGURE_DPI
)

cat("  ✓ Individual categorical plots saved\n")

# save combined plots
ggsave(
  filename = file.path(fig_dir, "combined_continuous.png"),
  plot = combined_continuous_plot,
  width = 14, height = 10, dpi = FIGURE_DPI
)

ggsave(
  filename = file.path(fig_dir, "combined_categorical.png"),
  plot = combined_categorical_plot,
  width = 14, height = 10, dpi = FIGURE_DPI
)

cat("  ✓ Combined plots saved\n")

# save main figure
ggsave(
  filename = file.path(fig_dir, "main_figure_continuous.png"),
  plot = main_figure_continuous,
  width = 14, height = 10, dpi = FIGURE_DPI
)

# save pdf versions if requested
if ("pdf" %in% FIGURE_FORMAT) {
  ggsave(
    filename = file.path(fig_dir, "main_figure_continuous.pdf"),
    plot = main_figure_continuous,
    width = 14, height = 10, device = "pdf"
  )

  ggsave(
    filename = file.path(fig_dir, "combined_continuous.pdf"),
    plot = combined_continuous_plot,
    width = 14, height = 10, device = "pdf"
  )

  ggsave(
    filename = file.path(fig_dir, "combined_categorical.pdf"),
    plot = combined_categorical_plot,
    width = 14, height = 10, device = "pdf"
  )

  cat("  ✓ PDF versions saved\n")
}

# ========================================================================
# SUMMARY
# ========================================================================
cat("\n==========================================================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("==========================================================================\n")
cat("Figures saved to:", fig_dir, "\n")
cat("\nMain outputs:\n")
cat("  - main_figure_continuous.png/pdf: Main figure with continuous outcomes\n")
cat("  - combined_continuous.png/pdf: All continuous outcome plots\n")
cat("  - combined_categorical.png/pdf: All categorical outcome plots\n")
cat("  - Individual plots for each outcome and data type\n")
cat("\nNote: Categorical plots contain facets for response levels (low/med/high)\n")
cat("\nNext step: Run 04_generate_tables.R\n")

