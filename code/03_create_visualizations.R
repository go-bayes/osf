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
library(patchwork)
library(glue)

# source configuration
source(("code/config/config.R"))

# load predictions
cat("Loading model predictions...\n")
predictions_all <- readRDS(("results/model_outputs/predictions_all.rds"))
cat("  ✓ Predictions loaded\n\n")

# set plot theme
theme_set(theme_minimal(base_size = 12))

# color palette
colors_observed <- "#0072B2"  # blue
colors_imputed <- "#D55E00"   # orange

# ========================================================================
# CONTINUOUS OUTCOME PLOTS
# ========================================================================
cat("Creating continuous outcome plots...\n")

# function to create continuous outcome plot
create_continuous_plot <- function(pred_observed, pred_imputed, 
                                 outcome_label, y_limits = c(4.5, 5.5)) {
  
  # prepare data
  df_observed <- as.data.frame(pred_observed) %>%
    mutate(
      data_type = "Observed",
      year_label = case_when(
        years == 0 ~ "2019-2020",
        years == 1 ~ "2020-2021", 
        years == 2 ~ "2021-2022",
        years == 3 ~ "2022-2024",
        TRUE ~ as.character(years)
      )
    )
  
  df_imputed <- as.data.frame(pred_imputed) %>%
    mutate(
      data_type = "Imputed",
      year_label = case_when(
        years == 0 ~ "2019-2020",
        years == 1 ~ "2020-2021",
        years == 2 ~ "2021-2022", 
        years == 3 ~ "2022-2024",
        TRUE ~ as.character(years)
      )
    )
  
  df_combined <- bind_rows(df_observed, df_imputed)
  
  # create plot
  p <- ggplot(df_combined, aes(x = years, y = predicted, 
                               color = data_type, fill = data_type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
                alpha = 0.2, color = NA) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Observed" = colors_observed, 
                                 "Imputed" = colors_imputed)) +
    scale_fill_manual(values = c("Observed" = colors_observed,
                                "Imputed" = colors_imputed)) +
    scale_x_continuous(
      breaks = 0:3,
      labels = c("2019-2020", "2020-2021", "2021-2022", "2022-2024")
    ) +
    coord_cartesian(ylim = y_limits) +
    labs(
      x = "Year",
      y = outcome_label,
      color = "Data Type",
      fill = "Data Type"
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# create plots
plot_science_continuous <- create_continuous_plot(
  predictions_all$observed$gee_science,
  predictions_all$imputed$gee_science,
  "Social Value of Science (1-7)",
  y_limits = c(4.8, 5.8)
)

plot_scientists_continuous <- create_continuous_plot(
  predictions_all$observed$gee_scientists,
  predictions_all$imputed$gee_scientists,
  "Trust in Scientists (1-7)",
  y_limits = c(4.4, 5.4)
)

cat("  ✓ Continuous outcome plots created\n")

# ========================================================================
# CATEGORICAL OUTCOME PLOTS
# ========================================================================
cat("\nCreating categorical outcome plots...\n")

# function to create categorical outcome plot
create_categorical_plot <- function(pred_observed, pred_imputed,
                                  outcome_label) {
  
  # prepare observed data
  df_observed <- as.data.frame(pred_observed) %>%
    mutate(
      data_type = "Observed",
      response_label = case_when(
        response.level == "low" ~ "Low (1-3)",
        response.level == "med" ~ "Medium (4-5)",
        response.level == "high" ~ "High (6-7)",
        TRUE ~ as.character(response.level)
      )
    )
  
  # prepare imputed data
  df_imputed <- as.data.frame(pred_imputed) %>%
    mutate(
      data_type = "Imputed",
      response_label = case_when(
        response.level == "low" ~ "Low (1-3)",
        response.level == "med" ~ "Medium (4-5)", 
        response.level == "high" ~ "High (6-7)",
        TRUE ~ as.character(response.level)
      )
    )
  
  # combine data
  df_combined <- bind_rows(df_observed, df_imputed)
  
  # set factor levels
  df_combined$response_label <- factor(
    df_combined$response_label,
    levels = c("Low (1-3)", "Medium (4-5)", "High (6-7)")
  )
  
  # create plot
  p <- ggplot(df_combined, aes(x = years, y = predicted,
                               color = data_type, fill = data_type)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.2, color = NA) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    facet_wrap(~ response_label, ncol = 3, scales = "free_y") +
    scale_color_manual(values = c("Observed" = colors_observed,
                                 "Imputed" = colors_imputed)) +
    scale_fill_manual(values = c("Observed" = colors_observed,
                                "Imputed" = colors_imputed)) +
    scale_x_continuous(
      breaks = 0:3,
      labels = c("2019-20", "2020-21", "2021-22", "2022-24")
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      x = "Year",
      y = "Predicted Probability",
      color = "Data Type",
      fill = "Data Type",
      title = outcome_label
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  return(p)
}

# create plots
plot_science_categorical <- create_categorical_plot(
  predictions_all$observed$polr_science,
  predictions_all$imputed$polr_science,
  "Social Value of Science Categories"
)

plot_scientists_categorical <- create_categorical_plot(
  predictions_all$observed$polr_scientists,
  predictions_all$imputed$polr_scientists,
  "Trust in Scientists Categories"
)

cat("  ✓ Categorical outcome plots created\n")

# ========================================================================
# COMBINED PLOTS
# ========================================================================
cat("\nCreating combined plots...\n")

# combine continuous plots
combined_continuous <- plot_science_continuous + plot_scientists_continuous +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

# combine categorical plots
combined_categorical <- plot_science_categorical / plot_scientists_categorical +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# main figure combining all plots
main_figure <- (combined_continuous / combined_categorical) +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    title = "Trust in Science Over Time: Observed vs Imputed Data",
    subtitle = glue("New Zealand Attitudes and Values Study (N = {N_PARTICIPANTS})"),
    caption = "Note: Shaded areas represent 95% confidence intervals.",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  )

cat("  ✓ Combined plots created\n")

# ========================================================================
# SAVE FIGURES
# ========================================================================
cat("\nSaving figures...\n")

# create figure directory if needed
fig_dir <- ("results/figures")
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# save individual plots
ggsave(
  filename = (fig_dir, "trust_science_continuous.png"),
  plot = plot_science_continuous,
  width = 8, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = (fig_dir, "trust_scientists_continuous.png"),
  plot = plot_scientists_continuous,
  width = 8, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = (fig_dir, "trust_science_categorical.png"),
  plot = plot_science_categorical,
  width = 12, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = (fig_dir, "trust_scientists_categorical.png"),
  plot = plot_scientists_categorical,
  width = 12, height = 6, dpi = FIGURE_DPI
)

# save combined plots
ggsave(
  filename = (fig_dir, "combined_continuous.png"),
  plot = combined_continuous,
  width = 14, height = 6, dpi = FIGURE_DPI
)

ggsave(
  filename = (fig_dir, "combined_categorical.png"),
  plot = combined_categorical,
  width = 14, height = 10, dpi = FIGURE_DPI
)

# save main figure
ggsave(
  filename = (fig_dir, "main_figure.png"),
  plot = main_figure,
  width = 14, height = 16, dpi = FIGURE_DPI
)

# save pdf versions
if ("pdf" %in% FIGURE_FORMAT) {
  ggsave(
    filename = (fig_dir, "main_figure.pdf"),
    plot = main_figure,
    width = 14, height = 16, device = "pdf"
  )
  cat("  ✓ PDF version saved\n")
}

cat("  ✓ All figures saved\n")

# ========================================================================
# CREATE SUPPLEMENTARY PLOTS
# ========================================================================
cat("\nCreating supplementary plots...\n")

# plot showing missingness patterns
if (exists("mids_obj")) {
  # would need to load mids_obj - skip for now
  cat("  - Missingness pattern plot (skipped - requires mids object)\n")
}

# ========================================================================
# SUMMARY
# ========================================================================
cat("\n==========================================================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("==========================================================================\n")
cat("Figures saved to:", fig_dir, "\n")
cat("\nMain outputs:\n")
cat("  - main_figure.png/pdf: Combined figure for manuscript\n")
cat("  - Individual plots for each outcome and model type\n")
cat("\nNext step: Run 04_generate_tables.R\n")