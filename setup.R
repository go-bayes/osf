# setup for attrition simulation report

load_package <- function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

packages <- c("dplyr", "ggplot2", "here", "knitr", "kableExtra", "patchwork", "scales")
invisible(lapply(packages, load_package))

scenario_results <- readRDS(here::here("results", "objects", "test_3_improved_outputs.rds"))

mean_range <- range(unlist(lapply(scenario_results, function(res) {
  res$continuous_means$mean_trust
})), na.rm = TRUE)
mean_range <- c(
  max(1, mean_range[1] - 0.15),
  min(7, mean_range[2] + 0.15)
)

scenario_summary <- bind_rows(lapply(scenario_results, function(res) {
  res$continuous_change |>
    mutate(scenario = res$scenario)
}))

category_summary <- bind_rows(lapply(scenario_results, function(res) {
  res$cat_shift_summary_pct |>
    mutate(scenario = res$scenario)
}))

dropout_note <- function(scenario_label) {
  if (tolower(scenario_label) == "mnar") {
    "Dropout depends on baseline, lagged, and current trust."
  } else {
    "Dropout depends on baseline and lagged trust."
  }
}

plot_mean_recovery <- function(res) {
  note <- dropout_note(res$scenario)
  ggplot(res$continuous_means, aes(x = years, y = mean_trust, color = method)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 0:4, labels = 1:5) +
    scale_y_continuous(
      limits = mean_range,
      breaks = scales::pretty_breaks(n = 5)
    ) +
    labs(
      x = "Year of Study",
      y = "Mean Trust in Science (1-7)",
      title = paste("Trust in Science: Mean Recovery by Method -", res$scenario),
      subtitle = paste("Year means with retention-driven attrition.", note)
    ) +
    theme_minimal()
}

plot_continuous_comparison <- function(res) {
  oracle_mean <- res$oracle_data |>
    group_by(years) |>
    summarise(mean_trust = mean(trust_science), .groups = "drop")

  observed_complete <- res$observed_data |>
    filter(!is.na(trust_science))

  observed_mean <- observed_complete |>
    group_by(years) |>
    summarise(mean_trust = mean(trust_science), .groups = "drop")

  oracle_plot <- ggplot(res$oracle_data, aes(x = years, y = trust_science)) +
    geom_jitter(alpha = 0.02, width = 0.2, height = 0.5, color = "grey40") +
    geom_line(
      data = oracle_mean,
      aes(y = mean_trust),
      color = "darkgreen",
      linewidth = 1.1
    ) +
    geom_point(
      data = oracle_mean,
      aes(y = mean_trust),
      color = "darkgreen",
      size = 3
    ) +
    scale_x_continuous(breaks = 0:4, labels = 1:5) +
    scale_y_continuous(limits = c(1, 7)) +
    labs(
      x = "Year of Study",
      y = "Trust in Science (1-7)",
      title = "Trust in Science - Oracle (Ground Truth)"
    ) +
    theme_bw()

  observed_plot <- ggplot(observed_complete, aes(x = years, y = trust_science)) +
    geom_jitter(alpha = 0.02, width = 0.2, height = 0.5, color = "grey40") +
    geom_line(
      data = observed_mean,
      aes(y = mean_trust),
      color = "red",
      linewidth = 1.1
    ) +
    geom_point(
      data = observed_mean,
      aes(y = mean_trust),
      color = "red",
      size = 3
    ) +
    scale_x_continuous(breaks = 0:4, labels = 1:5) +
    scale_y_continuous(limits = c(1, 7)) +
    labs(
      x = "Year of Study",
      y = "Trust in Science (1-7)",
      title = "Trust in Science - Observed (Survivors)"
    ) +
    theme_bw()

  note <- dropout_note(res$scenario)
  oracle_plot + observed_plot +
    plot_annotation(
      title = paste("Selection Bias in Trust Trajectories (", res$scenario, ")", sep = ""),
      subtitle = paste("Low trust individuals drop out.", note)
    )
}

plot_category_recovery <- function(res) {
  ggplot(res$cat_prop_long, aes(x = years, y = proportion, color = method)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    facet_wrap(~ category, nrow = 1) +
    scale_x_continuous(breaks = 0:4, labels = 1:5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      x = "Year of Study",
      y = "Proportion",
      title = paste("Category Proportions by Method -", res$scenario),
      subtitle = "Low = ≤3, Medium = 4-5, High = ≥6"
    ) +
    theme_minimal()
}
