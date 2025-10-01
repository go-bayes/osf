# Quick Demo of Trust in Science Analysis
# This script runs a simplified version using synthetic data

cat("\n=== TRUST IN SCIENCE ANALYSIS DEMO ===\n\n")

# setup
library(here)
setwd(())

# set to use synthetic data with fewer imputations for speed
cat("Configuring for demo mode...\n")
config_file <- ("code/config/config.R")
config_lines <- readLines(config_file)
config_lines[grep("USE_REAL_DATA", config_lines)] <- "USE_REAL_DATA <- FALSE"
config_lines[grep("N_IMPUTATIONS", config_lines)] <- "N_IMPUTATIONS <- 2  # reduced for demo"
writeLines(config_lines, config_file)

# run analysis pipeline
cat("\nRunning analysis pipeline...\n\n")

scripts <- c(
  "00_setup_environment.R",
  "01_load_prepare_data.R",
  "02_fit_models.R",
  "03_create_visualizations.R",
  "04_generate_tables.R",
  "05_extract_results.R"
)

for (script in scripts) {
  cat("\n>>> Running", script, "\n")
  source(("code", script))
}

# display results
cat("\n\n=== DEMO COMPLETE ===\n")
cat("\nKey results saved to:\n")
cat("  - Figures: results/figures/\n")
cat("  - Tables: results/tables/\n")
cat("  - Summary: results/key_findings.json\n")

# show main figure if possible
if (interactive()) {
  cat("\nOpening main figure...\n")
  if (file.exists(("results/figures/main_figure.png"))) {
    browseURL(("results/figures/main_figure.png"))
  }
}

# reset config
cat("\nResetting configuration...\n")
config_lines[grep("N_IMPUTATIONS", config_lines)] <- "N_IMPUTATIONS <- 10"
writeLines(config_lines, config_file)

cat("\nDemo complete! For full analysis, run scripts with N_IMPUTATIONS = 10\n")

