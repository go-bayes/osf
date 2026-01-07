# quick_run_ipcw.R
# Quick script to run just the essential parts for testing
# joseph.bulbulia@vuw.ac.nz

# setup
library(tidyverse)
library(here)

# set working directory
setwd(here::here("keep"))

# clear workspace
rm(list = ls())

cat("Running IPCW simulation (essential parts only)...\n\n")

# source the main simulation
source("test_2_with_ipcw.R")

# check if key objects exist
cat("\n\nChecking key objects:\n")
cat("- overall_performance exists:", exists("overall_performance"), "\n")
cat("- comparison_data exists:", exists("comparison_data"), "\n")
cat("- error_summary exists:", exists("error_summary"), "\n")
cat("- best_method:", ifelse(exists("best_method"), best_method, "NOT FOUND"), "\n")

# if successful, you can now run:
# source("analyze_ipcw_results.R")
# source("generate_ipcw_diagnostics.R")

cat("\nReady to run analysis scripts!\n")