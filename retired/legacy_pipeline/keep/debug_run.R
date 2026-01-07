# debug_run.R
# Debug script to identify issues in the pipeline
# joseph.bulbulia@vuw.ac.nz

# setup
library(tidyverse)
library(here)

# clear workspace
rm(list = ls())

cat("=== DEBUG RUN FOR IPCW ANALYSIS ===\n\n")

# Step 1: Try to run the simulation with error handling
cat("Step 1: Running simulation with detailed error tracking...\n")

tryCatch({
  # change to keep directory
  setwd(here::here("keep"))
  
  # source the simulation
  source("test_2_with_ipcw.R")
  
  cat("\n✓ Simulation completed successfully!\n")
  
}, error = function(e) {
  cat("\n✗ Error in simulation at line:\n")
  print(e)
  cat("\nTraceback:\n")
  traceback()
})

# Step 2: Check what objects were created
cat("\n\nStep 2: Checking created objects...\n")

all_objects <- ls()
cat("Total objects in environment:", length(all_objects), "\n")

# check specific objects needed for analysis
required_for_analysis <- c(
  "overall_performance",
  "comparison_data", 
  "error_summary",
  "bias_data",
  "oracle_means",
  "complete_case_means",
  "ipcw_means",
  "amelia_means",
  "mice_means",
  "oracle_cat_props",
  "cc_cat_props",
  "ipcw_cat_props", 
  "amelia_cat_props",
  "mice_cat_props",
  "cat_shift_comparison",
  "baseline_comparison",
  "threshold_comparison",
  "best_method"
)

cat("\nChecking required objects:\n")
for (obj in required_for_analysis) {
  if (exists(obj)) {
    cat("✓", obj, "\n")
  } else {
    cat("✗", obj, "- MISSING\n")
  }
}

# Step 3: If key objects exist, check their structure
if (exists("baseline_comparison")) {
  cat("\n\nStructure of baseline_comparison:\n")
  cat("Dimensions:", nrow(baseline_comparison), "x", ncol(baseline_comparison), "\n")
  cat("Column names:", paste(names(baseline_comparison), collapse = ", "), "\n")
  print(head(baseline_comparison))
}

if (exists("overall_performance")) {
  cat("\n\nStructure of overall_performance:\n")
  print(str(overall_performance))
}

# Step 4: Try to run analysis with detailed error catching
if (exists("overall_performance") && exists("comparison_data")) {
  cat("\n\nStep 4: Attempting to run analysis script...\n")
  
  tryCatch({
    source("analyze_ipcw_results.R")
    cat("\n✓ Analysis completed successfully!\n")
  }, error = function(e) {
    cat("\n✗ Error in analysis:\n")
    print(e)
    cat("\nThis might be due to missing packages or data issues.\n")
  })
} else {
  cat("\n\nStep 4: Cannot run analysis - required objects missing\n")
  cat("Please check the simulation output above for errors.\n")
}

cat("\n=== DEBUG RUN COMPLETE ===\n")