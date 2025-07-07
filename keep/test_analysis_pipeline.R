# test_analysis_pipeline.R
# Test script to run the full IPCW analysis pipeline
# joseph.bulbulia@vuw.ac.nz

# clear environment
rm(list = ls())

# set working directory to keep folder
setwd(here::here("keep"))

cat("=== RUNNING IPCW ANALYSIS PIPELINE ===\n\n")

# Step 1: Run the simulation
cat("Step 1: Running IPCW simulation...\n")
cat("This will take approximately 2-3 minutes\n\n")

tryCatch({
  source("test_2_with_ipcw.R")
  cat("\n✓ Simulation completed successfully\n\n")
}, error = function(e) {
  cat("\n✗ Error in simulation:\n")
  print(e)
  stop("Pipeline halted due to simulation error")
})

# Step 2: Check that required objects exist
cat("Step 2: Checking required objects...\n")

required_objects <- c(
  "overall_performance",
  "comparison_data",
  "error_summary",
  "bias_data",
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

missing_objects <- required_objects[!sapply(required_objects, exists)]

if (length(missing_objects) > 0) {
  cat("\n✗ Missing objects:\n")
  print(missing_objects)
  stop("Cannot proceed - required objects are missing")
} else {
  cat("✓ All required objects found\n\n")
}

# Step 3: Generate analysis outputs
cat("Step 3: Generating analysis outputs...\n")

tryCatch({
  source("analyze_ipcw_results.R")
  cat("\n✓ Analysis outputs generated successfully\n\n")
}, error = function(e) {
  cat("\n✗ Error in analysis:\n")
  print(e)
  cat("\nTrying to identify the specific issue...\n")

  # check individual components
  if (!exists("p_continuous")) cat("- p_continuous plot not created\n")
  if (!exists("p_bias")) cat("- p_bias plot not created\n")
  if (!exists("p_categories")) cat("- p_categories plot not created\n")
  if (!exists("continuous_table")) cat("- continuous_table not created\n")
  if (!exists("categorical_table")) cat("- categorical_table not created\n")
  if (!exists("overall_table")) cat("- overall_table not created\n")
})

# Step 4: Generate diagnostic outputs
cat("\nStep 4: Generating diagnostic outputs...\n")

tryCatch({
  source("generate_ipcw_diagnostics.R")
  cat("\n✓ Diagnostic outputs generated successfully\n\n")
}, error = function(e) {
  cat("\n✗ Error in diagnostics:\n")
  print(e)
})

# Summary
cat("\n=== PIPELINE SUMMARY ===\n")

# check what files were created
results_dir <- here::here("results")
figures_dir <- file.path(results_dir, "figures")
tables_dir <- file.path(results_dir, "tables")

if (dir.exists(figures_dir)) {
  figures <- list.files(figures_dir, pattern = "^ipcw_.*\\.png$")
  cat("\nFigures created (", length(figures), "):\n", sep = "")
  if (length(figures) > 0) {
    cat(paste("- ", figures, "\n", sep = ""), sep = "")
  }
}

if (dir.exists(tables_dir)) {
  tables <- list.files(tables_dir, pattern = "^ipcw_.*\\.html$")
  cat("\nTables created (", length(tables), "):\n", sep = "")
  if (length(tables) > 0) {
    cat(paste("- ", tables, "\n", sep = ""), sep = "")
  }
}

summary_files <- list.files(results_dir, pattern = "^ipcw_.*\\.txt$")
if (length(summary_files) > 0) {
  cat("\nSummary files created:\n")
  cat(paste("- ", summary_files, "\n", sep = ""), sep = "")
}

cat("\n=== PIPELINE COMPLETE ===\n")
cat("To view results, check the results/ directory\n")
