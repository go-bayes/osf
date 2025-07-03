# Setup Environment for Trust in Science Analysis
# This script installs/loads required packages and sets up the analysis environment
# joseph.bulbulia@gmail.com

cat("==========================================================================\n")
cat("TRUST IN SCIENCE: REPRODUCIBLE ANALYSIS SETUP\n")
cat("==========================================================================\n\n")

# set cran mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# required packages
required_packages <- c(
  # data manipulation
  "tidyverse",
  "here",
  
  # missing data
  "mice",
  "Amelia",
  
  # statistical models
  "geepack",      # for gee
  "MASS",         # for polr
  "nnet",         # for reference (not used in final)
  "ordinal",      # for reference
  
  # model tools
  "ggeffects",    # for predictions
  "parameters",   # for model summaries
  "sandwich",     # for robust ses
  "lmtest",       # for testing
  
  # splines
  "splines",
  
  # visualization
  "patchwork",    # for combining plots
  "ggplot2",
  
  # utilities
  "glue"
)

# check and install missing packages
cat("Checking required packages...\n")
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
} else {
  cat("All required packages are installed.\n")
}

# load packages
cat("\nLoading packages...\n")
invisible(lapply(required_packages, library, character.only = TRUE))

# check for margot package (may need github installation)
if (!"margot" %in% installed.packages()[, "Package"]) {
  cat("\nNote: The 'margot' package is required but not on CRAN.\n")
  cat("To install, run: devtools::install_github('go-bayes/margot')\n")
  cat("For this demo, we'll work without it.\n")
}

# check for boilerplate package
if (!"boilerplate" %in% installed.packages()[, "Package"]) {
  cat("\nNote: The 'boilerplate' package is used in the manuscript.\n")
  cat("To install, run: devtools::install_github('go-bayes/boilerplate')\n")
}

# set options
cat("\nSetting global options...\n")
options(
  scipen = 999,           # avoid scientific notation
  digits = 3,             # default digits
  width = 80              # console width
)

# set ggplot theme
theme_set(theme_minimal(base_size = 12))

# create output directories if they don't exist
cat("\nCreating output directories...\n")
output_dirs <- c(
  "results/figures",
  "results/tables", 
  "results/model_outputs",
  "results/predictions"
)

for (dir in output_dirs) {
  if (!dir.exists((dir))) {
    dir.create((dir), recursive = TRUE)
    cat("Created:", dir, "\n")
  }
}

# source configuration
cat("\nLoading configuration...\n")
config_file <- ("code/config/config.R")
if (file.exists(config_file)) {
  source(config_file)
} else {
  cat("No config file found. Using defaults.\n")
  # set defaults
  USE_REAL_DATA <- FALSE
  N_IMPUTATIONS <- 10
  FIGURE_HEIGHT <- 10
  FIGURE_WIDTH <- 10
  FIGURE_DPI <- 300
}

# print session info
cat("\n==========================================================================\n")
cat("SESSION INFORMATION\n")
cat("==========================================================================\n")
cat("R version:", R.version$version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("Date:", Sys.Date(), "\n\n")

# print package versions
cat("Key package versions:\n")
key_packages <- c("mice", "geepack", "MASS", "ggeffects", "tidyverse")
for (pkg in key_packages) {
  if (pkg %in% installed.packages()[, "Package"]) {
    cat(sprintf("  %-15s %s\n", paste0(pkg, ":"), packageVersion(pkg)))
  }
}

cat("\n==========================================================================\n")
cat("SETUP COMPLETE\n")
cat("==========================================================================\n")
cat("\nNext steps:\n")
cat("1. To run demo with synthetic data: source('code/demo/run_synthetic_demo.R')\n")
cat("2. To run full analysis: run scripts 01-06 in order\n")
cat("\nFor help, see documentation/README.md\n")