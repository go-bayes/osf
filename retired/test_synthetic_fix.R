# Quick test of fixed synthetic data
set.seed(123)
source("data/synthetic/generate_synthetic_data.R")

# Generate small test data
test_data <- generate_synthetic_trust_data(
  n_participants = 100,
  n_waves = 4,
  baseline_year = 2019,
  seed = 123
)

cat("\nTest successful!\n")
cat("Rows generated:", nrow(test_data), "\n")
cat("Columns:", paste(names(test_data), collapse = ", "), "\n")
cat("\nWeight summary:\n")
print(summary(test_data$weights))

# Check weight variation by demographics
cat("\nMean weights by group:\n")
cat("By gender:\n")
print(tapply(test_data$weights[test_data$wave == 0], 
             test_data$gender[test_data$wave == 0], mean))
cat("\nBy age group:\n")
age_groups <- cut(test_data$age_baseline[test_data$wave == 0], 
                  breaks = c(0, 35, 45, 55, 100))
print(tapply(test_data$weights[test_data$wave == 0], age_groups, mean))

# Check if oracle data exists
if (file.exists("oracle_trust_data.csv")) {
  oracle <- read.csv("oracle_trust_data.csv")
  cat("\nOracle data created successfully with", nrow(oracle), "rows\n")
  cat("Oracle columns:", paste(names(oracle), collapse = ", "), "\n")
}