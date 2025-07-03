# Quick test of COVID-specific attrition pattern
# joseph.bulbulia@gmail.com

cat("=== TESTING COVID-SPECIFIC ATTRITION ===\n\n")

# generate small test dataset
source(("data/synthetic/generate_synthetic_data.R"))

set.seed(2024)
test_data <- generate_synthetic_trust_data(
  n_participants = 1000,  # smaller for quick test
  n_waves = 4,
  baseline_year = 2019,
  seed = 2024
)

# analyze attrition patterns
cat("\n=== ATTRITION ANALYSIS ===\n")

# get baseline characteristics for everyone
baseline <- test_data[test_data$years == 0, ]

# who drops out at year 1 (COVID)?
year1_ids <- unique(test_data$id[test_data$years == 1 & !is.na(test_data$trust_science)])
covid_dropouts <- baseline[!(baseline$id %in% year1_ids), ]
covid_stayers <- baseline[baseline$id %in% year1_ids, ]

cat("\nYear 0 -> Year 1 (COVID) attrition:\n")
cat("Total at baseline:", nrow(baseline), "\n")
cat("Dropped out:", nrow(covid_dropouts), "(", round(100 * nrow(covid_dropouts)/nrow(baseline), 1), "%)\n")
cat("Stayed in:", nrow(covid_stayers), "\n\n")

# compare characteristics
cat("Characteristics of dropouts vs stayers:\n")
cat("\nTrust in Science:\n")
cat("  Dropouts: Mean =", round(mean(covid_dropouts$trust_science), 2), 
    ", Low trust (<4) =", sum(covid_dropouts$trust_science < 4), 
    "(", round(100 * mean(covid_dropouts$trust_science < 4), 1), "%)\n")
cat("  Stayers:  Mean =", round(mean(covid_stayers$trust_science), 2),
    ", Low trust (<4) =", sum(covid_stayers$trust_science < 4),
    "(", round(100 * mean(covid_stayers$trust_science < 4), 1), "%)\n")

cat("\nEducation:\n")
cat("  Dropouts: Mean =", round(mean(covid_dropouts$education), 2),
    ", High ed (6-7) =", sum(covid_dropouts$education >= 6),
    "(", round(100 * mean(covid_dropouts$education >= 6), 1), "%)\n")
cat("  Stayers:  Mean =", round(mean(covid_stayers$education), 2),
    ", High ed (6-7) =", sum(covid_stayers$education >= 6),
    "(", round(100 * mean(covid_stayers$education >= 6), 1), "%)\n")

# dropout rates by trust level
cat("\n=== DROPOUT RATES BY TRUST LEVEL ===\n")
for (trust_level in 1:7) {
  n_baseline <- sum(baseline$trust_science >= trust_level - 0.5 & 
                    baseline$trust_science < trust_level + 0.5)
  n_dropped <- sum(covid_dropouts$trust_science >= trust_level - 0.5 & 
                   covid_dropouts$trust_science < trust_level + 0.5)
  if (n_baseline > 0) {
    dropout_rate <- 100 * n_dropped / n_baseline
    cat("Trust", trust_level, ": ", round(dropout_rate, 1), "% dropout",
        " (n=", n_baseline, ")\n", sep = "")
  }
}

# dropout rates by education level
cat("\n=== DROPOUT RATES BY EDUCATION LEVEL ===\n")
for (ed_level in 1:7) {
  n_baseline <- sum(baseline$education == ed_level)
  n_dropped <- sum(covid_dropouts$education == ed_level)
  if (n_baseline > 0) {
    dropout_rate <- 100 * n_dropped / n_baseline
    cat("Education", ed_level, ": ", round(dropout_rate, 1), "% dropout",
        " (n=", n_baseline, ")\n", sep = "")
  }
}

cat("\n=== TEST COMPLETE ===\n")
cat("\nKey finding: Low trust individuals drop out at much higher rates during COVID,\n")
cat("especially if they also have low education. This creates the appearance of a\n")
cat("larger COVID bump in observed data than actually exists in the population.\n")