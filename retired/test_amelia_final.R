# Final test with correct Amelia specification
# joseph.bulbulia@gmail.com

cat("=== FINAL AMELIA TEST ===\n\n")

# load data
library(Amelia)
test_data <- read.csv("data/synthetic/synthetic_trust_data.csv", stringsAsFactors = FALSE)

# convert wave to character as required
test_data$wave <- as.character(test_data$wave)

cat("Data info:\n")
cat("- Rows:", nrow(test_data), "\n")
cat("- Missing trust_science:", sum(is.na(test_data$trust_science)), "\n")
cat("- Missing trust_scientists:", sum(is.na(test_data$trust_scientists)), "\n\n")

# run amelia WITHOUT specifying trust variables as ordinal
# they're continuous anyway, so no need for ords
cat("Running Amelia with continuous trust variables...\n")

# only specify bounds, not ords for trust variables
bounds_matrix <- matrix(c(9, 1, 7, 10, 1, 7), nrow = 2, ncol = 3, byrow = TRUE)
test_data
am_result <- amelia(
  test_data,
  m = 3,
  idvars = c("wave", "age_baseline", "weights"),  # removed id from idvars
  noms = c("gender", "ethnicity"),
  ts = "years",
  cs = "id",
  bounds = bounds_matrix,
  p2s = 0
)

# check results
cat("\nAmelia results:\n")
cat("- Class:", class(am_result), "\n")
cat("- Number of imputations:", length(am_result$imputations), "\n")

if (length(am_result$imputations) > 0) {
  cat("\n✓ SUCCESS! Amelia created", length(am_result$imputations), "imputations\n")

  # check first imputation
  imp1 <- am_result$imputations[[1]]
  cat("\nFirst imputation check:\n")
  cat("- Complete trust_science:", sum(!is.na(imp1$trust_science)), "\n")
  cat("- Complete trust_scientists:", sum(!is.na(imp1$trust_scientists)), "\n")

  # save result for further use
  saveRDS(am_result, "test_amelia_result.rds")
  cat("\nSaved Amelia result to test_amelia_result.rds\n")
} else {
  cat("\n✗ FAILED: No imputations created\n")
  cat("Message:", am_result$message, "\n")
}

cat("\n=== TEST COMPLETE ===\n")

