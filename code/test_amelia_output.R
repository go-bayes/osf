# Check if Amelia actually creates imputations despite error
# joseph.bulbulia@gmail.com

cat("=== CHECKING AMELIA OUTPUT ===\n\n")

# load data
library(Amelia)
test_data <- read.csv("data/synthetic/synthetic_trust_data.csv", stringsAsFactors = FALSE)
test_data$wave <- as.character(test_data$wave)

# run amelia and capture output
cat("Running Amelia...\n")
am_result <- amelia(
  test_data,
  m = 3,
  idvars = c("id", "wave", "age_baseline", "weights"),
  noms = c("gender", "ethnicity"),
  ords = c("education"),
  bounds = matrix(c(1, 1, 7, 2, 1, 7), nrow = 2, ncol = 3, byrow = TRUE,
                  dimnames = list(c("trust_science", "trust_scientists"), NULL)),
  ts = "years",
  cs = "id",
  p2s = 0
)

# check what we got
cat("\nAmelia output check:\n")
cat("- Class:", class(am_result), "\n")
cat("- Number of imputations:", length(am_result$imputations), "\n")

if (length(am_result$imputations) > 0) {
  cat("\nFirst imputation summary:\n")
  imp1 <- am_result$imputations[[1]]
  cat("- Rows:", nrow(imp1), "\n")
  cat("- Columns:", ncol(imp1), "\n")
  
  # check missing data before and after
  cat("\nMissing data check:\n")
  cat("Original trust_science missing:", sum(is.na(test_data$trust_science)), "\n")
  cat("Imputed trust_science missing:", sum(is.na(imp1$trust_science)), "\n")
  
  # show some imputed values
  missing_rows <- which(is.na(test_data$trust_science))[1:5]
  cat("\nFirst 5 imputed values for trust_science:\n")
  print(data.frame(
    id = imp1$id[missing_rows],
    year = imp1$years[missing_rows],
    original = test_data$trust_science[missing_rows],
    imputed = imp1$trust_science[missing_rows]
  ))
}

cat("\n=== CHECK COMPLETE ===\n")