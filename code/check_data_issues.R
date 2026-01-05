# Check data issues
library(here)
data <- readRDS(here::here("data", "synthetic", "synthetic_trust_data.rds"))

# check for all-missing individuals
trust_by_id <- tapply(data$trust_science, data$id, function(x) sum(!is.na(x)))
all_missing_ids <- names(trust_by_id)[trust_by_id == 0]
cat("IDs with all trust_science missing:", length(all_missing_ids), "\n")

# check baseline missingness
baseline <- data[data$years == 0, ]
cat("Baseline missing rate:", mean(is.na(baseline$trust_science)), "\n")

# check if there are numeric issues
cat("\nChecking numeric columns:\n")
num_cols <- sapply(data, is.numeric)
for (col in names(data)[num_cols]) {
  if (any(is.infinite(data[[col]]) | is.nan(data[[col]]), na.rm = TRUE)) {
    cat(col, "has Inf or NaN values\n")
  }
}
