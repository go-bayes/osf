# Check data structure for Amelia compatibility
# joseph.bulbulia@gmail.com

cat("=== CHECKING DATA STRUCTURE ===\n\n")

# load data
test_data <- read.csv("data/synthetic/synthetic_trust_data.csv", stringsAsFactors = FALSE)

# check panel structure
cat("Panel structure check:\n")
cat("- Unique IDs:", length(unique(test_data$id)), "\n")
cat("- Unique years:", sort(unique(test_data$years)), "\n")
cat("- Rows per ID:", length(unique(test_data$years)), "\n")

# check if panel is balanced
panel_check <- table(test_data$id, test_data$years)
cat("\nPanel balance check:\n")
cat("- Min obs per person:", min(rowSums(panel_check > 0)), "\n")
cat("- Max obs per person:", max(rowSums(panel_check > 0)), "\n")

# check for duplicate id-year combinations
dup_check <- paste(test_data$id, test_data$years)
cat("\nDuplicate check:\n")
cat("- Any duplicate id-year pairs?", any(duplicated(dup_check)), "\n")

# check variable types
cat("\nVariable types:\n")
str(test_data)

# try minimal dataset
cat("\n=== TESTING WITH MINIMAL DATASET ===\n")

# create minimal test data
library(Amelia)

# create a very simple panel
simple_data <- data.frame(
  id = rep(1:10, each = 4),
  year = rep(0:3, 10),
  x = rnorm(40),
  y = rnorm(40)
)

# add some missing values
simple_data$y[sample(1:40, 10)] <- NA

cat("\nSimple data structure:\n")
str(simple_data)

# try amelia on simple data
cat("\nTrying Amelia on simple data...\n")
tryCatch({
  am_simple <- amelia(
    simple_data,
    m = 2,
    ts = "year",
    cs = "id",
    p2s = 0
  )
  cat("✓ Success! Generated", length(am_simple$imputations), "imputations\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\n=== STRUCTURE CHECK COMPLETE ===\n")