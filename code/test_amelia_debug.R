# Debug Amelia error step by step
# joseph.bulbulia@gmail.com

cat("=== DEBUGGING AMELIA ERROR ===\n\n")

# load data
library(Amelia)
test_data <- read.csv("data/synthetic/synthetic_trust_data.csv", stringsAsFactors = FALSE)
test_data$wave <- as.character(test_data$wave)

cat("Data structure:\n")
str(test_data)

# test 1: minimal amelia call
cat("\n1. Testing minimal Amelia call (no ords, no bounds)...\n")
tryCatch({
  am1 <- amelia(
    test_data,
    m = 2,
    idvars = c("id", "wave", "age_baseline", "weights"),
    noms = c("gender", "ethnicity"),
    ts = "years",
    cs = "id",
    p2s = 0
  )
  cat("✓ Success!\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

# test 2: add education as ordinal
cat("\n2. Testing with education as ordinal...\n")
tryCatch({
  am2 <- amelia(
    test_data,
    m = 2,
    idvars = c("id", "wave", "age_baseline", "weights"),
    noms = c("gender", "ethnicity"),
    ords = c("education"),
    ts = "years",
    cs = "id",
    p2s = 0
  )
  cat("✓ Success!\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

# test 3: add bounds
cat("\n3. Testing with bounds...\n")
bounds_matrix <- matrix(c(1, 1, 7, 2, 1, 7), nrow = 2, ncol = 3, byrow = TRUE)
rownames(bounds_matrix) <- c("trust_science", "trust_scientists")

tryCatch({
  am3 <- amelia(
    test_data,
    m = 2,
    idvars = c("id", "wave", "age_baseline", "weights"),
    noms = c("gender", "ethnicity"),
    bounds = bounds_matrix,
    ts = "years",
    cs = "id",
    p2s = 0
  )
  cat("✓ Success!\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

# test 4: add trust variables as ordinal
cat("\n4. Testing with trust variables as ordinal...\n")
tryCatch({
  am4 <- amelia(
    test_data,
    m = 2,
    idvars = c("id", "wave", "age_baseline", "weights"),
    noms = c("gender", "ethnicity"),
    ords = c("trust_science", "trust_scientists"),
    ts = "years",
    cs = "id",
    p2s = 0
  )
  cat("✓ Success!\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})

cat("\n=== DEBUG COMPLETE ===\n")