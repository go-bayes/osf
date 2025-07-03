# Minimal test to isolate Amelia error
# joseph.bulbulia@gmail.com

cat("=== MINIMAL AMELIA TEST ===\n\n")

library(Amelia)

# create very simple panel data that should work
set.seed(123)
n_people <- 50
n_times <- 4

# create balanced panel
test_data <- data.frame(
  id = rep(1:n_people, each = n_times),
  years = rep(0:(n_times-1), n_people),
  wave = rep(2019:(2019+n_times-1), n_people),
  age = rep(sample(20:60, n_people, replace = TRUE), each = n_times),
  gender = rep(sample(c("M", "F"), n_people, replace = TRUE), each = n_times),
  trust = rnorm(n_people * n_times, mean = 5, sd = 1)
)

# add missing values (but not too many at baseline)
missing_idx <- sample(which(test_data$years > 0), size = 30)
test_data$trust[missing_idx] <- NA

# convert types
test_data$wave <- as.character(test_data$wave)

cat("Test data structure:\n")
str(test_data)
cat("\nMissing by year:\n")
print(table(test_data$years, is.na(test_data$trust)))

# try amelia
cat("\nRunning Amelia...\n")
am_result <- amelia(
  test_data,
  m = 2,
  idvars = c("wave", "age"),
  noms = c("gender"),
  ts = "years",
  cs = "id",
  p2s = 0
)

cat("\nResult:\n")
cat("- Class:", class(am_result), "\n")
cat("- Imputations:", length(am_result$imputations), "\n")

if (length(am_result$imputations) > 0) {
  cat("\n✓ SUCCESS!\n")
} else {
  cat("\n✗ Failed. Trying without p2s=0...\n")
  
  # try without p2s=0
  am_result2 <- amelia(
    test_data,
    m = 2,
    idvars = c("wave", "age"),
    noms = c("gender"),
    ts = "years",
    cs = "id"
  )
  
  cat("Result without p2s=0:\n")
  cat("- Imputations:", length(am_result2$imputations), "\n")
}