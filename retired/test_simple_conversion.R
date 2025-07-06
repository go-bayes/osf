# Simple test of Amelia to MICE conversion
# This identifies where the missing data problem occurs

cat("=== SIMPLE CONVERSION TEST ===\n\n")

library(Amelia)
library(mice)
set.seed(123)

# create tiny dataset with more variables
n <- 20
dat <- data.frame(
  id = rep(1:5, each = 4),
  wave = rep(0:3, 5),
  age = rep(c(30, 45, 25, 60, 35), each = 4),
  gender = rep(c("M", "F", "F", "M", "F"), each = 4),
  trust = c(5, 6, NA, NA,  # person 1: drops out at wave 2
           4, 5, 5, NA,   # person 2: drops out at wave 3
           6, NA, NA, NA, # person 3: drops out at wave 1
           3, 4, 4, 5,    # person 4: complete
           5, 5, NA, NA), # person 5: drops out at wave 2
  trust2 = c(4, 5, NA, NA,  # person 1: drops out at wave 2
            3, 4, 4, NA,   # person 2: drops out at wave 3
            5, NA, NA, NA, # person 3: drops out at wave 1
            2, 3, 3, 4,    # person 4: complete
            4, 4, NA, NA)  # person 5: drops out at wave 2
)

cat("Original data:\n")
print(dat)
cat("\nMissing values:", sum(is.na(dat$trust)), "\n\n")

# run amelia
am_result <- amelia(dat, m = 2, cs = "id", ts = "wave", 
                   noms = "gender", idvars = c("age"), p2s = 0)

# check first imputation
imp1 <- am_result$imputations[[1]]
cat("After Amelia imputation:\n")
print(imp1)
cat("\nMissing values in Amelia output:", sum(is.na(imp1$trust)), "\n\n")

# now test simple MICE conversion
cat("=== TESTING MICE CONVERSION ===\n\n")

# Method 1: Direct conversion (what should work)
mids_simple <- list()
mids_simple$data <- dat  # original data with missing values
mids_simple$m <- 2
mids_simple$where <- is.na(dat)

# create imp slots
mids_simple$imp <- list()
for (var in names(dat)) {
  if (var %in% c("id", "wave")) {
    mids_simple$imp[[var]] <- matrix(NA, nrow = 0, ncol = 2)
  } else {
    # for trust variable, extract imputed values
    missing_idx <- which(is.na(dat[[var]]))
    if (length(missing_idx) > 0) {
      imp_matrix <- matrix(NA, nrow = length(missing_idx), ncol = 2)
      for (m in 1:2) {
        imp_matrix[, m] <- am_result$imputations[[m]][[var]][missing_idx]
      }
      mids_simple$imp[[var]] <- imp_matrix
      cat("Imputation matrix for", var, ":\n")
      print(imp_matrix)
    } else {
      mids_simple$imp[[var]] <- matrix(NA, nrow = 0, ncol = 2)
    }
  }
}

# set other required elements
mids_simple$nmis <- colSums(is.na(dat))
mids_simple$method <- rep("amelia", ncol(dat))
names(mids_simple$method) <- names(dat)
mids_simple$visitSequence <- names(dat)[mids_simple$nmis > 0]
mids_simple$predictorMatrix <- matrix(0, ncol(dat), ncol(dat))
class(mids_simple) <- "mids"

# test completion
cat("\n=== TESTING MICE COMPLETE ===\n")
tryCatch({
  complete1 <- mice::complete(mids_simple, 1)
  cat("Complete successful!\n")
  print(complete1)
  cat("\nMissing in completed data:", sum(is.na(complete1$trust)), "\n")
}, error = function(e) {
  cat("Error in complete:", e$message, "\n")
})

# Method 2: See what's in our actual conversion function
cat("\n=== UNDERSTANDING THE ISSUE ===\n")
cat("Original data shape:", dim(dat), "\n")
cat("Amelia imputation shape:", dim(imp1), "\n")
cat("Missing indices:", which(is.na(dat$trust)), "\n")
cat("Values at missing indices in Amelia:", imp1$trust[which(is.na(dat$trust))], "\n")