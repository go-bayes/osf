# Script to check and fix the mids object factor variables
# This ensures trust_science_factor and trust_scientists_factor exist

# helper function to read qs files (or use base R if qs not available)
here_read_qs <- function(filename, directory) {
  filepath <- file.path(directory, paste0(filename, ".qs"))
  if (requireNamespace("qs", quietly = TRUE)) {
    return(qs::qread(filepath))
  } else {
    # fallback to rds if qs not available
    filepath_rds <- file.path(directory, paste0(filename, ".rds"))
    if (file.exists(filepath_rds)) {
      return(readRDS(filepath_rds))
    } else {
      stop("Neither qs nor rds file found for: ", filename)
    }
  }
}

# helper function to save files
here_save <- function(object, filename, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  if (requireNamespace("qs", quietly = TRUE)) {
    qs::qsave(object, file.path(directory, paste0(filename, ".qs")))
  } else {
    # fallback to rds
    saveRDS(object, file.path(directory, paste0(filename, ".rds")))
  }
}

cat("=== CHECKING AND FIXING MIDS OBJECT ===\n\n")

# First, check if a pre-transformed mids object exists
cat("1. Checking for pre-saved mids object...\n")
mids_file <- file.path(push_mods, "mids_obj.qs")

if (file.exists(mids_file)) {
  cat("   Found pre-saved mids object. Loading...\n")
  mids_obj_saved <- here_read_qs("mids_obj", push_mods)
  
  # Check if it has the factor variables
  test_complete <- mice::complete(mids_obj_saved, 1)
  if ("trust_science_factor" %in% names(test_complete)) {
    cat("   ✓ Pre-saved mids object has factor variables!\n")
    cat("   Using the pre-saved version.\n")
    mids_obj <- mids_obj_saved
    rm(mids_obj_saved)
  } else {
    cat("   ✗ Pre-saved mids object lacks factor variables.\n")
    rm(mids_obj_saved)
  }
} else {
  cat("   No pre-saved mids object found.\n")
}

# Check current mids object
cat("\n2. Checking current mids object...\n")
test_complete <- mice::complete(mids_obj, 1)
if ("trust_science_factor" %in% names(test_complete)) {
  cat("   ✓ Current mids object already has factor variables!\n")
  cat("   No transformation needed.\n")
} else {
  cat("   ✗ Current mids object lacks factor variables.\n")
  cat("   Applying transformation...\n\n")
  
  # Apply transformation inline (since transform_mids_add_factors.R is not in this package)
  # Transform each imputation to add factor variables
  cat("   Transforming mids object to add factor variables...\n")
  
  # Function to add factors to a single dataset
  add_factors <- function(data) {
    if (!"trust_science_factor" %in% names(data)) {
      data$trust_science_factor <- cut(
        data$trust_science,
        breaks = c(0, 3.5, 5.5, 8),
        labels = c("low", "med", "high"),
        include.lowest = TRUE
      )
    }
    
    if (!"trust_scientists_factor" %in% names(data)) {
      data$trust_scientists_factor <- cut(
        data$trust_scientists,
        breaks = c(0, 3.5, 5.5, 8),
        labels = c("low", "med", "high"),
        include.lowest = TRUE
      )
    }
    
    return(data)
  }
  
  # Apply to original data
  mids_obj$data <- add_factors(mids_obj$data)
  
  # Update imp list for new variables (they have no missing values)
  if (!"trust_science_factor" %in% names(mids_obj$imp)) {
    mids_obj$imp$trust_science_factor <- matrix(NA, nrow = 0, ncol = mids_obj$m)
  }
  if (!"trust_scientists_factor" %in% names(mids_obj$imp)) {
    mids_obj$imp$trust_scientists_factor <- matrix(NA, nrow = 0, ncol = mids_obj$m)
  }
  
  # Verify transformation worked
  test_complete <- mice::complete(mids_obj, 1)
  if ("trust_science_factor" %in% names(test_complete)) {
    cat("\n   ✓ Transformation successful!\n")
  } else {
    stop("Transformation failed - factor variables not created")
  }
}

# Optional: save the transformed mids object for future use
cat("\n3. Saving transformed mids object for future use...\n")
here_save(mids_obj, "mids_obj_with_factors", push_mods)
cat("   Saved as 'mids_obj_with_factors'\n")

cat("\n=== MIDS OBJECT READY ===\n")
cat("You can now run the multinomial analysis.\n")