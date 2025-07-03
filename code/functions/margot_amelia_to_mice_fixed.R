#' Convert an Amelia object to a mice object with proper missing data handling
#'
#' This fixed version properly preserves the missing data pattern from the original data
#' rather than using the first imputation as the "original" data.
#'
#' @param amelia_obj An object of class `amelia`, containing imputed datasets
#' @param original_data Optional: the original data with missing values. If not provided,
#'   the function will attempt to reconstruct the missing pattern.
#' @param verbose Logical: print diagnostic information
#'
#' @return A `mids` object compatible with the `mice` package
#' 
#' @examples
#' # Convert amelia object to mice object
#' mids_obj <- margot_amelia_to_mice_fixed(amelia_output, original_data = dat_long_amelia_log)
margot_amelia_to_mice_fixed <- function(amelia_obj, original_data = NULL, verbose = TRUE) {
  # Verify input is an amelia object
  if (!inherits(amelia_obj, "amelia")) {
    stop("Input must be an amelia object")
  }
  
  # Get imputations
  imp_list <- amelia_obj$imputations
  n_imp <- length(imp_list)
  
  if (verbose) {
    cat("Converting Amelia object with", n_imp, "imputations to MICE format\n")
  }
  
  # Initialize mice format list
  mice_format <- list()
  
  # If original data is provided, use it; otherwise try to reconstruct
  if (!is.null(original_data)) {
    mice_format$data <- original_data
    if (verbose) cat("Using provided original data\n")
  } else {
    # Attempt to reconstruct original data by finding differences across imputations
    # This is the key fix - we need to identify what was originally missing
    if (verbose) cat("Reconstructing missing data pattern from imputations\n")
    
    # Start with first imputation as template
    mice_format$data <- imp_list[[1]]
    
    # For each variable, check if values differ across imputations
    # If they differ, it was likely originally missing
    for (var in names(imp_list[[1]])) {
      if (var %in% c(".imp", ".id")) next
      
      # Check if this variable has any variation across imputations
      values_across_imps <- sapply(imp_list, function(imp) imp[[var]])
      
      if (is.matrix(values_across_imps) || is.data.frame(values_across_imps)) {
        # For each observation, check if values vary across imputations
        varies <- apply(values_across_imps, 1, function(row) {
          # Handle numeric and factor variables
          if (is.numeric(row)) {
            # Use a small tolerance for numeric comparison
            return(max(row, na.rm = TRUE) - min(row, na.rm = TRUE) > .Machine$double.eps * 100)
          } else {
            # For factors/characters, check if all are the same
            return(length(unique(row)) > 1)
          }
        })
        
        # Set to NA where values vary (indicating it was imputed)
        mice_format$data[varies, var] <- NA
      }
    }
  }
  
  # Remove .imp column if it exists
  if (".imp" %in% names(mice_format$data)) {
    mice_format$data$.imp <- NULL
  }
  
  # Create where matrix - TRUE indicates missing values
  mice_format$where <- is.na(mice_format$data)
  
  # Get all variables except special columns
  all_vars <- setdiff(names(mice_format$data), c(".imp", ".id"))
  
  # Find imputed variables (those with any missing values)
  imputed_vars <- names(which(colSums(mice_format$where) > 0))
  imputed_vars <- setdiff(imputed_vars, c(".imp", ".id"))
  
  if (verbose) {
    cat("Found", length(imputed_vars), "variables with missing data:\n")
    if (length(imputed_vars) <= 10) {
      cat("  ", paste(imputed_vars, collapse = ", "), "\n")
    } else {
      cat("  ", paste(head(imputed_vars, 10), collapse = ", "), "...\n")
    }
  }
  
  # Create imp list for imputed variables
  mice_format$imp <- vector("list", length(all_vars))
  names(mice_format$imp) <- all_vars
  
  # Fill imp list for variables with missing data
  for (var in imputed_vars) {
    # Skip factor variables - they shouldn't be imputed
    if (is.factor(mice_format$data[[var]])) {
      if (verbose) {
        cat("  Skipping factor variable:", var, "\n")
      }
      mice_format$imp[[var]] <- matrix(NA, nrow = 0, ncol = n_imp)
      next
    }
    
    # Get indices of missing values
    missing_idx <- which(mice_format$where[, var])
    n_missing <- length(missing_idx)
    
    if (n_missing > 0) {
      # Create matrix to store imputed values
      imp_matrix <- matrix(NA, nrow = n_missing, ncol = n_imp)
      
      # Fill with imputed values from each imputation
      for (m in 1:n_imp) {
        # Get imputed values
        imp_values <- imp_list[[m]][[var]][missing_idx]
        
        # Check dimensions match
        if (length(imp_values) != n_missing) {
          warning("Dimension mismatch for variable ", var, 
                  ": expected ", n_missing, " values, got ", length(imp_values))
          next
        }
        
        imp_matrix[, m] <- imp_values
      }
      
      mice_format$imp[[var]] <- imp_matrix
      
      if (verbose && var %in% head(imputed_vars, 3)) {
        cat("  Variable", var, ": imputed", n_missing, "missing values\n")
      }
    } else {
      # No missing values for this variable
      mice_format$imp[[var]] <- matrix(NA, nrow = 0, ncol = n_imp)
    }
  }
  
  # For variables without missing data, create empty matrices
  non_imputed_vars <- setdiff(all_vars, imputed_vars)
  for (var in non_imputed_vars) {
    mice_format$imp[[var]] <- matrix(NA, nrow = 0, ncol = n_imp)
  }
  
  # Set MICE-specific attributes
  mice_format$m <- n_imp
  mice_format$nmis <- colSums(mice_format$where)
  mice_format$method <- rep("amelia", length(all_vars))
  names(mice_format$method) <- all_vars
  mice_format$predictorMatrix <- matrix(0, length(all_vars), length(all_vars))
  rownames(mice_format$predictorMatrix) <- colnames(mice_format$predictorMatrix) <- all_vars
  mice_format$visitSequence <- imputed_vars
  mice_format$seed <- NA
  mice_format$iteration <- 1
  mice_format$lastSeedValue <- .Random.seed
  mice_format$chainMean <- NULL
  mice_format$chainVar <- NULL
  mice_format$loggedEvents <- NULL
  mice_format$version <- packageVersion("mice")
  mice_format$date <- Sys.Date()
  mice_format$call <- match.call()
  
  # Set class
  oldClass(mice_format) <- "mids"
  
  if (verbose) {
    # Summary statistics
    total_cells <- nrow(mice_format$data) * length(all_vars)
    missing_cells <- sum(mice_format$where)
    cat("\nConversion complete:\n")
    cat("  Total observations:", nrow(mice_format$data), "\n")
    cat("  Total variables:", length(all_vars), "\n")
    cat("  Variables with missing data:", length(imputed_vars), "\n")
    cat("  Total missing cells:", missing_cells, 
        sprintf("(%.1f%%)", 100 * missing_cells / total_cells), "\n")
    
    # Check if we have increasing missingness over time
    if ("years" %in% names(mice_format$data) || "wave" %in% names(mice_format$data)) {
      time_var <- if ("years" %in% names(mice_format$data)) "years" else "wave"
      miss_by_time <- aggregate(mice_format$where, 
                                by = list(time = mice_format$data[[time_var]]), 
                                FUN = mean, na.rm = TRUE)
      cat("\nMissingness by", time_var, ":\n")
      print(round(miss_by_time[1:min(5, nrow(miss_by_time)), ], 3))
    }
  }
  
  return(mice_format)
}