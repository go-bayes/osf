# Function to check and fix the mids object factor variables
# This ensures trust_science_factor and trust_scientists_factor exist

fix_mids_factors <- function(mids_obj, factor_vars, verbose = TRUE) {
  # check current mids object
  if (verbose) cat("Checking mids object for factor variables...\n")
  
  test_complete <- mice::complete(mids_obj, 1)
  
  # check if all requested factor vars exist
  missing_vars <- setdiff(factor_vars, names(test_complete))
  
  if (length(missing_vars) == 0) {
    if (verbose) cat("✓ All factor variables already exist in mids object!\n")
    return(mids_obj)
  }
  
  if (verbose) {
    cat("✗ Missing factor variables:", paste(missing_vars, collapse = ", "), "\n")
    cat("Adding factor variables to mids object...\n")
  }
  
  # function to add factors to a single dataset
  add_factors <- function(data) {
    if ("trust_science_factor" %in% missing_vars && "trust_science" %in% names(data)) {
      data$trust_science_factor <- factor(
        case_when(
          data$trust_science <= 3 ~ "low",
          data$trust_science <= 5 ~ "med",
          data$trust_science > 5 ~ "high"
        ),
        levels = c("low", "med", "high"),
        ordered = TRUE
      )
    }
    
    if ("trust_scientists_factor" %in% missing_vars && "trust_scientists" %in% names(data)) {
      data$trust_scientists_factor <- factor(
        case_when(
          data$trust_scientists <= 3 ~ "low",
          data$trust_scientists <= 5 ~ "med",
          data$trust_scientists > 5 ~ "high"
        ),
        levels = c("low", "med", "high"),
        ordered = TRUE
      )
    }
    
    return(data)
  }
  
  # apply to original data
  mids_obj$data <- add_factors(mids_obj$data)
  
  # update imp list for new variables (they have no missing values)
  for (var in missing_vars) {
    if (!var %in% names(mids_obj$imp)) {
      mids_obj$imp[[var]] <- matrix(NA, nrow = 0, ncol = mids_obj$m)
    }
  }
  
  # update nmis for new variables
  for (var in missing_vars) {
    mids_obj$nmis[var] <- 0
  }
  
  # verify transformation worked
  test_complete <- mice::complete(mids_obj, 1)
  still_missing <- setdiff(factor_vars, names(test_complete))
  
  if (length(still_missing) == 0) {
    if (verbose) cat("✓ Successfully added all factor variables!\n")
  } else {
    stop("Failed to add factor variables: ", paste(still_missing, collapse = ", "))
  }
  
  return(mids_obj)
}

# helper function to pool predictions from multiple models
pool_predictions <- function(pred_list) {
  # check if we have a valid list
  if (!is.list(pred_list) || length(pred_list) == 0) {
    stop("pred_list must be a non-empty list")
  }
  
  # extract predictions from each imputation
  preds <- sapply(pred_list, function(p) {
    if (is.data.frame(p) || is.list(p)) {
      return(p$predicted)
    } else {
      stop("Each element must have a 'predicted' component")
    }
  })
  
  # ensure preds is a matrix
  if (!is.matrix(preds)) {
    preds <- as.matrix(preds)
  }
  
  # pool using Rubin's rules (simple average for predictions)
  pooled <- rowMeans(preds)
  
  # create pooled prediction object
  result <- pred_list[[1]]
  result$predicted <- pooled
  
  # pool standard errors if available
  if ("std.error" %in% names(result)) {
    ses <- sapply(pred_list, function(p) p$std.error)
    if (!is.matrix(ses)) {
      ses <- as.matrix(ses)
    }
    # within-imputation variance
    w_var <- rowMeans(ses^2)
    # between-imputation variance
    if (ncol(preds) > 1) {
      b_var <- apply(preds, 1, function(x) stats::var(x))
    } else {
      b_var <- rep(0, nrow(preds))
    }
    # total variance
    total_var <- w_var + b_var + b_var/length(pred_list)
    result$std.error <- sqrt(total_var)
    
    # update confidence intervals
    result$conf.low <- result$predicted - 1.96 * result$std.error
    result$conf.high <- result$predicted + 1.96 * result$std.error
  }
  
  return(result)
}