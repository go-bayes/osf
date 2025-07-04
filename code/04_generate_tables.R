# Generate Tables for Trust in Science Analysis
# This script creates all tables for the manuscript
# joseph.bulbulia@gmail.com

# setup
cat("==========================================================================\n")
cat("STEP 4: GENERATE TABLES\n")
cat("==========================================================================\n\n")

# load required packages
library(tidyverse)
library(here)
library(knitr)
library(kableExtra)
library(parameters)
library(glue)

# source configuration
source(("code/config/config.R"))

# load model outputs
cat("Loading model outputs...\n")
models_observed <- readRDS(("results/model_outputs/models_observed.rds"))
predictions_all <- readRDS(("results/model_outputs/predictions_all.rds"))
data_summary <- readRDS(("results/model_outputs/model_summaries.rds"))$data_info
cat("  ✓ Model outputs loaded\n\n")

# ========================================================================
# TABLE 1: DATA SUMMARY
# ========================================================================
cat("Creating Table 1: Data Summary...\n")

# calculate missing data percentages
missing_summary <- data.frame(
  Variable = c("Social Value of Science", "Trust in Scientists"),
  Variable_Name = c("trust_science", "trust_scientists"),
  Missing_N = c(
    data_summary$missing_by_var["trust_science"],
    data_summary$missing_by_var["trust_scientists"]
  ),
  Total_N = data_summary$n_total
) %>%
  mutate(
    Missing_Percent = round(100 * Missing_N / Total_N, 1),
    Complete_N = Total_N - Missing_N,
    Complete_Percent = round(100 * Complete_N / Total_N, 1)
  )

# create wave summary
wave_summary <- data.frame(
  Wave = c("Wave 11", "Wave 12", "Wave 13", "Wave 14"),
  Year = c("2019-2020", "2020-2021", "2021-2022", "2022-2024"),
  Time_Point = 0:3,
  Description = c(
    "Baseline (pre-COVID)",
    "COVID-19 response period", 
    "Post-vaccination rollout",
    "Endemic phase"
  )
)

# combine into table 1
table1 <- kable(
  wave_summary,
  format = "html",
  caption = "Table 1. Study Waves and Time Points",
  col.names = c("Wave", "Year", "Time Point", "Description")
) %>%
  kable_styling(full_width = FALSE, position = "left")

# save table 1
cat(as.character(table1), file = ("results/tables/table1_wave_summary.html"))

cat("  ✓ Table 1 created\n")

# ========================================================================
# TABLE 2: MISSING DATA PATTERNS
# ========================================================================
cat("\nCreating Table 2: Missing Data Patterns...\n")

table2 <- kable(
  missing_summary %>%
    select(Variable, Complete_N, Complete_Percent, Missing_N, Missing_Percent),
  format = "html",
  caption = glue("Table 2. Missing Data Summary (N = {data_summary$n_total})"),
  col.names = c("Variable", "Complete (n)", "Complete (%)", "Missing (n)", "Missing (%)"),
  digits = 1
) %>%
  kable_styling(full_width = FALSE, position = "left")

# save table 2
cat(as.character(table2), file = ("results/tables/table2_missing_data.html"))

cat("  ✓ Table 2 created\n")

# ========================================================================
# TABLE 3: MODEL ESTIMATES
# ========================================================================
cat("\nCreating Table 3: Model Estimates...\n")

# function to extract key estimates
extract_estimates <- function(predictions, model_type, outcome) {
  df <- as.data.frame(predictions)
  
  # get estimates at each time point
  estimates <- df %>%
    filter(x %in% 0:4) %>%
    mutate(
      estimate_ci = glue("{round(predicted, 3)} [{round(conf.low, 3)}, {round(conf.high, 3)}]"),
      year_label = case_when(
        x == 0 ~ "2019",
        x == 1 ~ "2020",
        x == 2 ~ "2021",
        x == 3 ~ "2022",
        x == 4 ~ "2023"
      )
    ) %>%
    select(year_label, estimate_ci)
  
  # calculate change (from 2019 to 2023)
  change <- round(df$predicted[df$x == 4] - df$predicted[df$x == 0], 3)
  
  return(list(estimates = estimates, change = change))
}

# extract continuous estimates
science_obs_cont <- extract_estimates(predictions_all$observed$gee_science, "Observed", "Science")
science_imp_cont <- extract_estimates(predictions_all$imputed$gee_science, "Imputed", "Science")
scientists_obs_cont <- extract_estimates(predictions_all$observed$gee_scientists, "Observed", "Scientists")
scientists_imp_cont <- extract_estimates(predictions_all$imputed$gee_scientists, "Imputed", "Scientists")

# create estimates table
estimates_table <- data.frame(
  Outcome = rep(c("Social Value of Science", "Trust in Scientists"), each = 10),
  Data = rep(rep(c("Observed", "Imputed"), each = 5), 2),
  Year = rep(c("2019", "2020", "2021", "2022", "2023"), 4)
)

# add estimates
estimates_table$Estimate <- c(
  science_obs_cont$estimates$estimate_ci,
  science_imp_cont$estimates$estimate_ci,
  scientists_obs_cont$estimates$estimate_ci,
  scientists_imp_cont$estimates$estimate_ci
)

# reshape to wide format
estimates_wide <- estimates_table %>%
  pivot_wider(names_from = Year, values_from = Estimate) %>%
  mutate(
    Change = c(
      science_obs_cont$change,
      science_imp_cont$change,
      scientists_obs_cont$change,
      scientists_imp_cont$change
    )
  )

# create table 3
table3 <- kable(
  estimates_wide,
  format = "html",
  caption = "Table 3. Marginal Mean Estimates (1-7 Scale) with 95% Confidence Intervals",
  align = c("l", "l", "c", "c", "c", "c", "c", "c")
) %>%
  kable_styling(full_width = TRUE, position = "left") %>%
  column_spec(2, bold = TRUE) %>%
  add_header_above(c(" " = 2, "Year" = 5, " " = 1))

# save table 3
cat(as.character(table3), file = ("results/tables/table3_estimates.html"))

cat("  ✓ Table 3 created\n")

# ========================================================================
# TABLE 4: MODEL COEFFICIENTS
# ========================================================================
cat("\nCreating Table 4: Model Coefficients...\n")

# function to extract model parameters
extract_model_params <- function(model, model_name) {
  tryCatch({
    if (inherits(model, "polr")) {
      # handle polr models manually
      coefs <- coef(model)
      se <- sqrt(diag(vcov(model)))
      
      df <- data.frame(
        Model = model_name,
        Term = names(coefs),
        Estimate = round(coefs, 4),
        SE = round(se[1:length(coefs)], 4),
        stringsAsFactors = FALSE
      )
    } else {
      # use parameters for other models
      params <- parameters::model_parameters(model, ci_method = "wald")
      
      df <- data.frame(
        Model = model_name,
        Term = params$Parameter,
        Estimate = round(params$Coefficient, 4),
        SE = round(params$SE, 4),
        stringsAsFactors = FALSE
      )
    }
    
    return(df)
  }, error = function(e) {
    cat("  Warning: Could not extract parameters for", model_name, "\n")
    return(NULL)
  })
}

# extract parameters for key models
params_list <- list(
  extract_model_params(models_observed$gee_science, "GEE: Science (Observed)"),
  extract_model_params(models_observed$gee_scientists, "GEE: Scientists (Observed)"),
  extract_model_params(models_observed$polr_science, "POLR: Science (Observed)"),
  extract_model_params(models_observed$polr_scientists, "POLR: Scientists (Observed)")
)

# remove NULL entries
params_list <- params_list[!sapply(params_list, is.null)]

# combine
all_params <- bind_rows(params_list)

# create table 4 if we have parameters
if (length(params_list) > 0) {
  table4 <- kable(
    all_params,
    format = "html",
    caption = "Table 4. Model Coefficients",
    digits = 4
  ) %>%
    kable_styling(full_width = TRUE, position = "left") %>%
    collapse_rows(columns = 1, valign = "top")
  
  # save table 4
  cat(as.character(table4), file = ("results/tables/table4_coefficients.html"))
  cat("  ✓ Table 4 created\n")
} else {
  cat("  ! No model parameters extracted\n")
}

# ========================================================================
# SUPPLEMENTARY TABLES
# ========================================================================
cat("\nCreating supplementary tables...\n")

# s1: categorical outcome probabilities
cat("  - Creating Table S1: Categorical probabilities...\n")

# extract categorical probabilities
extract_cat_probs <- function(predictions, outcome_name, data_type) {
  df <- as.data.frame(predictions)
  
  df %>%
    filter(x %in% c(0, 4)) %>%
    mutate(
      prob_ci = glue("{round(predicted * 100, 1)}% [{round(conf.low * 100, 1)}, {round(conf.high * 100, 1)}]"),
      year_label = ifelse(x == 0, "2019", "2023"),
      outcome = outcome_name,
      data = data_type
    ) %>%
    select(outcome, data, year_label, response.level, prob_ci)
}

cat_probs <- bind_rows(
  extract_cat_probs(predictions_all$observed$polr_science, "Science", "Observed"),
  extract_cat_probs(predictions_all$imputed$polr_science, "Science", "Imputed"),
  extract_cat_probs(predictions_all$observed$polr_scientists, "Scientists", "Observed"),
  extract_cat_probs(predictions_all$imputed$polr_scientists, "Scientists", "Imputed")
)

# reshape for table
cat_probs_wide <- cat_probs %>%
  unite("key", c(outcome, data, response.level), sep = "_") %>%
  pivot_wider(names_from = year_label, values_from = prob_ci)

table_s1 <- kable(
  cat_probs_wide,
  format = "html",
  caption = "Table S1. Predicted Probabilities for Categorical Outcomes",
  col.names = c("Outcome_Data_Level", "2019", "2023")
) %>%
  kable_styling(full_width = FALSE, position = "left")

cat(as.character(table_s1), file = ("results/tables/table_s1_categorical.html"))

cat("  ✓ Supplementary tables created\n")

# ========================================================================
# SAVE SUMMARY CSV FILES
# ========================================================================
cat("\nSaving CSV versions of key tables...\n")

# save key estimates as csv
write.csv(estimates_wide, 
          ("results/tables/marginal_means_estimates.csv"),
          row.names = FALSE)

# save model parameters as csv
write.csv(all_params,
          ("results/tables/model_coefficients.csv"), 
          row.names = FALSE)

# save categorical probabilities
write.csv(cat_probs,
          ("results/tables/categorical_probabilities.csv"),
          row.names = FALSE)

cat("  ✓ CSV files saved\n")

# ========================================================================
# SUMMARY
# ========================================================================
cat("\n==========================================================================\n")
cat("TABLE GENERATION COMPLETE\n")
cat("==========================================================================\n")
cat("Tables saved to: results/tables/\n")
cat("\nMain outputs:\n")
cat("  - HTML tables for manuscript\n")
cat("  - CSV files for further analysis\n")
cat("\nKey findings:\n")
cat("  - Social Value of Science:", science_imp_cont$change, "point change (imputed data)\n")
cat("  - Trust in Scientists:", scientists_imp_cont$change, "point change (imputed data)\n")
cat("\nNext step: Run 05_extract_results.R\n")