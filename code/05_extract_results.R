# Extract Key Results for Trust in Science Analysis
# This script extracts and saves key results for the manuscript
# joseph.bulbulia@gmail.com

# setup
cat("==========================================================================\n")
cat("STEP 5: EXTRACT KEY RESULTS\n")
cat("==========================================================================\n\n")

# load required packages
library(tidyverse)
library(here)
library(glue)

# source configuration
source(("code/config/config.R"))

# load all results
cat("Loading all results...\n")
predictions_all <- readRDS(("results/model_outputs/predictions_all.rds"))
model_summaries <- readRDS(("results/model_outputs/model_summaries.rds"))
data_summary <- model_summaries$data_info
cat("  ✓ Results loaded\n\n")

# ========================================================================
# EXTRACT KEY ESTIMATES
# ========================================================================
cat("Extracting key estimates...\n")

# function to extract point estimates and changes
extract_key_values <- function(predictions, outcome_name, data_type) {
  df <- as.data.frame(predictions)
  
  # baseline (year 0)
  baseline <- df %>%
    filter(years == 0) %>%
    summarise(
      estimate = round(predicted, 3),
      ci_low = round(conf.low, 3),
      ci_high = round(conf.high, 3),
      se = round(std.error, 3)
    ) %>%
    mutate(
      ci_string = glue("[{ci_low}, {ci_high}]"),
      year = "baseline"
    )
  
  # covid peak (year 1) 
  covid_peak <- df %>%
    filter(years == 1) %>%
    summarise(
      estimate = round(predicted, 3),
      ci_low = round(conf.low, 3),
      ci_high = round(conf.high, 3),
      se = round(std.error, 3)
    ) %>%
    mutate(
      ci_string = glue("[{ci_low}, {ci_high}]"),
      year = "covid_peak"
    )
  
  # final (year 3)
  final <- df %>%
    filter(years == 3) %>%
    summarise(
      estimate = round(predicted, 3),
      ci_low = round(conf.low, 3),
      ci_high = round(conf.high, 3),
      se = round(std.error, 3)
    ) %>%
    mutate(
      ci_string = glue("[{ci_low}, {ci_high}]"),
      year = "final"
    )
  
  # changes
  change_baseline_to_peak <- round(covid_peak$estimate - baseline$estimate, 3)
  change_peak_to_final <- round(final$estimate - covid_peak$estimate, 3)
  change_overall = round(final$estimate - baseline$estimate, 3)
  
  # combine
  results <- list(
    outcome = outcome_name,
    data_type = data_type,
    baseline = baseline$estimate,
    baseline_ci = baseline$ci_string,
    covid_peak = covid_peak$estimate,
    covid_peak_ci = covid_peak$ci_string,
    final = final$estimate,
    final_ci = final$ci_string,
    change_to_peak = change_baseline_to_peak,
    change_from_peak = change_peak_to_final,
    change_overall = change_overall
  )
  
  return(results)
}

# extract for all models
results_list <- list(
  science_observed = extract_key_values(
    predictions_all$observed$gee_science,
    "Social Value of Science",
    "Observed"
  ),
  science_imputed = extract_key_values(
    predictions_all$imputed$gee_science,
    "Social Value of Science", 
    "Imputed"
  ),
  scientists_observed = extract_key_values(
    predictions_all$observed$gee_scientists,
    "Trust in Scientists",
    "Observed"
  ),
  scientists_imputed = extract_key_values(
    predictions_all$imputed$gee_scientists,
    "Trust in Scientists",
    "Imputed"
  )
)

cat("  ✓ Key estimates extracted\n")

# ========================================================================
# CREATE RESULTS SUMMARY
# ========================================================================
cat("\nCreating results summary...\n")

# create summary dataframe
results_df <- bind_rows(lapply(results_list, as.data.frame))

# key findings for manuscript
key_findings <- list(
  # sample
  n_total = data_summary$n_total,
  n_observed = data_summary$n_observed,
  n_imputations = data_summary$n_imputations,
  
  # science - imputed
  science_baseline = results_list$science_imputed$baseline,
  science_baseline_ci = results_list$science_imputed$baseline_ci,
  science_covid = results_list$science_imputed$covid_peak,
  science_covid_ci = results_list$science_imputed$covid_peak_ci,
  science_final = results_list$science_imputed$final,
  science_final_ci = results_list$science_imputed$final_ci,
  science_rise = results_list$science_imputed$change_to_peak,
  science_fall = results_list$science_imputed$change_from_peak,
  science_overall = results_list$science_imputed$change_overall,
  
  # scientists - imputed
  scientists_baseline = results_list$scientists_imputed$baseline,
  scientists_baseline_ci = results_list$scientists_imputed$baseline_ci,
  scientists_covid = results_list$scientists_imputed$covid_peak,
  scientists_covid_ci = results_list$scientists_imputed$covid_peak_ci,
  scientists_final = results_list$scientists_imputed$final,
  scientists_final_ci = results_list$scientists_imputed$final_ci,
  scientists_rise = results_list$scientists_imputed$change_to_peak,
  scientists_fall = results_list$scientists_imputed$change_from_peak,
  scientists_overall = results_list$scientists_imputed$change_overall,
  
  # bias comparison
  science_bias = round(
    results_list$science_observed$change_overall - 
    results_list$science_imputed$change_overall, 3
  ),
  scientists_bias = round(
    results_list$scientists_observed$change_overall -
    results_list$scientists_imputed$change_overall, 3
  )
)

cat("  ✓ Results summary created\n")

# ========================================================================
# SAVE RESULTS
# ========================================================================
cat("\nSaving results...\n")

# save as RDS
saveRDS(key_findings, ("results/key_findings.rds"))
saveRDS(results_df, ("results/results_summary.rds"))

# save as JSON for easy access
jsonlite::write_json(
  key_findings,
  ("results/key_findings.json"),
  pretty = TRUE
)

# save as CSV
write.csv(
  results_df,
  ("results/results_summary.csv"),
  row.names = FALSE
)

cat("  ✓ Results saved\n")

# ========================================================================
# GENERATE TEXT FOR MANUSCRIPT
# ========================================================================
cat("\nGenerating text snippets for manuscript...\n")

# create text snippets
text_snippets <- list(
  # abstract
  abstract_n = glue("N = {format(key_findings$n_total, big.mark = ',')}"),
  
  # methods
  methods_sample = glue(
    "We analysed data from {format(key_findings$n_total, big.mark = ',')} ",
    "participants in the New Zealand Attitudes and Values Study (NZAVS) ",
    "who were present at baseline (2019-2020)."
  ),
  
  methods_missing = glue(
    "Complete case analysis included {format(key_findings$n_observed, big.mark = ',')} ",
    "observations. We used multiple imputation with {key_findings$n_imputations} ",
    "imputations to address missing data."
  ),
  
  # results - science
  results_science = glue(
    "After correcting for attrition bias, the social value of science ",
    "increased from {key_findings$science_baseline} {key_findings$science_baseline_ci} ",
    "at baseline to {key_findings$science_covid} {key_findings$science_covid_ci} ",
    "during the COVID-19 response (an increase of {key_findings$science_rise} points). ",
    "However, by 2022-2024, it had declined to {key_findings$science_final} ",
    "{key_findings$science_final_ci}, representing an overall change of ",
    "{key_findings$science_overall} points from baseline."
  ),
  
  # results - scientists
  results_scientists = glue(
    "Similarly, trust in scientists rose from {key_findings$scientists_baseline} ",
    "{key_findings$scientists_baseline_ci} to {key_findings$scientists_covid} ",
    "{key_findings$scientists_covid_ci} during the pandemic response ",
    "(+{key_findings$scientists_rise} points), before declining to ",
    "{key_findings$scientists_final} {key_findings$scientists_final_ci} ",
    "by the study's end, yielding an overall change of ",
    "{key_findings$scientists_overall} points."
  ),
  
  # results - bias
  results_bias = glue(
    "Complete case analysis masked these declines. Without correcting for attrition, ",
    "the apparent changes were {key_findings$science_bias} points more positive ",
    "for science values and {key_findings$scientists_bias} points more positive ",
    "for trust in scientists."
  )
)

# save text snippets
writeLines(
  unlist(text_snippets),
  ("results/manuscript_text_snippets.txt")
)

cat("  ✓ Text snippets generated\n")

# ========================================================================
# PRINT SUMMARY
# ========================================================================
cat("\n==========================================================================\n")
cat("KEY FINDINGS SUMMARY\n")
cat("==========================================================================\n")

cat("\nSAMPLE:\n")
cat("  Total N:", format(key_findings$n_total, big.mark = ","), "\n")
cat("  Complete cases:", format(key_findings$n_observed, big.mark = ","), "\n")
cat("  Imputations:", key_findings$n_imputations, "\n")

cat("\nSOCIAL VALUE OF SCIENCE (1-7 scale):\n")
cat("  Baseline (2019-20):", key_findings$science_baseline, key_findings$science_baseline_ci, "\n")
cat("  COVID peak (2020-21):", key_findings$science_covid, key_findings$science_covid_ci, "\n")
cat("  Final (2022-24):", key_findings$science_final, key_findings$science_final_ci, "\n")
cat("  Overall change:", key_findings$science_overall, "points\n")

cat("\nTRUST IN SCIENTISTS (1-7 scale):\n")
cat("  Baseline (2019-20):", key_findings$scientists_baseline, key_findings$scientists_baseline_ci, "\n")
cat("  COVID peak (2020-21):", key_findings$scientists_covid, key_findings$scientists_covid_ci, "\n")
cat("  Final (2022-24):", key_findings$scientists_final, key_findings$scientists_final_ci, "\n")
cat("  Overall change:", key_findings$scientists_overall, "points\n")

cat("\nATTRITION BIAS:\n")
cat("  Science (observed - imputed):", key_findings$science_bias, "points\n")
cat("  Scientists (observed - imputed):", key_findings$scientists_bias, "points\n")

cat("\n==========================================================================\n")
cat("Results saved to: results/\n")
cat("\nKey files:\n")
cat("  - key_findings.json: All key values in JSON format\n")
cat("  - results_summary.csv: Detailed results table\n")
cat("  - manuscript_text_snippets.txt: Pre-written text for manuscript\n")
cat("\nNext step: Run 06_generate_report.R\n")