# Workflow Summary: Trust in Science Analysis

## Completed Tasks

### 1. Data Generation and Preparation ✓
- Generated synthetic data with 40,000 participants and 5 waves (2019-2023)
- Used three-group simulation design (low/medium/high trust based on education)
- **NEW**: Implemented correlated baseline trust scores (bivariate normal, r≈0.6 within groups)
- **NEW**: Linear time trends instead of piecewise adjustments (low: -0.3/year, medium: 0, high: +0.2/year)
- **NEW**: Reduced measurement noise (σ=0.10) for clearer signal
- **NEW**: Dropout depends on current trust values via logistic model
- Baseline sampling reflects NZAVS oversampling (62% women, 17% Māori, older age distribution)
- Post-stratification weights correct for sampling design (fixed at baseline)
- Applied multiple imputation using Amelia → MICE conversion with improved parameters:
  - Removed empri parameter
  - Added splinetime=3, polytime=3
  - Fixed education inclusion in imputation model
- Files created:
  - `data/synthetic/synthetic_trust_data.rds`
  - `data/synthetic/oracle_trust_data.rds`
  - `data/processed/mids_obj.rds`
  - `data/processed/dat_observed.rds`

### 2. Statistical Models ✓
- Fitted GEE models for continuous outcomes (trust scales 1-7)
- Fitted proportional odds models for categorical outcomes (low/med/high)
- Models fitted for both observed and imputed data
- Files created:
  - `results/model_outputs/models_observed.rds`
  - `results/model_outputs/predictions_all.rds`
  - `results/model_outputs/model_summaries.rds`

### 3. Visualizations ✓
- Created plots using ggeffects plot() function
- Generated individual and combined plots for:
  - Continuous outcomes (GEE models)
  - Categorical outcomes (POLR models)
- Created main figure combining continuous outcomes
- Files created (14 figures total):
  - Individual plots for each outcome/data type
  - Combined continuous and categorical plots
  - Main figure in PNG and PDF formats

### 4. Tables ✓
- Generated comprehensive tables including:
  - Table 1: Wave summary
  - Table 2: Missing data patterns
  - Table 3: Marginal mean estimates
  - Table 4: Model coefficients
  - Table S1: Categorical probabilities
- Files created:
  - HTML tables for manuscript
  - CSV files for further analysis

## Key Findings

Based on the three-group synthetic data analysis:

1. **Selection Bias Pattern**
   - Oracle data (ground truth): Shows overall decline in trust
   - Observed data (survivors): Shows artificial stability/increase
   - Low-trust individuals (education 1-2) experience strong COVID dropout
   - High-trust individuals (education 6-7) remain in study

2. **Trust Trajectories by Group**
   - High trust group: Gradual increase over time
   - Medium trust group: Relatively stable
   - Low trust group: Sharp decline, especially post-COVID
   - Selective attrition masks population-level decline

3. **Missing Data Impact**
   - ~42.5% missing data by end of study (Year 4/2023)
   - Differential attrition by trust level:
     - Low trust group: ~57% missing by Year 4
     - Medium trust group: ~41% missing by Year 4  
     - High trust group: ~33% missing by Year 4
   - COVID wave (Year 1) shows strongest selection: 25% extra dropout for low trust
   - Multiple imputation partially recovers true population trends

## File Structure

```
osf/
├── code/
│   ├── 00_setup_environment.R
│   ├── 01_load_prepare_data.R
│   ├── 02_fit_models.R
│   ├── 03_create_visualizations.R
│   ├── 04_generate_tables.R
│   └── config/config.R
├── data/
│   ├── synthetic/
│   │   ├── generate_synthetic_data.R
│   │   ├── synthetic_trust_data.rds
│   │   └── oracle_trust_data.rds
│   └── processed/
│       ├── mids_obj.rds
│       ├── dat_observed.rds
│       └── data_summary.rds
└── results/
    ├── figures/ (14 files)
    ├── tables/ (8 files)
    └── model_outputs/ (3 files)
```

## Notes

- The analysis uses synthetic data that mimics the structure of the real NZAVS data
- Three-group simulation design creates clear demonstration of selection bias
- Baseline sampling and weights reflect actual NZAVS survey design
- Weights are fixed at baseline (represent sampling probability, not response probability)
- Trust categories (low/med/high) created after imputation from continuous measures
- The visualization script uses ggeffects plot() function directly
- All scripts run successfully with 5 waves of data (2019-2023)
