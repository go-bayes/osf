# Trust in Science: Sensitivity Analysis of Imputation Methods

This repository contains code for evaluating different missing data imputation methods in the context of trust in science longitudinal data. It extends the original synthetic data generation to comprehensively compare imputation approaches.

## Important Note on Data Access

The original data from the New Zealand Attitudes and Values Study (NZAVS) contains sensitive personal information and cannot be publicly shared. This repository provides:

- Complete analysis code used in the study
- Synthetic example data that mirrors the structure of the real data
- Pre-computed model outputs and predictions
- Detailed documentation of all methods
- Instructions for requesting access to the real data

## Repository Structure

```
osf_reproducible_analysis/
├── code/               # All analysis scripts
├── data/               # Synthetic data and templates
├── results/            # Model outputs and figures
├── documentation/      # Methods and technical notes
└── references/         # Bibliography and citations
```

## Quick Start

1. **Setup Environment**
   ```r
   source("code/00_setup_environment.R")
   ```

2. **Run Demo with Synthetic Data**
   ```r
   source("code/demo/run_synthetic_demo.R")
   ```

3. **View Pre-computed Results**
   - See `results/figures/` for all plots
   - See `results/tables/` for summary statistics
   - See `results/model_outputs/` for saved model objects

## For Researchers with Data Access

If you have access to the NZAVS data:

1. Place your data files in the location specified in `code/config/config.R`
2. Set `USE_REAL_DATA <- TRUE` in the config file
3. Run the numbered scripts in `code/` sequentially

## Key Findings from Method Comparison

- **Complete Case Analysis**: Severely biased due to differential attrition
- **MICE PMM (wide format)**: Near-perfect recovery of true trends (100% bias reduction)
- **Amelia**: Moderate recovery (~54% bias reduction) due to assumed common trends
- **IPCW**: Good for homogeneous populations, may struggle with heterogeneous trajectories
- **Critical insight**: Data format matters - MICE requires wide format for longitudinal data

## Statistical Methods

- **Continuous outcomes**: Generalized Estimating Equations (GEE) with natural splines
- **Categorical outcomes**: Proportional odds models (MASS::polr) with cluster-robust SEs
- **Missing data**: Multiple imputation by chained equations (10 imputations)
- **Weights**: Post-stratification weights based on NZ Census

See `documentation/METHODS.md` for complete details.

## Software Requirements

- R version 4.3.0 or higher
- Key packages: margot, mice, geepack, MASS, ggeffects, tidyverse
- Full package list in `code/00_setup_environment.R`

## Citation

If you use this code, please cite:

[Citation details to be added]

## Data Access

For information about accessing the original NZAVS data, see `documentation/DATA_ACCESS.md`

## Contact

For questions about this analysis:
- Joseph A. Bulbulia (joseph.bulbulia@vuw.ac.nz)

For questions about the NZAVS:
- Chris G. Sibley (c.sibley@auckland.ac.nz)