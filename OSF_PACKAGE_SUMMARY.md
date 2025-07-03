# OSF Reproducible Analysis Package Summary

## Overview
This package contains all materials needed to reproduce the analysis from:
**"Response Bias Masks the Erosion of Trust in Science Over Time"**

## Package Contents

### 📁 code/
- **00_setup_environment.R** - Install/load packages and set up environment
- **01_load_prepare_data.R** - Load data (real or synthetic) and prepare for analysis
- **02_fit_models.R** - Fit all statistical models (GEE and proportional odds)
- **03_create_visualizations.R** - Generate all figures
- **04_generate_tables.R** - Create all tables
- **05_extract_results.R** - Extract key findings and create summaries
- **06_generate_report.R** - Generate comprehensive report

#### 📁 code/config/
- **config.R** - Centralized configuration settings

#### 📁 code/functions/
- **margot_amelia_to_mice_fixed.R** - Convert Amelia to MICE format
- **fix_mids_factors.R** - Helper for factor variables in MICE

#### 📁 code/demo/
- **run_demo.R** - Quick demonstration with synthetic data

### 📁 data/
#### 📁 data/synthetic/
- **generate_synthetic_data.R** - Create synthetic data matching real data structure
- **synthetic_trust_data.csv** - Pre-generated synthetic dataset

### 📁 documentation/
- **README.md** - Main documentation
- **METHODS.md** - Detailed statistical methods
- **RESULTS.md** - Key findings summary
- **DATA_DICTIONARY.md** - Variable descriptions
- **analysis_report.Rmd** - Full report template

### 📁 results/
(Created when analysis is run)
- **figures/** - All plots and visualizations
- **tables/** - All tables in HTML and CSV formats
- **model_outputs/** - Saved model objects
- **key_findings.json** - Main results in JSON format

## Quick Start

### Option 1: Run Full Analysis
```r
# Set working directory to package root
setwd("path/to/osf_reproducible_analysis")

# Run all scripts in order
source("code/00_setup_environment.R")
source("code/01_load_prepare_data.R")  
source("code/02_fit_models.R")
source("code/03_create_visualizations.R")
source("code/04_generate_tables.R")
source("code/05_extract_results.R")
source("code/06_generate_report.R")
```

### Option 2: Quick Demo
```r
source("code/demo/run_demo.R")
```

## Key Settings (in code/config/config.R)

- `USE_REAL_DATA`: FALSE (uses synthetic data by default)
- `N_IMPUTATIONS`: 10 (reduce to 2 for quick demo)
- `SEED`: 123 (for reproducibility)

## Data Privacy Note

The original NZAVS data cannot be shared due to privacy constraints. This package includes:
1. **Synthetic data generator** that creates realistic data matching the structure of the original
2. **Pre-generated synthetic dataset** for immediate use
3. **Configuration to use real data** if you have access

## Expected Runtime

- Full analysis with 10 imputations: ~10-15 minutes
- Demo with 2 imputations: ~2-3 minutes

## System Requirements

- R version 4.0 or higher
- ~2GB RAM
- Required packages installed automatically by setup script

## Contact

For questions about this reproducible analysis package:
- joseph.bulbulia@gmail.com

For access to original NZAVS data:
- Visit: www.nzavs.auckland.ac.nz

## Citation

If using this code or synthetic data, please cite:
```
Kerr, J., Sibley, C. G., Houkamau, C., Osborne, D., Wilson, M., 
Yogeswaran, K., & Bulbulia, J. A. (2025). Response bias masks 
the erosion of trust in science over time. [Journal Name].
```