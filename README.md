# Simulation study: attrition bias in longitudinal trust surveys

Companion code for **"Response Bias Masks the Erosion of Trust in Science Over Time"** (Bulbulia, Kerr, Houkamau, Osborne, Wilson, Yogeeswaran, & Sibley).

This repository contains the simulation study reported in Supplement S5 of the paper. The simulation evaluates correction methods for attrition bias in panel surveys of institutional trust under Missing at Random (MAR) and Missing Not at Random (MNAR) conditions.

## overview

A single canonical script generates all simulation results:

```r
source("code/simulate_attrition_methods.R")
```

The simulation creates a synthetic cohort of 40,000 participants over five waves with differential attrition by trust group, then compares four methods for recovering the true (oracle) population trajectory:

- **complete case analysis** (observed only)
- **inverse probability of censoring weighting** (IPCW)
- **multiple imputation** (Amelia, designed for panel data)
- **oracle** (ground truth, no missingness)

## repository structure

```
code/
  simulate_attrition_methods.R   # canonical simulation script
  functions/                     # helper utilities
results/
  figures/                       # simulation comparison plots (PNG)
  objects/                       # saved simulation outputs (RDS)
documentation/
  analysis_report.qmd            # supplementary report template
setup.R                          # plotting functions for the report
```

## requirements

R (>= 4.2) with the following packages: `Amelia`, `geepack`, `MASS`, `ggeffects`, `tidyverse`, `patchwork`, `sandwich`.

## citation

If you use this code, please cite the accompanying paper.

## licence

Code in this repository is released under the MIT licence.
