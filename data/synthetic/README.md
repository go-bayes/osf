# Synthetic Data Documentation

This folder contains synthetic data generation scripts and outputs that demonstrate the analysis while protecting participant privacy.

## Files

- `generate_synthetic_data.R` - Script to generate synthetic longitudinal data
- `synthetic_trust_data.csv` - Generated synthetic dataset (created on first run)
- `oracle_trust_data.csv` - Complete data before missingness (for validation)

## Synthetic Data Features

### Variable Definitions

| Variable | Description | Scale/Values |
|----------|-------------|-------------|
| id | Participant identifier | Integer |
| wave | Survey wave (0-3) | 0 = 2019, 1 = 2020, 2 = 2021, 3 = 2022 |
| years | Years since baseline | 0-3 |
| trust_science | "I have a high degree of confidence in the scientific community" | 1 (Strongly Disagree) - 7 (Strongly Agree) |
| trust_scientists | "Our society places too much emphasis on science" (reverse coded) | 1 (Strongly Disagree) - 7 (Strongly Agree) |
| age_baseline | Age at first measurement | Years |
| gender | Gender | Male/Female |
| ethnicity | Ethnic group | NZ European/Maori/Pacific/Asian/Other |
| education | Education level | 1 (No high school) - 7 (Postgraduate degree) |
| weights | Post-stratification weights | Numeric |

### Statistical Properties

The synthetic data preserves key statistical properties:
- **Correlated trust measures** (r ≈ 0.7 between trust_science and trust_scientists)
- **Education predicts baseline trust** (higher education → higher trust, β ≈ 0.3)
- **Temporal patterns** matching observed data:
  - COVID-19 bump in 2020 (wave 1)
  - Sustained elevation in 2021 (wave 2)
  - Decline below baseline in 2022 (wave 3)
- **Education moderates trajectories** (lower education → steeper declines)

### Missing Data Patterns

Selective attrition mechanisms:
- **Trust effects**: Lower trust → higher dropout probability
- **Education effects**: Lower education → higher dropout probability  
- **Age effects**: Younger participants → higher dropout
- **Time effects**: Increasing missingness over waves
- **Monotone dropout**: Once missing, always missing

#### COVID-Specific Attrition (Wave 1)

During the COVID-19 pandemic (2020, wave 1), selective attrition was particularly strong:
- **Low trust penalty**: Participants with trust < 3 had 70% higher dropout probability
- **Moderate trust penalty**: Participants with trust 3-4 had 50% higher dropout probability
- **High education bonus**: Participants with education ≥ 6 had 30% lower dropout probability

This creates a strong selection effect where the COVID bump appears larger in observed data because low-trust individuals disproportionately left the study.

### Oracle Data

The `oracle_trust_data.csv` file contains the complete synthetic data before applying missingness. This allows researchers to:
- Compare true population trends vs. observed (biased) trends
- Validate that imputation recovers patterns closer to the truth
- Understand the impact of selective attrition

## Usage

Generate new synthetic data:
```r
source("generate_synthetic_data.R")
synthetic_data <- generate_synthetic_trust_data(
  n_participants = 42681,  # match NZAVS sample size
  n_waves = 4,
  baseline_year = 2019,
  seed = 123
)
```

## Key Insights

The synthetic data demonstrates how:
1. **Observed data shows stable/increasing trust** due to selective retention of high-trust individuals
2. **True population trends show decline** after initial COVID bump
3. **Education helps imputation recover missing values** because it predicts both baseline trust and dropout
4. **Multiple imputation reveals hidden erosion** in trust that complete-case analysis misses

## Technical Notes

- All trust values bounded to 1-7 scale
- Post-stratification weights approximate NZ Census targets