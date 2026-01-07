# Synthetic Data Documentation

This folder contains synthetic data generation scripts and outputs that demonstrate the analysis while protecting participant privacy.

## Files

- `generate_synthetic_data.R` - Script to generate synthetic longitudinal data
- `synthetic_trust_data.rds` - Generated synthetic dataset (created on first run)
- `oracle_trust_data.rds` - Complete data before missingness (for validation)

## Synthetic Data Features

### Variable Definitions

| Variable | Description | Scale/Values |
|----------|-------------|-------------|
| id | Participant identifier | Integer |
| wave | Survey wave (0-4) | 0 = 2019, 1 = 2020, 2 = 2021, 3 = 2022, 4 = 2023 |
| years | Years since baseline | 0-4 |
| trust_science | "I have a high degree of confidence in the scientific community" | 1 (Strongly Disagree) - 7 (Strongly Agree) |
| trust_scientists | "Our society places too much emphasis on science" (reverse coded) | 1 (Strongly Disagree) - 7 (Strongly Agree) |
| age_baseline | Age at first measurement | Years (mean ~50) |
| gender | Gender | Female (62%) / Male (38%) |
| ethnicity | Ethnic group | NZ European (68%) / Maori (17%) / Asian (10%) / Pacific (6%) / Other (4%) |
| education | Education level | 1 (No high school) - 7 (Postgraduate degree) |
| weights | Post-stratification weights (fixed at baseline) | Numeric (mean = 1.0) |

### Statistical Properties

The synthetic data uses a three-group simulation design with improved features:

#### Baseline Distribution
- **Trust in Science**: Generated from demographic predictors + bivariate normal distribution (r≈0.7)
  - Base formula: 4.0 + 0.3×education + 0.01×(age-50) + 0.2×female + ethnicity effects
  - Education effect: +0.3 per level (strongest predictor)
  - Age effect: +0.01 per year above 50
  - Gender effect: Females +0.2 higher trust
  - Ethnicity effects: Māori -0.3, Pacific -0.2, Asian +0.1
- **Trust in Scientists**: Same predictors, slightly lower intercept (3.8)

#### Group Trajectories (Linear Slopes)
- **High trust group** (education 6-7): +0.2 per year
- **Medium trust group** (education 3-5): 0.0 per year (stable)
- **Low trust group** (education 1-2): -0.3 per year
- **Measurement noise**: σ=0.10 (reduced for clearer education signal)

#### Sampling Design
- **Oversampled**: Women (62%), Māori (17%), older adults (mean age 50)
- **Undersampled**: Men, Pacific/Asian populations, younger adults
- **Post-stratification weights** correct for this design:
  - Women: 0.90 (oversampled, need less weight)
  - Men: 1.10 (undersampled, need more weight)
  - Māori: 0.95 (slightly oversampled)
  - Pacific: 1.20, Asian: 1.15 (undersampled)
  - Age 55+: 0.85 (oversampled)
  - Age <35: 1.15 (undersampled)

### Missing Data Patterns

Realistic survey dropout patterns with improved mechanism:
- **Monotone dropout**: Once missing, always missing
- **Complete wave missingness**: When someone drops out, all their responses for that wave are missing
- **Baseline characteristics remain known**: Age, gender, ethnicity, education stay in database
- **Weights remain constant**: Fixed at baseline (not time-varying)

#### Current-Value-Dependent Dropout
Dropout probability now depends on **current trust values**, not just baseline group:
```
Logit(dropout) = -2.0 + 0.4*(4 - trust_science) + 0.15*years
```
This creates realistic patterns where:
- People with lower current trust are more likely to drop out
- Dropout increases over time for everyone
- Creates selection bias that masks population decline

#### Differential Attrition Outcomes
With this mechanism, we observe:
- **Low trust group**: ~43% dropout by Year 4
- **Medium trust group**: ~42% dropout by Year 4
- **High trust group**: ~42% dropout by Year 4

While overall dropout rates are similar, the **composition** changes dramatically - low-trust individuals drop out preferentially, creating the illusion of stability/increase in observed data.

### Oracle Data

The `oracle_trust_data.rds` file contains the complete synthetic data before applying missingness. This allows researchers to:
- Compare true population trends vs. observed (biased) trends
- Validate that imputation recovers patterns closer to the truth
- Understand the impact of selective attrition

## Usage

Generate new synthetic data:
```r
source("generate_synthetic_data.R")
synthetic_data <- generate_synthetic_trust_data(
  n_participants = 40000,  # large sample for clear patterns
  n_waves = 5,             # 2019-2023
  baseline_year = 2019,
  seed = 2025
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
- Post-stratification weights fixed at baseline (represent sampling probability)
- Trust categories (low/med/high) created after data generation/imputation
- Baseline sampling reflects actual NZAVS design (not population representative)
- Weights correct for intentional oversampling in survey design
