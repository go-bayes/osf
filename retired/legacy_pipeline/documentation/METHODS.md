
## Statistical Methods

### Data and Sample
We analyzed longitudinal data from the New Zealand Attitudes and Values Study (NZAVS), 
a national probability sample of adults. Our analysis included N = 42,681 participants 
who were present at baseline (Wave 11, 2019-2020) and tracked through Wave 14 (2022-2024).

### Measures
1. **Social Value of Science**: Single item rated 1-7
2. **Trust in Scientists**: Single item rated 1-7

Both measures were analyzed as:
- Continuous outcomes (1-7 scale)
- Categorical outcomes (Low: 1-3, Medium: 4-5, High: 6-7)

### Missing Data
We addressed missing data using multiple imputation with the Amelia II algorithm:
- 10 imputations
- Bounds specified for ordinal variables (1-7)
- Missing data patterns showed increasing attrition over time
- Complete case analysis was conducted for comparison

### Statistical Models

#### Continuous Outcomes
Generalized Estimating Equations (GEE) with:
- Natural cubic splines (3 df) for time trends
- Exchangeable correlation structure
- Post-stratification weights
- Cluster-robust standard errors

#### Categorical Outcomes  
Proportional odds models (MASS::polr) with:
- Natural cubic splines (3 df) for time trends
- Post-stratification weights
- Cluster-robust standard errors (sandwich estimator)

### Software
All analyses were conducted in R version 4.3+ using:
- mice (v3.16+) for multiple imputation handling
- geepack (v1.3+) for GEE models
- MASS (v7.3+) for proportional odds models
- ggeffects (v1.5+) for marginal predictions

