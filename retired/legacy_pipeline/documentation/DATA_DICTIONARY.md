# Data Dictionary

## Variables in Analysis Dataset

### Identifiers
- **id**: Unique participant identifier
- **wave**: Study wave (11-14)
- **years**: Time in years since baseline (0-3)

### Outcome Variables
- **trust_science**: "I have a high degree of confidence in the scientific community" (1-7)
  - 1 = Strongly Disagree
  - 7 = Strongly Agree
  
- **trust_scientists**: "Our society places too much emphasis on science" (1-7, reverse-coded)
  - 1 = Strongly Disagree (i.e., high trust)
  - 7 = Strongly Agree (i.e., low trust)
  
- **trust_science_factor**: Categorized trust in science
  - "low" = 1-3
  - "med" = 4-5
  - "high" = 6-7
  
- **trust_scientists_factor**: Categorized trust in scientists
  - "low" = 1-3
  - "med" = 4-5
  - "high" = 6-7

### Other Variables
- **weights**: Post-stratification weights for population representation

## Missing Data Patterns
Missing data increased over waves due to participant attrition:
- Wave 11 (Baseline): ~5% missing
- Wave 12: ~15% missing
- Wave 13: ~25% missing
- Wave 14: ~35% missing

## Synthetic Data Note
The synthetic data preserves:
- Overall distributions of trust variables
- Correlation structure between variables
- Missing data patterns
- Temporal trends

But does not preserve:
- Individual-level trajectories
- Demographic associations
- Exact parameter values

