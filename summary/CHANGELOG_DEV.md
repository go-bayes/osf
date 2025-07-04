# CHANGELOG_DEV.md

## 2025-01-04 - Major Improvements to Synthetic Data and Imputation

### Changed
- **Synthetic Data Generation**:
  - Implemented correlated baseline trust scores using bivariate normal distribution (r≈0.6)
  - Changed from piecewise year adjustments to linear time trends:
    - Low trust group: -0.3 per year
    - Medium trust group: 0.0 per year (stable)
    - High trust group: +0.2 per year
  - Reduced measurement noise from σ=0.15 to σ=0.10 for clearer education signal
  - Dropout now depends on current trust values via logistic model:
    ```r
    Logit(dropout) = -2.0 + 0.4*(4 - trust_science) + 0.15*years
    ```

- **Amelia Imputation**:
  - Removed empri parameter that was causing issues
  - Added splinetime=3 and polytime=3 for better time series modeling
  - Fixed education being incorrectly included in id_vars
  - Now properly excludes baseline trust variables from imputation
  - Excludes simulation artifacts (trust_group, drop_prob, dropped) from imputation

### Fixed
- Collinearity warning between trust_science and trust_scientists variables
- MASS::select namespace conflict by using dplyr::select explicitly
- Apply error in pool_predictions function by using stats::var explicitly
- Education not being used in imputation model (was incorrectly in id_vars)
- Imputation showing wrong direction (now tracks oracle data well)

### Added
- test_3_improved.R implementing all improvements
- Comprehensive documentation updates:
  - workflow_summary.md with new simulation details
  - osf_package_summary.md with imputation improvements
  - data/synthetic/README.md with new data generation approach
  - planning/PLANNING.md tracking all progress
  - summary/CHANGELOG_DEV.md (this file)

### Technical Details
- Correlated baselines use MASS::mvrnorm with appropriate covariance matrix
- Linear slopes provide interpretable year-on-year changes
- Current-value dropout creates realistic selection patterns
- Reduced noise strengthens education signal for imputation
- Proper variable exclusion prevents imputation artifacts

### Results
- Oracle data shows clear population decline in trust
- Observed data shows artificial stability due to selective dropout
- Imputation now successfully recovers trend closer to oracle
- Selection bias mechanism clearly demonstrated