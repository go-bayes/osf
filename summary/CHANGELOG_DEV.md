# CHANGELOG_DEV.md

## 2026-01-05 - Repository Tidy and Canonical Script Relocation

### Changed
- Moved the canonical simulation script to `code/simulate_attrition_methods.R` and removed the obsolete `keep/` directory.
- Archived legacy pipeline materials under `retired/legacy_pipeline` while preserving helper functions in `code/functions`.
- Updated repository docs to reflect the new minimal structure and entrypoint.

## 2026-01-05 - Canonical Simulation Refresh, IPCW Comparison, and Pooled Ordinal Results

### Added
- **IPCW integration in `code/simulate_attrition_methods.R`** to compare attrition correction under the current-trust dropout mechanism.
- **Pooled ordinal thresholds** for Amelia and MICE, with threshold summaries saved alongside outputs.
- **RDS output bundle** (`results/objects/test_3_improved_outputs.rds`) for reproducible downstream use.

### Changed
- **Canonical simulation** now anchored to `code/simulate_attrition_methods.R` with IPCW, imputation, and categorical comparisons.
- **MAR and MNAR scenarios** now simulate attrition using baseline and lagged trust, with calibrated intercepts to preserve group retention targets.
- **Category cutpoints aligned** to Low ≤3, Medium 4–5, High ≥6 across active scripts.
- **Simulation outputs and figures** now use `here::here()` for file paths.
- **Synthetic data storage** now defaults to `.rds` files with updated loaders.
- **Dropout calibration** now targets group retention with overall attrition tuning.
- **Medium trust stability** enforced by removing medium-group measurement noise.
- **MICE comparison** now uses a real wide-format imputation instead of Amelia-converted mids objects.
- **Observed-by-group diagnostics** now compute proportions correctly within each group-year.
- **IPCW plotting** now aligns baseline to the oracle mean for direct comparison.
- **Simplified baseline trust generation** with group-based ranges to ensure low and high categories exist.
- **Dropout mechanism simplified** to per-wave group-specific hazards matching final retention targets.
- **Baseline measurement noise removed** so all methods align at year 0 without post-hoc adjustment.
- **IPCW model aligned** to baseline trust group and time only, consistent with the dropout mechanism.
- **GEE trajectories** now use `factor(years)` to match per-wave means and avoid spline artefacts.
- **Post-stratification weights fixed at 1** to isolate attrition effects in the simplified run.
- **Continuous recovery** now reports per-year weighted means for Oracle, Observed, IPCW, Amelia, and MICE, with a dedicated mean-comparison plot.
- **Category proportion plot** now facets by trust category for clearer method comparison.
- **Observed-by-group diagnostics** now exclude missing categories so proportions are conditional on observation.
- **Baseline trust group mix** now set to high-majority (high 0.60, medium 0.30, low 0.10).
- **Amelia imputation** now uses baseline trust predictors, ordinal handling for trust items, and lag terms to better mirror the real analysis.
- **Simulated trust outcomes** now use integer 1-7 values to mirror ordinal survey responses.
- **IPCW dropout model** now uses baseline trust, lagged trust, and demographics rather than trust group only.
- **MICE wide-format imputation** now includes baseline trust predictors for closer alignment with the MAR mechanism.
- **Scenario-labelled outputs** now save separate figures and `.rds` outputs for MAR and MNAR runs.
- **High trust trajectory** now fixed to zero change for a simpler benchmark.
- **Calibration diagnostics** now report per-wave dropout rates against targets and final retention by group.
- **Scenario summary** now prints mean trajectories across MAR and MNAR at the end of the run.
- **MICE model** now uses a long-format imputation with baseline and lagged trust predictors to mirror the Amelia setup.
- **Trust in scientists** now treated as an auxiliary variable only, and excluded from simulation outputs.
- **Bias strength** now set to milder retention targets (high 0.70, medium 0.45, low 0.25) with softer attrition coefficients.
- **Low and medium trust trajectories** now decline over time to increase low-trust prevalence in the oracle data.
- **Continuous comparison plots** now use per-year mean lines with raw points so plotted trajectories match reported means.

### Deprecated
- `_delete_me.R` is now archived in `retired/legacy_pipeline` in favour of the canonical script.

## 2025-01-07 - IPCW Ordinal Analysis and Visualization Scripts

### Added
- **Extended `test_2_with_ipcw.R`** with comprehensive ordinal model analysis:
  - Category proportion tracking (Low ≤3, Medium ≤5, High >5)
  - POLR model fitting and threshold comparison
  - Baseline distribution preservation metrics (KL divergence)
  - Enhanced visualizations for categorical outcomes
  - Comprehensive performance summary across all metrics

- **Created `analyze_ipcw_results.R`** for publication-ready outputs:
  - Continuous outcome recovery plots with confidence bands
  - Categorical distribution faceted plots
  - Performance summary tables using gt package
  - Composite visualizations with patchwork
  - Automated summary report generation

- **Created `generate_ipcw_diagnostics.R`** for method diagnostics:
  - Attrition pattern analysis by trust group
  - IPCW weight distribution and extremes
  - Baseline covariate balance assessment
  - Ordinal model threshold comparisons
  - Correlation analysis between methods

### Key Findings
- **Amelia performs best overall** with superior time-series modeling
- **MICE may introduce baseline distribution shifts** (explains Year 0 discrepancy)
- **IPCW provides partial correction** but struggles with complex patterns
- **Complete case analysis severely biased** due to differential attrition

### Documentation
- Created `ORDINAL_MODEL_ENHANCEMENTS.md` documenting all changes
- Scripts save outputs to `results/figures/` and `results/tables/`
- Automated summary reports for quick interpretation

## 2025-01-06 - Major Repository Cleanup and Enhancement

### Repository Reorganization
- Created `keep/` directory for important scripts
  - Moved `test_2_with_ipcw.R` (primary comparison with IPCW)
  - Moved `test_3_improved.R` (now `code/simulate_attrition_methods.R`) for comprehensive method comparison
  - Moved `method_comparison_summary.R` (useful summary)
- Created `retired/` directory for old test scripts
- Removed all data files (.csv, .rds, .qs)
- Updated .gitignore to exclude data files while keeping synthetic examples

### Enhanced Method Comparisons
- Created `compare_imputation_methods_enhanced.R` with:
  - Reduced imputation count (m=5) for efficiency
  - Categorical outcome analysis (Low/Medium/High proportions)
  - Ordinal model comparison using MASS::polr
  - Comprehensive performance metrics
  
### Key Enhancements
- **Categorical Analysis**: Track proportion shifts in trust categories
- **Ordinal Models**: Compare threshold recovery across methods
- **Streamlined Workflow**: Focus on 5 methods (Oracle, Complete Case, MICE, Amelia, IPCW)
- **Enhanced Metrics**: MAE, bias, category shift detection, threshold recovery

### Documentation Updates
- Updated README.md to reflect new focus on method sensitivity analysis
- Created METHODS_COMPARISON.md with detailed guidance
- Fixed all dplyr::select() namespace issues

### Repository Purpose Change
- From: "Response Bias Masks the Erosion of Trust in Science"
- To: "Sensitivity Analysis of Imputation Methods for Trust in Science Data"
- Now focuses on comprehensive method evaluation beyond synthetic data

## 2025-01-04 - Major Refactoring: Amelia to MICE PMM

### Epic Models Refactoring Complete

Successfully refactored trust in science analysis from Amelia to MICE PMM with AR(1) correlation structure. Created new analysis pipeline in `/Users/joseph/GIT/epic-models/2025/25-refactor-trust-science/`.

#### Key Changes:
1. **Imputation Method**: Switched from Amelia (time series) to MICE PMM (chained equations)
2. **Data Format**: Converted from long to wide format for proper temporal dependencies
3. **Correlation Structure**: All GEE models now use `corstr = "ar1"`
4. **Bounding**: Implemented post-imputation bounding to constrain trust values to [1,7]
5. **Forward Filling**: Time-invariant demographics forward-filled to reduce missingness

#### Files Created:
- `00-simulate-trust-data.R`: Synthetic data generation with selection bias
- `01-helper-functions-mice.R`: Core MICE utilities including pooling functions
- `02-trust-science-growth-MICE.R`: Main imputation script (20 imputations)
- `03-trust-science-analysis-MICE.R`: Pooled GEE analysis with AR(1)
- `04-trust-science-consistent-MICE.R`: Comprehensive sensitivity analyses
- `README.md`: Complete documentation

#### Technical Implementation:
- **Baseline Cohort Selection**: Retained original approach (year_measured == 1 & wave == 2019)
- **Temporal Predictor Matrix**: Future values cannot predict past values
- **Natural Splines**: Non-linear year effects with df=3
- **Pooling**: Rubin's rules for combining estimates across imputations

## 2025-01-04 - Major Improvements to Synthetic Data and Imputation

### Latest Updates

#### Comprehensive Imputation Method Comparison
- Tested all major imputation methods with proper wide format for longitudinal data
- Results with 5,000 participants show remarkable performance:
  - **Oracle (truth)**: -0.053 change
  - **Observed (biased)**: +0.135 change (severe selection bias)
  - **MICE PMM (wide)**: -0.053 change (100.2% bias reduction!)
  - **MICE CART (wide)**: -0.052 change (99.5% bias reduction)
  - **miceRanger (wide)**: -0.048 change (97.5% bias reduction)
  - **Amelia**: +0.033 change (54.1% bias reduction)
- Timing: MICE PMM fastest (1.5s), miceRanger slowest (67s)
- Key finding: With proper wide format, MICE methods achieve near-perfect recovery

#### Wide vs Long Format - Critical Finding!
- **MICE requires wide format for longitudinal data** to properly model dependencies
- Comparison with 5,000 participants shows dramatic differences:
  - Oracle (truth): -0.053 change
  - Observed (biased): +0.135 change
  - **MICE Wide format**: -0.053 change (perfect recovery!)
  - MICE Long format: +0.035 change (poor recovery)
  - Amelia: +0.031 change (moderate recovery)
- Wide format allows MICE to see that trust_science_t+1 depends on trust_science_t
- Correlation between adjacent waves: 0.97-0.99
- **Key insight**: Data format matters critically for longitudinal imputation

#### Imputation Method Comparison
- Tested multiple imputation methods with demographic predictors:
  - **Amelia**: Spline-based time series imputation
  - **MICE PMM**: Predictive mean matching
  - **MICE CART**: Classification and regression trees
  - **MICE norm.predict**: Linear regression prediction
- Added COVID indicator (year ≥ 1) and time splines as auxiliary variables
- Results with 10,000 participants:
  - Oracle: -0.047 decline
  - Observed: +0.160 increase (biased)
  - MICE CART: +0.027 (best recovery)
  - Amelia: +0.044
  - MICE PMM: +0.049
  - MICE norm.predict: +0.050
- All methods partially recover true trend, with CART performing best

#### Demographics Predict Trust
- **Demographic Predictors**: Trust now depends on all demographics, not just education:
  ```r
  trust_science_mean = 4.0 + 
    0.3 * education +
    0.01 * (age_baseline - 50) +
    0.2 * (gender == "Female") +
    -0.3 * (ethnicity == "Maori") +
    -0.2 * (ethnicity == "Pacific") +
    0.1 * (ethnicity == "Asian")
  ```
- **Improved Imputation**: Demographics provide auxiliary information for recovering missing trust values
- **Results**: Imputation now shows smaller increase (+0.032) vs observed (+0.134), partially recovering oracle decline (-0.053)

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
- test_3_improved.R implementing all improvements (now `code/simulate_attrition_methods.R`)
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
