# PLANNING.md

<!---->
<!-- ## Project Goal -->
<!-- Demonstrate how response bias masks the erosion of trust in science over time through a reproducible analysis workflow. -->
<!---->
<!-- ## Key Objectives -->
<!-- 1. Create synthetic data that mimics NZAVS structure while protecting privacy -->
<!-- 2. Show how selective attrition creates artificial stability/increases in observed data -->
<!-- 3. Demonstrate that multiple imputation partially recovers true population trends -->
<!-- 4. Provide fully reproducible analysis package for OSF -->
<!---->
<!-- ## Completed Tasks ✓ -->
<!---->
<!-- ### Phase 1: Initial Implementation ✓ -->
<!-- - [x] Created three-group synthetic data design (low/medium/high trust based on education) -->
<!-- - [x] Implemented realistic baseline sampling (oversample women, Māori, older adults) -->
<!-- - [x] Added post-stratification weights to correct for sampling design -->
<!-- - [x] Created monotone dropout patterns with differential attrition by trust level -->
<!-- - [x] Implemented Amelia imputation with MICE conversion -->
<!-- - [x] Built GEE models for continuous outcomes -->
<!-- - [x] Built proportional odds models for categorical outcomes -->
<!-- - [x] Created visualization pipeline using ggeffects -->
<!-- - [x] Generated comprehensive tables -->
<!---->
<!-- ### Phase 2: Debugging and Refinement ✓ -->
<!-- - [x] Fixed collinearity between trust_science and trust_scientists -->
<!-- - [x] Removed empri parameter from Amelia -->
<!-- - [x] Added splinetime=3, polytime=3 to Amelia -->
<!-- - [x] Fixed education inclusion in imputation model (removed from id_vars) -->
<!-- - [x] Excluded baseline trust variables from imputation -->
<!-- - [x] Fixed namespace conflicts (MASS::select vs dplyr::select) -->
<!-- - [x] Fixed apply error in pool_predictions function -->
<!---->
<!-- ### Phase 3: Improved Simulation Design ✓ -->
<!-- - [x] Implemented correlated baseline trust scores (bivariate normal, r≈0.6) -->
<!-- - [x] Changed from piecewise to linear time trends: -->
<!--   - Low trust: -0.3 per year -->
<!--   - Medium trust: 0.0 per year (stable) -->
<!--   - High trust: +0.2 per year -->
<!-- - [x] Made dropout depend on current trust values (not just baseline) -->
<!-- - [x] Reduced measurement noise (σ=0.10) for clearer education signal -->
<!-- - [x] Properly excluded simulation artifacts from imputation -->
<!---->
<!-- ### Phase 4: Documentation ✓ -->
<!-- - [x] Updated workflow_summary.md with new improvements -->
<!-- - [x] Updated osf_package_summary.md with imputation performance -->
<!-- - [x] Updated data/synthetic/README.md with new generation approach -->
<!-- - [x] Created PLANNING.md to track progress -->
<!-- - [x] Updated CHANGELOG_DEV.md with all changes -->
<!---->
<!-- ### Phase 5: Major Refactoring - Amelia to MICE PMM ✓ (2025-01-04) -->
<!---->
<!-- #### Epic Models Refactoring -->
<!-- - [x] Created new analysis pipeline at `/Users/joseph/GIT/epic-models/2025/25-refactor-trust-science/` -->
<!-- - [x] Developed complete set of analysis scripts: -->
<!--   - `00-simulate-trust-data.R`: Synthetic data with selection bias -->
<!--   - `01-helper-functions-mice.R`: Core MICE utilities -->
<!--   - `02-trust-science-growth-MICE.R`: Main imputation (20 imputations) -->
<!--   - `03-trust-science-analysis-MICE.R`: Pooled GEE with AR(1) -->
<!--   - `04-trust-science-consistent-MICE.R`: Sensitivity analyses -->
<!--   - `README.md`: Complete documentation -->
<!---->
<!-- #### Technical Improvements -->
<!-- - [x] **Data Format**: Converted from long to wide format for MICE -->
<!-- - [x] **Correlation**: All GEE models now use `corstr = "ar1"` -->
<!-- - [x] **Bounding**: Post-imputation bounding to [1,7] scale -->
<!-- - [x] **Forward Filling**: Time-invariant demographics to reduce missingness -->
<!-- - [x] **Baseline Cohort**: Retained original selection (year_measured == 1 & wave == 2019) -->
<!---->
<!-- #### Key Discoveries -->
<!-- - [x] **Wide Format Critical**: MICE requires wide format for longitudinal data -->
<!-- - [x] **Performance**: MICE PMM achieves ~100% bias correction with wide format -->
<!-- - [x] **Comparison**: Amelia achieves only ~54% bias correction -->
<!-- - [x] **Speed**: MICE PMM is faster than complex methods (RF, CART) -->
<!---->
<!-- ## Results Achieved -->
<!-- 1. **Selection Bias Demonstrated**:  -->
<!--    - Oracle shows population decline -->
<!--    - Observed shows artificial stability/increase -->
<!--    - Imputation successfully recovers true trend (MICE PMM with wide format) -->
<!---->
<!-- 2. **Technical Improvements**: -->
<!--    - Wide format enables proper temporal dependencies -->
<!--    - AR(1) correlation better models longitudinal data -->
<!--    - Forward filling reduces artificial missingness -->
<!--    - Post-imputation bounding ensures valid values -->
<!---->
<!-- 3. **Reproducibility**: -->
<!--    - All code runs without errors -->
<!--    - Complete documentation provided -->
<!--    - Synthetic data protects privacy -->
<!--    - Clear workflow from data to results -->
<!---->
<!-- ## Next Steps -->
<!-- 1. Run refactored analysis on full NZAVS dataset -->
<!-- 2. Compare results with original Amelia approach -->
<!-- 3. Optimize performance for large datasets -->
<!-- 4. Prepare publication with new methodology -->
<!-- 5. Update OSF package with improved approach -->
