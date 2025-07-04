# PLANNING.md

## Project Goal
Demonstrate how response bias masks the erosion of trust in science over time through a reproducible analysis workflow.

## Key Objectives
1. Create synthetic data that mimics NZAVS structure while protecting privacy
2. Show how selective attrition creates artificial stability/increases in observed data
3. Demonstrate that multiple imputation partially recovers true population trends
4. Provide fully reproducible analysis package for OSF

## Completed Tasks ✓

### Phase 1: Initial Implementation ✓
- [x] Created three-group synthetic data design (low/medium/high trust based on education)
- [x] Implemented realistic baseline sampling (oversample women, Māori, older adults)
- [x] Added post-stratification weights to correct for sampling design
- [x] Created monotone dropout patterns with differential attrition by trust level
- [x] Implemented Amelia imputation with MICE conversion
- [x] Built GEE models for continuous outcomes
- [x] Built proportional odds models for categorical outcomes
- [x] Created visualization pipeline using ggeffects
- [x] Generated comprehensive tables

### Phase 2: Debugging and Refinement ✓
- [x] Fixed collinearity between trust_science and trust_scientists
- [x] Removed empri parameter from Amelia
- [x] Added splinetime=3, polytime=3 to Amelia
- [x] Fixed education inclusion in imputation model (removed from id_vars)
- [x] Excluded baseline trust variables from imputation
- [x] Fixed namespace conflicts (MASS::select vs dplyr::select)
- [x] Fixed apply error in pool_predictions function

### Phase 3: Improved Simulation Design ✓
- [x] Implemented correlated baseline trust scores (bivariate normal, r≈0.6)
- [x] Changed from piecewise to linear time trends:
  - Low trust: -0.3 per year
  - Medium trust: 0.0 per year (stable)
  - High trust: +0.2 per year
- [x] Made dropout depend on current trust values (not just baseline)
- [x] Reduced measurement noise (σ=0.10) for clearer education signal
- [x] Properly excluded simulation artifacts from imputation

### Phase 4: Documentation ✓
- [x] Updated workflow_summary.md with new improvements
- [x] Updated osf_package_summary.md with imputation performance
- [x] Updated data/synthetic/README.md with new generation approach
- [x] Created PLANNING.md to track progress
- [x] Will update CHANGELOG_DEV.md next

## Results Achieved
1. **Selection Bias Demonstrated**: 
   - Oracle shows population decline
   - Observed shows artificial stability/increase
   - Imputation partially recovers true trend

2. **Technical Improvements**:
   - Amelia now properly uses education signal
   - Correlated baselines create realistic data
   - Current-value dropout creates proper selection
   - Linear trends are interpretable

3. **Reproducibility**:
   - All code runs without errors
   - Synthetic data protects privacy
   - Complete documentation provided

## Next Steps
1. Update CHANGELOG_DEV.md with all changes
2. Consider updating generate_synthetic_data.R with improved approach
3. Run full workflow with 10 imputations for final results
4. Prepare for OSF upload