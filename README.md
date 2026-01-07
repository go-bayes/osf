# Trust in Science: Attrition Simulation Methods

1. This repository now centres on one canonical simulation script that compares Oracle, Observed, IPCW, Amelia, and MICE under MAR and MNAR attrition. It saves figures to `results/figures` and outputs to `results/objects`.
2. Run the simulation from the project root with the command below. The script will create any required output directories.

```r
source("code/simulate_attrition_methods.R")
```

3. The active code lives in `code/simulate_attrition_methods.R` with helper functions in `code/functions`. The legacy pipeline is archived under `retired/legacy_pipeline` for reference.
4. The report template is available at `documentation/analysis_report.qmd` and uses the saved outputs.
