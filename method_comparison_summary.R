# method_comparison_summary.R
# Summary of when to use each missing data method
# joseph.bulbulia@gmail.com

library(tidyverse)
library(gt)
library(patchwork)

# Create summary data frame
method_summary <- data.frame(
  Method = c("Complete Case", "MICE", "Amelia", "IPCW", "Delta Adjustment"),
  
  Assumptions = c(
    "MCAR (Missing Completely At Random)",
    "MAR (Missing At Random)",
    "MAR with multilevel structure",
    "Dropout mechanism can be modeled",
    "MNAR with sensitivity analysis"
  ),
  
  Strengths = c(
    "Simple, unbiased if MCAR holds",
    "Flexible, handles complex patterns",
    "Captures individual heterogeneity",
    "Direct modeling of dropout",
    "Acknowledges uncertainty"
  ),
  
  Weaknesses = c(
    "Biased if dropout is informative",
    "May miss group-specific trends",
    "Assumes common time structure",
    "Requires correct dropout model",
    "Results depend on δ values"
  ),
  
  Best_When = c(
    "Minimal missing data (<5%)",
    "Homogeneous trajectories",
    "Heterogeneous trajectories",
    "Dropout mechanism understood",
    "Need sensitivity analysis"
  ),
  
  Performance_Score = c(1, 4, 5, 3, 3)
)

# Create performance comparison from simulation
simulation_results <- data.frame(
  Method = c("Complete Case", "MICE", "Amelia", "IPCW"),
  MAE = c(0.105, 0.0131, 0.00843, 0.0408),
  Bias = c(0.105, 0.0131, 0.00767, 0.0386),
  Rank = c(4, 2, 1, 3)
)

# Create visualization
p1 <- ggplot(simulation_results, aes(x = reorder(Method, -MAE), y = MAE, fill = Method)) +
  geom_col() +
  geom_text(aes(label = round(MAE, 3)), vjust = -0.5) +
  scale_fill_manual(values = c(
    "Complete Case" = "red",
    "MICE" = "orange",
    "Amelia" = "darkgreen",
    "IPCW" = "blue"
  )) +
  labs(
    x = "Method",
    y = "Mean Absolute Error",
    title = "Simulation Performance: Recovery of True Means",
    subtitle = "Lower is better (Amelia performs best in heterogeneous trajectory scenario)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Create decision tree visualization
decision_data <- data.frame(
  x = c(0, -2, 2, -3, -1, 1, 3),
  y = c(4, 3, 3, 2, 2, 2, 2),
  label = c(
    "Missing Data\nProblem",
    "MCAR?", "Not MCAR",
    "Complete\nCase", "MAR?", "MNAR?", "Sensitivity\nAnalysis"
  ),
  type = c("start", "decision", "decision", "method", "decision", "decision", "method")
)

arrow_data <- data.frame(
  x = c(0, 0, -2, 2, 2),
  xend = c(-2, 2, -3, -1, 1),
  y = c(4, 4, 3, 3, 3),
  yend = c(3, 3, 2, 2, 2),
  label = c("Check", "Check", "Yes", "Yes", "No")
)

p2 <- ggplot(decision_data, aes(x = x, y = y)) +
  geom_segment(data = arrow_data, 
               aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.3, "cm")),
               size = 1) +
  geom_label(data = arrow_data,
             aes(x = (x + xend)/2, y = (y + yend)/2, label = label),
             size = 3) +
  geom_point(aes(color = type), size = 15) +
  geom_text(aes(label = label), size = 3, lineheight = 0.8) +
  scale_color_manual(values = c(
    "start" = "black",
    "decision" = "orange", 
    "method" = "darkgreen"
  )) +
  xlim(-4, 4) + ylim(1, 5) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Decision Tree for Missing Data Methods")

# Create summary table using gt
summary_table <- method_summary %>%
  select(Method, Assumptions, Best_When, Performance_Score) %>%
  gt() %>%
  tab_header(
    title = "When to Use Each Missing Data Method",
    subtitle = "Based on simulation results and theoretical considerations"
  ) %>%
  cols_label(
    Performance_Score = "Performance\n(1-5)"
  ) %>%
  data_color(
    columns = Performance_Score,
    colors = scales::col_numeric(
      palette = c("red", "orange", "yellow", "lightgreen", "darkgreen"),
      domain = c(1, 5)
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "darkgreen", alpha = 0.3),
    locations = cells_body(
      columns = everything(),
      rows = Method == "Amelia"
    )
  ) %>%
  tab_footnote(
    footnote = "Performance based on simulation with heterogeneous trajectories",
    locations = cells_column_labels(columns = Performance_Score)
  )

# Print outputs
print(p1)
print(p2)
print(summary_table)

# Key recommendations
cat("\n=== KEY RECOMMENDATIONS ===\n\n")
cat("1. NO SINGLE METHOD IS ALWAYS BEST\n")
cat("   - Method performance depends on the data structure\n")
cat("   - Consider the missing data mechanism carefully\n\n")

cat("2. FOR HETEROGENEOUS TRAJECTORIES (like trust by education):\n")
cat("   - Amelia excels due to multilevel structure\n")
cat("   - MICE needs careful setup (wide format, auxiliaries)\n")
cat("   - IPCW may overcorrect if groups have different trends\n\n")

cat("3. FOR HOMOGENEOUS TRAJECTORIES:\n")
cat("   - MICE and IPCW perform similarly well\n")
cat("   - Amelia's advantage diminishes\n")
cat("   - Focus on modeling dropout mechanism\n\n")

cat("4. ALWAYS PERFORM SENSITIVITY ANALYSIS:\n")
cat("   - Compare multiple methods\n")
cat("   - Use delta adjustment to test MNAR scenarios\n")
cat("   - Report uncertainty in conclusions\n\n")

cat("5. IN PRACTICE:\n")
cat("   - Start with visual inspection of missing patterns\n")
cat("   - Test MAR assumption with auxiliary variables\n")
cat("   - Use multiple methods and compare results\n")
cat("   - Be transparent about assumptions\n")

# Save combined visualization
combined_viz <- p1 / p2 +
  plot_annotation(
    title = "Choosing the Right Missing Data Method",
    subtitle = "Performance depends on data structure and missing mechanism"
  )

ggsave("results/figures/missing_data_methods_guide.png", combined_viz,
       width = 10, height = 12, dpi = 300)