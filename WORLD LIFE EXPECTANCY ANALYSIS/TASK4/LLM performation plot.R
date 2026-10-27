library(ggplot2)
library(dplyr)
library(tidyr)

df_plot <- data.frame(
  Scenario = rep(c("1. Baseline (Guided)", "2. Stress Test (Econometric)", "3. Anomaly Test"), each = 3),
  Task = rep(c("Syntactic/Extraction", "Log-Log Deduction", "Applied Math"), 3),
  Accuracy = c(
    1.00, 1.00, NA,   # Baseline 
    1.00, 0.00, 0.00, # Econometric
    1.00, 0.00, 0.00  # Anomaly
  )
)

ggplot(df_plot, aes(x = Scenario, y = Accuracy, fill = Task)) +
  geom_col(position = "dodge", color = "black", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
  scale_fill_manual(values = c("Syntactic/Extraction" = "#4CAF50", 
                               "Log-Log Deduction" = "#F44336", 
                               "Applied Math" = "#FF9800")) +
  labs(
    title = "LLM Performance Degradation across Complexity Levels",
    subtitle = "High syntactic reliability vs. complete semantic failure in unguided tasks",
    x = "Simulation Type",
    y = "Accuracy Rate",
    fill = "Evaluated Skill"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
