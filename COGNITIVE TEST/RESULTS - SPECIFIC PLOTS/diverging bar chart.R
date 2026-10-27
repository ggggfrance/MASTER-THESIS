# DIVERGING BAR CHART: ROBUSTNESS AGAINST NOISE (Clean vs Perturbed)
library(dplyr)
library(ggplot2)

df_robustness <- df_clean %>%
  group_by(base_task, condition) %>%
  summarise(mean_error = mean(error_sys2, na.rm = TRUE), .groups = 'drop')

ggplot(df_robustness, aes(x = base_task, y = mean_error, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Clean" = "#2ca02c", "Perturbed" = "#d62728")) +
  theme_minimal() +
  labs(
    title = "Model Robustness to Typographical Perturbations",
    subtitle = "System 2 Error Rates on Clean vs Perturbed Prompts",
    x = "Cognitive Task",
    y = "System 2 Error Rate",
    fill = "Prompt Condition"
  ) +
  coord_flip() 