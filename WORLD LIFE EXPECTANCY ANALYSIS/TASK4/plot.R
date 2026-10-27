library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)

# ---------------------------------------------------------
# CHART 1: The Cognitive Trade-off (Accuracy per Task)
# ---------------------------------------------------------

# Transform data into the 'long' format required by ggplot
metrics_long <- summary_metrics %>%
  select(-total_perfect_responses) %>%
  pivot_longer(cols = everything(), names_to = "Task", values_to = "Accuracy") %>%
  mutate(Task = case_when(
    Task == "accuracy_anomaly" ~ "1. Anomaly Detection\n(Syntactic Check)",
    Task == "accuracy_log_log" ~ "2. Log-Log Interpretation\n(Context Deduction)",
    Task == "accuracy_applied_math" ~ "3. Econometric Calculation\n(Applied Math)",
    TRUE ~ Task
  ))

plot_accuracy <- ggplot(metrics_long, aes(x = Task, y = Accuracy, fill = Task)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = scales::percent(Accuracy)), vjust = -0.5, fontface = "bold", size = 5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
  scale_fill_manual(values = c("#4CAF50", "#F44336", "#FF9800")) + # Green for success, red/orange for failures
  labs(
    title = "LLM Performance by Statistical Task Type",
    subtitle = "100-Simulation Stress Test: Structural Excellence vs Semantic Blindness",
    x = NULL,
    y = "Accuracy Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", size = 11)
  )

print(plot_accuracy)


# ---------------------------------------------------------
# CHART 2: Detailed Original Error Distribution
# ---------------------------------------------------------

# Maintain original text but limit to the top 7 most frequent exact phrases
top_errors <- error_analysis %>%
  slice_max(n, n = 7, with_ties = FALSE) %>%
  # Wrap text at 65 characters to keep lines neat
  mutate(error_reasoning_wrapped = str_wrap(error_reasoning, width = 65)) %>%
  # Reorder for plotting
  mutate(error_reasoning_wrapped = reorder(error_reasoning_wrapped, n))

plot_errors <- ggplot(top_errors, aes(x = n, y = error_reasoning_wrapped)) +
  geom_col(fill = "#E57373", width = 0.7) +
  geom_text(aes(label = n), hjust = -0.3, size = 4.5, fontface = "bold") +
  # Expand X axis slightly to make room for the numbers
  scale_x_continuous(limits = c(0, max(top_errors$n) * 1.15)) +
  labs(
    title = "Classification of Interpretation Errors",
    subtitle = "Top exact logical hallucinations diagnosed by the scorer",
    x = "Number of Occurrences",
    y = NULL 
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    # Reduce text size and lineheight to prevent overlaps
    axis.text.y = element_text(size = 9.5, lineheight = 0.9), 
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 10)
  )

print(plot_errors)
