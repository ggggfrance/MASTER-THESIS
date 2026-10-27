# DUMBBELL PLOT: SYSTEM 1 VS SYSTEM 2 (Chain of Thought effect)
library(dplyr)
library(ggplot2)

df_dumbbell <- df_clean %>%
  group_by(category) %>%
  summarise(
    mean_sys1 = mean(error_sys1, na.rm = TRUE),
    mean_sys2 = mean(error_sys2, na.rm = TRUE)
  )

ggplot(df_dumbbell) +
  geom_segment(aes(y = reorder(category, mean_sys1), yend = reorder(category, mean_sys1), 
                   x = mean_sys1, xend = mean_sys2), color = "gray60", size = 1) +
  # System 1 points
  geom_point(aes(y = category, x = mean_sys1, color = "System 1 (Direct)"), size = 4) +
  # System 2 points
  geom_point(aes(y = category, x = mean_sys2, color = "System 2 (CoT)"), size = 4) +
  scale_color_manual(values = c("System 1 (Direct)" = "tomato", "System 2 (CoT)" = "steelblue")) +
  theme_minimal() +
  labs(
    title = "Impact of Chain-of-Thought on Error Rates",
    subtitle = "System 1 (Instinctive) vs System 2 (Reasoning) by Category",
    x = "Average Error Rate",
    y = "",
    color = "Response Mode"
  ) +
  theme(legend.position = "bottom")