library(ggplot2)

df_error <- data.frame(
  Error_Type = c(
    "Misidentified as log-level (Main Error)", 
    "Minor parsing variations (Same logical root)"
  ),
  Count = c(84, 16)
)

ggplot(df_error, aes(x = reorder(Error_Type, Count), y = Count)) +
  geom_col(fill = "#B71C1C", color = "black", width = 0.5) +
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Error Taxonomy in Adversarial Task",
    subtitle = "100% of failures stem from log-level vs log-log misidentification",
    x = "",
    y = "Number of Iterations"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 11))
