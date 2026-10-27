library(ggplot2)
library(dplyr)

dataset_viz <- read.csv("Definitive_Dataset_CoT_Task1_2.csv", sep = ";")

theme_set(theme_minimal(base_size = 14) + 
          theme(plot.title = element_text(face = "bold", size = 16),
                plot.subtitle = element_text(color = "grey40", size = 12)))

# PLOT 1: THE EVALUATION GAP 
gap_data_viz <- data.frame(
  Evaluation_Method = factor(
    c("Strict Regex Benchmark\n(4 Specific Plots)", "LLM-as-a-Judge\n(Exploratory Validity)"),
    levels = c("Strict Regex Benchmark\n(4 Specific Plots)", "LLM-as-a-Judge\n(Exploratory Validity)")
  ),
  Success_Rate = c(
    mean(dataset_viz$Pass_Rate_Strict == "TRUE") * 100, 
    mean(dataset_viz$LLM_Judge_Score >= 4) * 100 
  )
)

plot_gap_viz <- ggplot(gap_data_viz, aes(x = Evaluation_Method, y = Success_Rate, fill = Evaluation_Method)) +
  geom_bar(stat = "identity", color = "black", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(round(Success_Rate, 1), "%")), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("tomato", "seagreen")) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 25)) +
  labs(
    title = "The Under-Specification Penalty in Code Generation",
    subtitle = "Models fail strict target matching but produce highly valid exploratory analyses",
    x = NULL,
    y = "Success Rate (%)"
  ) +
  theme(legend.position = "none")

# PLOT 2: CONFIDENCE DISTRIBUTION  
mean_conf_viz <- mean(dataset_viz$Confidence_Score, na.rm = TRUE)

plot_confidence_viz <- ggplot(dataset_viz, aes(x = Confidence_Score)) +
  geom_histogram(binwidth = 2, fill = "darkorange", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean_conf_viz, color = "darkred", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = mean_conf_viz + 0.5, y = 75, 
           label = paste("Mean Confidence:\n", round(mean_conf_viz, 1), "%"), 
           color = "darkred", fontface = "bold", hjust = 0) +
  coord_cartesian(xlim = c(min(dataset_viz$Confidence_Score, na.rm=TRUE) - 2, 102)) +
  labs(
    title = "Model Certainty Under Ambiguous Instructions",
    subtitle = "LLM confidence remains high despite ambiguous 'suitable representation' prompt",
    x = "Self-Reported Confidence Score (0-100)",
    y = "Frequency (Number of Iterations)"
  )

ggsave("Plot_1_Evaluation_Gap.png", plot = plot_gap_viz, width = 9, height = 6, dpi = 300)
ggsave("Plot_2_Confidence_Distribution.png", plot = plot_confidence_viz, width = 9, height = 6, dpi = 300)
