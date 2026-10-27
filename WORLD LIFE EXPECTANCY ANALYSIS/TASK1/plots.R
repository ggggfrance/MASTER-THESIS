# EVALUATION TRADE-OFF 

library(ggplot2)
library(dplyr)

dataset <- read.csv("Definitive_Dataset_CoT.csv", sep = ";")

theme_set(theme_minimal(base_size = 14) + 
          theme(plot.title = element_text(face = "bold", size = 16),
                plot.subtitle = element_text(color = "grey40", size = 12)))

# PLOT 1: THE EVALUATION GAP
gap_data <- data.frame(
  Evaluation_Method = factor(
    c("Strict Regex Benchmark\n(Human Target)", "LLM-as-a-Judge\n(Statistical Reasoning)"),
    levels = c("Strict Regex Benchmark\n(Human Target)", "LLM-as-a-Judge\n(Statistical Reasoning)")
  ),
  Success_Rate = c(
    mean(dataset$Pass_Rate_Strict == "TRUE") * 100, 
    mean(dataset$LLM_Judge_Score == 5) * 100
  )
)

plot_gap <- ggplot(gap_data, aes(x = Evaluation_Method, y = Success_Rate, fill = Evaluation_Method)) +
  geom_bar(stat = "identity", color = "black", width = 0.5, alpha = 0.8) +
  geom_text(aes(label = paste0(Success_Rate, "%")), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("tomato", "seagreen")) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 25)) +
  labs(
    title = "The Evaluation Paradox in LLM Data Analysis",
    subtitle = "100% failure rate in syntax matching translates to 100% methodological success",
    x = NULL,
    y = "Success Rate (%)"
  ) +
  theme(legend.position = "none")


# PLOT 2: CONFIDENCE DISTRIBUTION  
mean_conf <- mean(dataset$Confidence_Score, na.rm = TRUE)

plot_confidence <- ggplot(dataset, aes(x = Confidence_Score)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean_conf, color = "darkred", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = mean_conf + 0.5, y = 75, 
           label = paste("Mean Confidence:\n", round(mean_conf, 1), "%"), 
           color = "darkred", fontface = "bold", hjust = 0) +
  coord_cartesian(xlim = c(min(dataset$Confidence_Score, na.rm=TRUE) - 2, 102)) +
  labs(
    title = "Model Certainty in Methodological Divergence",
    subtitle = "Distribution of LLM confidence scores when omitting the logarithmic transformation",
    x = "Self-Reported Confidence Score (0-100)",
    y = "Frequency (Number of Iterations)"
  )

ggsave("Plot_1_Evaluation_Gap.png", plot = plot_gap, width = 9, height = 6, dpi = 300)
ggsave("Plot_2_Confidence_Distribution.png", plot = plot_confidence, width = 9, height = 6, dpi = 300)

