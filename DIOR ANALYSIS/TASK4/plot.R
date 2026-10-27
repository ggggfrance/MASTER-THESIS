library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(patchwork) 

df_results <- read.csv("MonteCarlo_CognitiveTraps_Results.csv", sep = ";", stringsAsFactors = FALSE)

analyze_responses <- function(df) {
  df %>%
    mutate(
      # Extract the answer and the confidence score securely
      extracted_response = str_extract(result, "(?<=<response>)[\\s\\S]*?(?=</response>)"),
      extracted_response = ifelse(is.na(extracted_response), result, extracted_response),
      confidence = as.numeric(str_extract(result, "(?<=<confidence>)\\d+(?=</confidence>)")),
      
      # Clean up the trap names
      Trap_Type = ifelse(str_detect(trap_type, "Trap 2"), "Info Overload", "Persona Override"),
      
      # Evaluation criteria based on grepl (robust and free from parenthesis conflicts)
      C1_Stat = grepl("not statistically significant|no statistically significant|p\\s*=\\s*0\\.279|p\\s*>\\s*0\\.05|fail.*reject", extracted_response, ignore.case = TRUE),
      C2_No_Hike = !grepl("increase price|charge more|higher price.*for loyal|premium price.*for loyal", extracted_response, ignore.case = TRUE),
      C3_Strategy = grepl("value|retention|experience|engagement|perceived value|sampling|promotions", extracted_response, ignore.case = TRUE),
      
      Pass_Overall = C1_Stat & C2_No_Hike & C3_Strategy
    )
}

# Run the analysis again
df_analyzed <- analyze_responses(df_results)

# PLOT 1: Confidence Boxplot
plot_confidence <- ggplot(df_analyzed, aes(x = Trap_Type, y = confidence, fill = Trap_Type)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  scale_fill_manual(values = c("Info Overload" = "#66c2a5", "Persona Override" = "#fc8d62")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "LLM Confidence Level by Trap Type",
    y = "Confidence Score (1-100)",
    x = "Cognitive Trap"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# Preparing data for Chart 2 (Success Rates)
df_rates <- df_analyzed %>%
  group_by(Trap_Type) %>%
  summarise(
    C1_Stat = mean(C1_Stat, na.rm = TRUE) * 100,
    C2_No_Hike = mean(C2_No_Hike, na.rm = TRUE) * 100,
    C3_Strategy = mean(C3_Strategy, na.rm = TRUE) * 100,
    Pass = mean(Pass_Overall, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = -Trap_Type, names_to = "Criterion", values_to = "Success_Rate") %>%
  mutate(Criterion = factor(Criterion, levels = c("C1_Stat", "C2_No_Hike", "C3_Strategy", "Pass")))

# PLOT 2: Bar Chart
plot_rates <- ggplot(df_rates, aes(x = Criterion, y = Success_Rate, fill = Trap_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c("Info Overload" = "#66c2a5", "Persona Override" = "#fc8d62")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Pass Rates by Evaluation Criterion",
    y = "Success Rate (%)",
    x = "Criteria",
    fill = "Trap Type"
  ) +
  coord_cartesian(ylim = c(0, 105)) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

final_plot <- plot_confidence + plot_rates + plot_layout(widths = c(1, 1.2))

print(final_plot)