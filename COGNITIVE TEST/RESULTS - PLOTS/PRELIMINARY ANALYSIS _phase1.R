# PRELIMINARY ANALYSIS
library(dplyr)
library(stringr)
library(ggplot2)

eval_dataset <- eval_dataset %>%
  mutate(
    # Normalization: Lowercase, removes dots, dollars and SPACES (\s) to smooth
    clean_response = str_remove_all(tolower(str_trim(direct_answer)), "[\\.\\$\\s]"),
    clean_expected = str_remove_all(tolower(str_trim(expected_answer)), "[\\.\\$\\s]"),
    
    # Scoring (1 = Correct, 0 = Incorrect)
    is_correct = if_else(clean_response == clean_expected, 1, 0),
    
    # Calculation of the LLM Error Rate (0 = Perfect, 1 = Incorrect)
    llm_error_rate = 1 - is_correct
  )

# Computation of mean aggregated error rates by category
summary_dataset <- eval_dataset %>%
  group_by(category) %>%
  summarise(
    mean_human_error = mean(human_error_rate, na.rm = TRUE),
    mean_llm_error = mean(llm_error_rate, na.rm = TRUE)
  )

# plot
ggplot(summary_dataset, aes(x = mean_human_error, y = mean_llm_error, color = category)) +
  geom_point(size = 7, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.75, y = 0.2, label = "LLM is Better", color = "blue", fontface = "bold") +
  annotate("text", x = 0.25, y = 0.8, label = "Human is Better", color = "red", fontface = "bold") +
  theme_minimal() +
  labs(
    title = "System 1 Response Evaluation (Direct Match)",
    subtitle = "Preliminary Error Rate Trade-off: Human vs Gemini 3.7 Flash",
    x = "Average Human Error Rate",
    y = "Average Gemini Error Rate (Direct Answer)",
    color = "Cognitive Category"
  ) +
  xlim(0, 1) + ylim(0, 1)