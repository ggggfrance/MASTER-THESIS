eval_dataset <- read.csv("eval_dataset_phase1_completed.csv", stringsAsFactors = FALSE)

# PRELIMINARY ANALYSIS - PHASE 1 
library(dplyr)
library(stringr)
library(ggplot2)

eval_dataset <- eval_dataset %>%
  mutate(
    clean_text_resp = str_remove_all(tolower(str_trim(direct_answer)), "[\\.\\$\\s]"),
    clean_text_exp = str_remove_all(tolower(str_trim(expected_answer)), "[\\.\\$\\s]"),
    clean_num_resp = suppressWarnings(as.numeric(str_remove_all(direct_answer, "[\\$\\s]"))),
    clean_num_exp = suppressWarnings(as.numeric(str_remove_all(expected_answer, "[\\$\\s]"))),
    
    is_correct = if_else(
      str_detect(category, "cognitive_reflection"),
      if_else(clean_num_resp == clean_num_exp, 1, 0, missing = 0),
      if_else(clean_text_resp == clean_text_exp, 1, 0)
    ),
    
    llm_error_rate = 1 - is_correct
  ) %>%
  select(-clean_text_resp, -clean_text_exp, -clean_num_resp, -clean_num_exp)

# Computation of mean aggregated error rates by category
summary_dataset <- eval_dataset %>%
  group_by(category) %>%
  summarise(
    mean_human_error = mean(human_error_rate, na.rm = TRUE),
    mean_llm_error = mean(llm_error_rate, na.rm = TRUE)
  )

# Plot 
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
