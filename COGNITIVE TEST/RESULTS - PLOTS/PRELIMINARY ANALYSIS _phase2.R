eval_dataset <- read.csv("eval_dataset_phase2_completed.csv", stringsAsFactors = FALSE)

# PRELIMINARY ANALYSIS
library(dplyr)
library(stringr)
library(ggplot2)

eval_dataset <- eval_dataset %>%
  mutate(
    clean_cot_text = str_remove_all(tolower(str_trim(cot_final_answer)), "[\\.\\$\\s]"),
    clean_exp_text = str_remove_all(tolower(str_trim(expected_answer)), "[\\.\\$\\s]"),
    clean_cot_num = suppressWarnings(as.numeric(str_remove_all(cot_final_answer, "[\\$\\s]"))),
    clean_exp_num = suppressWarnings(as.numeric(str_remove_all(expected_answer, "[\\$\\s]"))),
  
    is_correct_sys2 = if_else(
      str_detect(category, "cognitive_reflection"),
      if_else(clean_cot_num == clean_exp_num, 1, 0, missing = 0),
      if_else(clean_cot_text == clean_exp_text, 1, 0)
    ),
    
    llm_error_rate_sys2 = 1 - is_correct_sys2
  ) %>%
  select(-clean_cot_text, -clean_exp_text, -clean_cot_num, -clean_exp_num)

# Computation of mean aggregated error rates by category
summary_dataset_sys2 <- eval_dataset %>%
  group_by(category) %>%
  summarise(
    mean_human_error = mean(human_error_rate, na.rm = TRUE),
    mean_llm_error_sys2 = mean(llm_error_rate_sys2, na.rm = TRUE)
  )

# plot
ggplot(summary_dataset_sys2, aes(x = mean_human_error, y = mean_llm_error_sys2, color = category)) +
  geom_point(size = 7, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.75, y = 0.2, label = "LLM is Better", color = "blue", fontface = "bold") +
  annotate("text", x = 0.25, y = 0.8, label = "Human is Better", color = "red", fontface = "bold") +
  theme_minimal() +
  labs(
    title = "System 2 Response Evaluation (CoT Direct Match)",
    subtitle = "Preliminary Error Rate Trade-off: Human vs Gemini 3.7 Flash (Reasoning)",
    x = "Average Human Error Rate",
    y = "Average Gemini Error Rate (System 2 / CoT)",
    color = "Cognitive Category"
  ) +
  xlim(0, 1) + ylim(0, 1)
