# CALIBRATION CURVE: MODEL OVERCONFIDENCE 
library(dplyr)
library(ggplot2)

# Divide the model's confidence (0–100) into 10% bins
df_calib <- df_clean %>%
  filter(!is.na(model_confidence)) %>%
  mutate(
    conf_bin = cut(model_confidence, breaks = seq(0, 100, by = 10), include.lowest = TRUE, labels = seq(5, 95, by = 10)),
    conf_numeric = as.numeric(as.character(conf_bin)) / 100 
  ) %>%
  group_by(conf_numeric) %>%
  summarise(
    actual_accuracy = mean(score_system2, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count > 2) # Filter out bins with too little data to avoid noise

ggplot(df_calib, aes(x = conf_numeric, y = actual_accuracy)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_line(color = "purple", linewidth = 1) + 
  geom_point(aes(size = count), color = "purple", alpha = 0.7) +
  geom_text(aes(label = scales::percent(actual_accuracy, accuracy = 1)), 
            vjust = -1.5, 
            color = "black", 
            size = 3.5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  theme_minimal() +
  labs(
    title = "Calibration Curve: Expected vs Actual Accuracy",
    subtitle = "Does Gemini 3.7 Flash know when it's wrong?",
    x = "Model Confidence (Stated)",
    y = "Actual Accuracy (Judged)",
    size = "Sample Size"
  )