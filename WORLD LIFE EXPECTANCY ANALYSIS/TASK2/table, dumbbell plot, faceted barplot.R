# TABLE COMPARISON
library(flextable)
library(dplyr)

comparison_df <- data.frame(
  Metric = c(
    "Alignment Rate (Human Ground Truth)", 
    "Overfitting Rate (Complexity Bias)", 
    "Dispersion Index (Standard Deviation)", 
    "Parsing Failure Rate"
  ),
  Standard_Baseline = c("100%", "0%", "0.00", "0%"),
  Adversarial_Stress_Test = c("100%", "0%", "0.00", "0%") 
)

robustness_ft <- flextable(comparison_df) %>%
  theme_vanilla() %>%
  set_header_labels(
    Metric = "Metric",
    Standard_Baseline = "Standard Baseline\n(Temp 0.0, Guided)",
    Adversarial_Stress_Test = "Adversarial Stress Test\n(Temp 1.2, Conflicting)"
  ) %>%
  add_header_lines(values = "Standard Prompt vs. Adversarial High-Entropy Scenario (Temperature = 1.2)") %>%
  add_header_lines(values = "Robustness Analysis: LLM Model Selection Performance") %>%
  bg(bg = "white", part = "all") %>%
  color(color = "black", part = "all") %>%
  align(j = c("Standard_Baseline", "Adversarial_Stress_Test"), align = "center", part = "all") %>%
  align(j = "Metric", align = "left", part = "all") %>%
  bold(i = 3, part = "header") %>% 
  bold(i = 1, part = "header") %>% 
  fontsize(i = 1, size = 16, part = "header") %>%
  fontsize(i = 2, size = 11, part = "header") %>%
  flextable::autofit()

save_as_image(robustness_ft, path = "Table_Robustness_Comparison.png")

# DUMBBELL PLOT
library(ggplot2)
library(dplyr)
library(tidyr)

dumbbell_data <- data.frame(
  Metric = factor(
    c("Alignment Rate", "Overfitting Rate", "Parsing Failure Rate"),
    levels = c("Parsing Failure Rate", "Overfitting Rate", "Alignment Rate") 
  ),
  Baseline = c(100, 0, 0),
  Stress_Test = c(100, 0, 0)
)

p_dumbbell <- ggplot(dumbbell_data) +
  geom_segment(aes(x = Baseline, xend = Stress_Test, y = Metric, yend = Metric), 
               color = "grey", linewidth = 1.5) +
  # baseline points
  geom_point(aes(x = Baseline, y = Metric, color = "Standard Baseline"), size = 6, alpha = 0.7) +
  # stress test points 
  geom_point(aes(x = Stress_Test, y = Metric, color = "Adversarial Stress Test"), size = 3) +
  scale_color_manual(
    name = "Scenario",
    values = c("Standard Baseline" = "#2C7BB6", "Adversarial Stress Test" = "#D7191C")
  ) +
  scale_x_continuous(limits = c(-5, 105), breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Zero-Degradation in LLM Statistical Reasoning",
    subtitle = "Performance comparison across standard and adversarial prompt architectures",
    x = "Success/Failure Rate (%)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey80")
  )

ggsave("Dumbbell_Plot_Robustness.png", plot = p_dumbbell, width = 9, height = 5, dpi = 300)

# FACETED BARPLOT
library(ggplot2)
library(dplyr)

baseline_df <- data.frame(
  Iteration = 1:100, 
  Selected_Size = 6, 
  Scenario = "Standard Baseline (Temp: 0.0)"
)

stress_df <- data.frame(
  Iteration = 1:100, 
  Selected_Size = 6, 
  Scenario = "Adversarial Stress Test (Temp: 1.2)"
)

combined_results <- bind_rows(baseline_df, stress_df) %>%
  mutate(Scenario = factor(Scenario, levels = c("Standard Baseline (Temp: 0.0)", "Adversarial Stress Test (Temp: 1.2)")))

p_facet <- ggplot(combined_results, aes(x = Selected_Size, fill = Scenario)) +
  geom_bar(color = "black", width = 1) +
  geom_vline(xintercept = 6, color = "black", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Scenario) +
  scale_fill_manual(values = c("Standard Baseline (Temp: 0.0)" = "#2C7BB6", 
                               "Adversarial Stress Test (Temp: 1.2)" = "#D7191C")) +
  scale_x_continuous(breaks = 1:15, limits = c(0.5, 15.5)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
  labs(
    title = "Determinism in Model Size Selection across 100 Iterations",
    subtitle = "The dashed line indicates the Human Ground Truth (6 predictors)",
    x = "Selected Number of Predictors (Model Size)",
    y = "Frequency (Number of Iterations)"
  ) +
  theme_bw() +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "grey90"),
    panel.grid.minor = element_blank()
  )

ggsave("Faceted_Plot_Distribution.png", plot = p_facet, width = 10, height = 5, dpi = 300)
