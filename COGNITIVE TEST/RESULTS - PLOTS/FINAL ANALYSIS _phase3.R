# FINAL ANALYSIS
library(dplyr)
library(ggplot2)

df_final <- read.csv("eval_dataset_final_scored.csv", stringsAsFactors = FALSE)

# Computation of final aggregated error rates by category
summary_final <- df_final %>%
  mutate(
    error_sys1 = 1 - as.numeric(score_system1),
    error_sys2 = 1 - as.numeric(score_system2)
  ) %>%
  group_by(category) %>%
  summarise(
    mean_human = mean(human_error_rate, na.rm = TRUE),
    mean_sys1 = mean(error_sys1, na.rm = TRUE),
    mean_sys2 = mean(error_sys2, na.rm = TRUE)
  )

ggplot(summary_final) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = mean_human, xend = mean_human, 
                   y = mean_sys1, yend = mean_sys2),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"), 
               color = "gray40", alpha = 0.6, linewidth = 1) +
  geom_point(aes(x = mean_human, y = mean_sys1, color = category), 
             size = 6, shape = 16, alpha = 0.4) +
  geom_point(aes(x = mean_human, y = mean_sys2, color = category), 
             size = 6, shape = 17) +
  annotate("text", x = 0.85, y = 0.1, label = "LLM is Better", color = "blue", fontface = "italic") +
  annotate("text", x = 0.15, y = 0.9, label = "Human is Better", color = "red", fontface = "italic") +
  theme_minimal() +
  labs(
    title = "Final Error Rate Trade-off: Human vs Gemini 3.7 Flash",
    subtitle = "Arrows indicate error reduction from System 1 (Circles) to System 2 (Triangles)",
    x = "Average Human Error Rate",
    y = "Validated LLM Error Rate",
    color = "Cognitive Category"
  ) +
  xlim(0, 1) + ylim(0, 1) +
  theme(legend.position = "right")


#ZOOM
ggplot(summary_final) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_segment(aes(x = mean_human, xend = mean_human, 
                   y = mean_sys1, yend = mean_sys2),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"), 
               color = "gray40", alpha = 0.6, linewidth = 1) +
  geom_point(aes(x = mean_human, y = mean_sys1, color = category), 
             size = 6, shape = 16, alpha = 0.4) +
  geom_point(aes(x = mean_human, y = mean_sys2, color = category), 
             size = 6, shape = 17) +
  annotate("text", x = 0.70, y = 0.22, label = "LLM is Better", color = "blue", fontface = "italic") +
  theme_minimal() +
  labs(
    title = "Final Error Rate Trade-off: Human vs Gemini 3.7 Flash",
    subtitle = "Zoomed view: System 1 (Circles) to System 2 (Triangles)",
    x = "Average Human Error Rate",
    y = "Validated LLM Error Rate",
    color = "Cognitive Category"
  ) +
  
  coord_cartesian(ylim = c(-0.01, 0.25), xlim = c(0.35, 0.85)) +
  theme(legend.position = "right")


# OVERLAPPING PROOF - DETAILED TRADE-OFF MATRIX (Clean vs Perturbed)
library(dplyr)
library(stringr)
library(gt)

df_clean <- read.csv("eval_dataset_final_scored.csv", stringsAsFactors = FALSE) %>%
  mutate(
    error_sys1 = 1 - as.numeric(score_system1),
    error_sys2 = 1 - as.numeric(score_system2),
    condition = if_else(str_detect(category, "_perturbed"), "Perturbed", "Clean"),
    base_task = str_remove(category, "_perturbed")
  )

df_detailed <- df_clean %>%
  group_by(base_task, condition) %>%
  summarise(
    Human_Error = mean(human_error_rate, na.rm = TRUE),
    Gemini_Sys1 = mean(error_sys1, na.rm = TRUE),
    Gemini_Sys2 = mean(error_sys2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(base_task, match(condition, c("Clean", "Perturbed")))

detailed_table <- df_detailed %>%
  gt(groupname_col = "base_task") %>% 
  tab_header(
    title = md("**Detailed Error Rates: Clean vs Perturbed**"),
    subtitle = "Empirical proof of model robustness and plotting overlap"
  ) %>%
  cols_label(
    condition = "Condition",
    Human_Error = "Human Error",
    Gemini_Sys1 = "Sys1 Error (Instinct)",
    Gemini_Sys2 = "Sys2 Error (CoT)"
  ) %>%
  fmt_percent(
    columns = c(Human_Error, Gemini_Sys1, Gemini_Sys2),
    decimals = 1
  ) %>%
  data_color(
    columns = c(Gemini_Sys2),
    colors = scales::col_numeric(
      palette = c("#eafaf1", "#f9ebea"), 
      domain = c(0, 1)
    )
  ) %>%
  tab_options(
    row_group.font.weight = "bold",
    row_group.background.color = "gray95",
    table.font.size = "small",
    column_labels.font.weight = "bold"
  )

detailed_table
gtsave(detailed_table, "table_2_phase3_zoom.html")