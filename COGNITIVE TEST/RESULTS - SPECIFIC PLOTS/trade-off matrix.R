# TRADE-OFF MATRIX 
library(dplyr)
library(ggplot2)
library(gt)

df_table <- df_clean %>%
  group_by(base_task) %>%
  summarise(
    Human_Error = mean(human_error_rate, na.rm = TRUE),
    Gemini_Sys1_Error = mean(error_sys1, na.rm = TRUE),
    Gemini_Sys2_Error = mean(error_sys2, na.rm = TRUE)
  ) %>%
  mutate(
    # Calculating deltas 
    Sys1_vs_Human = Gemini_Sys1_Error - Human_Error,
    Sys2_vs_Human = Gemini_Sys2_Error - Human_Error,
    Sys2_Improvement = Gemini_Sys1_Error - Gemini_Sys2_Error
  )

tradeoff_table <- df_table %>%
  gt() %>%
  tab_header(
    title = md("**Error Rate Trade-off Comparison**"),
    subtitle = "Human Baseline vs Gemini 3.7 Flash (System 1 & System 2)"
  ) %>%
  cols_label(
    base_task = "Cognitive Category",
    Human_Error = "Human",
    Gemini_Sys1_Error = "Gemini (Direct)",
    Gemini_Sys2_Error = "Gemini (CoT)",
    Sys1_vs_Human = "Δ Sys1 - Human",
    Sys2_vs_Human = "Δ Sys2 - Human",
    Sys2_Improvement = "CoT Benefit"
  ) %>%
  fmt_percent(
    columns = c(Human_Error, Gemini_Sys1_Error, Gemini_Sys2_Error, Sys1_vs_Human, Sys2_vs_Human, Sys2_Improvement),
    decimals = 1
  ) %>%
  # Highlight in green the cells where Chain of Thought reduced the error
  data_color(
    columns = c(Sys2_Improvement),
    colors = scales::col_numeric(palette = c("white", "lightgreen"), domain = NULL)
  ) %>%
  # Color the final Sys2 vs Human comparison red/blue
  data_color(
    columns = c(Sys2_vs_Human),
    colors = scales::col_numeric(palette = c("lightblue", "white", "lightpink"), domain = c(-0.5, 0.5))
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.title.font.weight = "bold"
  )

tradeoff_table
gtsave(tradeoff_table, "table_1_tradeoff.html")
