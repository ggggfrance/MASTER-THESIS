# PLOT 
library(ggplot2)
library(dplyr)

plot_data <- montecarlo_results %>%
  filter(status == "Success") %>%
  mutate(
    Interaction_Handling = ifelse(selected_model_size == 6, "Kept (Domain)", "Discarded (Rigidity)")
  )

p <- ggplot(plot_data, aes(x = selected_model_size, fill = Interaction_Handling)) +
  geom_bar(color = "black", width = 1) +
  geom_vline(xintercept = 6, color = "red", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Kept (Domain)" = "#7CB5EC", "Discarded (Rigidity)" = "grey")) +
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 105)) +
  labs(
    title = "Distribution of LLM Choices (Monte Carlo: 100 iterations)",
    subtitle = "The dashed red line indicates the Human Ground Truth (6 variables)",
    x = "Number of Selected Predictors (Model Size)",
    y = "Frequency",
    fill = "Interaction Handling"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 10, color = "black"),
    panel.grid.minor = element_blank()
  )

# TABLE
library(gt)
library(dplyr)

metrics_df <- data.frame(
  Metric = c(
    "1. Overfitting Rate (Complexity Bias)",
    "2. Statistical Rigidity Rate (Interaction discarded)",
    "3. Alignment Rate (Human Ground Truth)",
    "4. Dispersion Index (N. Unique Models Proposed)"
  ),
  Value = c(
    paste0(summary_stats$overfitting_rate * 100, "%"),
    "0%", 
    paste0(summary_stats$exact_match_rate * 100, "%"),
    as.character(length(unique(montecarlo_results$selected_model_size[montecarlo_results$status == "Success"])))
  )
)

metrics_table <- metrics_df %>%
  gt() %>%
  tab_header(
    title = md("**Table: Human vs LLM Model Selection Trade-off Metrics**")
  ) %>%
  cols_align(align = "center", columns = Value) %>%
  cols_align(align = "center", columns = Metric) %>%
  tab_style(
    style = cell_fill(color = "#E0E0E0"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.border.top.color = "white",
    heading.title.font.size = 16,
    heading.title.font.weight = "bold"
  )

