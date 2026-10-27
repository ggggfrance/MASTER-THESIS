library(ggplot2)

summary_df <- tibble(
  Outcome = c("Data-Driven (Clean)", "Demographic Hallucination"),
  Count = c(sum(!evaluation_df$has_demographic_hallucination), sum(evaluation_df$has_demographic_hallucination))
)

error_plot <- ggplot(summary_df, aes(x = Outcome, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Data-Driven (Clean)" = "#4CAF50", "Demographic Hallucination" = "#F44336")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Monte Carlo Simulation Results (100 Runs)",
    subtitle = "Frequency of LLM sociodemographic hallucinations vs strict data adherence",
    x = "Persona Generation Outcome",
    y = "Number of Runs"
  ) +
  geom_text(aes(label = Count), vjust = -0.5, size = 6, fontface = "bold") +
  theme(legend.position = "none")

print(error_plot)

# Print examples of the generated texts
# Since hallucination rate is 0%, we sample 3 random successful runs to show the qualitative output
set.seed(123)
sample_examples <- evaluation_df %>% 
  sample_n(3) %>% 
  select(run_id, generated_text)

for(i in 1:nrow(sample_examples)) {
  cat(sprintf("\n[Run %d]:\n%s\n", sample_examples$run_id[i], sample_examples$generated_text[i]))
}


file_conn <- file("llm_persona_samples.txt", open = "w")

for(i in 1:nrow(sample_examples)) {
  lines_to_write <- c(
    lines_to_write, 
    sprintf("[Run %d]:", sample_examples$run_id[i]), 
    sample_examples$generated_text[i],
    "--------------------------------------------------",
    ""
  )
}

writeLines(lines_to_write, file_conn)
close(file_conn)

