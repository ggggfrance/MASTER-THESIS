library(tidyverse)
library(scales)

# 1. Creazione del dataset consolidato con i risultati dei due esperimenti
df_comparison <- tibble(
  Scenario = rep(c("1. Full Context", "2. No Context (Stress Test)"), each = 4),
  Metric = rep(c(
    "Inference Accuracy (H0)", 
    "Magnitude Accuracy (η²)", 
    "Business Insight Accuracy", 
    "Critical Error (Inversion/Hallucination)"
  ), times = 2),
  Value = c(
    # Valori Esperimento 1 (Full Context)
    1.00, 1.00, 1.00, 0.00,
    # Valori Esperimento 2 (No Context)
    1.00, 1.00, 0.97, 0.00
  )
)

# Ordiniamo le metriche per visualizzarle nel modo corretto sul grafico
df_comparison <- df_comparison %>%
  mutate(Metric = factor(Metric, levels = c(
    "Inference Accuracy (H0)", 
    "Magnitude Accuracy (η²)", 
    "Business Insight Accuracy", 
    "Critical Error (Inversion/Hallucination)"
  )))

# 2. Creazione del Plot
p_comparativo <- ggplot(df_comparison, aes(x = Metric, y = Value, fill = Scenario)) +
  # Barre affiancate con dodge
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9, color = "black", size = 0.2) +
  # Etichette con le percentuali sopra le barre
  geom_text(
    aes(label = percent(Value, accuracy = 1)), 
    position = position_dodge(width = 0.8), 
    vjust = -0.5, 
    fontface = "bold", 
    size = 4,
    color = "black"
  ) +
  # Scala Y in formato percentuale
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.15), breaks = seq(0, 1, by = 0.25)) +
  # Colori eleganti e professionali per la tesi
  scale_fill_manual(values = c("1. Full Context" = "#00ced1", "2. No Context (Stress Test)" = "#ff69b4")) +
  labs(
    title = "LLM Robustness: Full Context vs. Missing Context",
    subtitle = "Task: ANOVA Interpretation (Monte Carlo N=100 per scenario)",
    x = NULL,
    y = "Success / Error Rate",
    fill = "Experimental Scenario"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
    plot.subtitle = element_text(color = "gray30", size = 12, margin = margin(b = 20)),
    axis.text.x = element_text(face = "bold", size = 11, angle = 15, hjust = 1),
    axis.text.y = element_text(size = 11),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Mostra il grafico in RStudio
print(p_comparativo)

# Salvataggio in alta risoluzione
ggsave("Task2_Context_Comparison_Plot.png", plot = p_comparativo, width = 10, height = 6, dpi = 300)
cat("\nGrafico comparativo salvato come 'Task2_Context_Comparison_Plot.png'\n")