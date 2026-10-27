library(tidyverse)
library(knitr)
library(scales)

# 1. Caricamento dei dati valutati (se non sono già in memoria)
df_final <- read_csv("Task2_LLM_Judge_Evaluated.csv")

# 2. Calcolo delle metriche finali (in formato tabulare ordinato)
final_metrics_table <- df_final %>%
  reframe(
    Metric = c("Inference Accuracy (H0)", "Magnitude Accuracy (η²)", "Business Accuracy", "Inversion Error Rate"),
    Value = c(
      mean(q1_reject_h0, na.rm = TRUE),
      mean(q2_magnitude_correct, na.rm = TRUE),
      mean(q3_business_insight_correct, na.rm = TRUE),
      mean(q3_inversion_error, na.rm = TRUE)
    ),
    Description = c(
      "Correctly identified to reject null hypothesis",
      "Correctly interpreted effect size magnitude",
      "Correctly suggested physical store priority",
      "Fell into the online assistance inversion trap"
    )
  )


# Visualizzazione della tabella in formato Markdown (ottima per il testo della tesi)
print("--- TABELLA PER LA TESI ---")
print(kable(final_metrics_table, format = "markdown", digits = 4))


# 3. Preparazione dei dati per il Plot
plot_data <- final_metrics_table %>%
  mutate(Metric = factor(Metric, levels = rev(Metric))) # Inverte per l'ordine sul grafico a barre

# Creazione del Plot professionale con ggplot2
p <- ggplot(plot_data, aes(x = Value, y = Metric, fill = Metric)) +
  geom_col(width = 0.6, show.legend = FALSE, alpha = 0.85) +
  # Aggiunge le etichette con i valori percentuali/proporzioni sulle barre
  geom_text(aes(label = scales::percent(Value, accuracy = 0.1)), 
            hjust = -0.2, fontface = "bold", size = 4) +
  xlim(0, 1.2) + # Spazio extra per i testi delle percentuali
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "LLM Performance on ANOVA Interpretation (Task 2)",
    subtitle = "Monte Carlo Simulation Results (N = 100)",
    x = "Proportion / Rate",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(color = "gray40", size = 11, hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black", size = 11)
  )

# Mostra il grafico
print(p)

# Salvataggio del grafico in alta risoluzione per la tesi
ggsave("Task2_Performance_Plot.png", plot = p, width = 8, height = 4.5, dpi = 300)
cat("\nGrafico salvato con successo come 'Task2_Performance_Plot.png'")