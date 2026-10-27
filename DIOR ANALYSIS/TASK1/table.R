library(flextable)
library(dplyr)
library(webshot2)
library(officer) 

df_results <- data.frame(
  Task = "Data Cleaning (Skip Logic)",
  Correct_Diagnosis = 97,
  Methodological_Error = 39,
  Prescriptive_Coherence = 61
)

table_ft <- flextable(df_results) %>%
  set_header_labels(
    Task = "Analytical Task",
    Correct_Diagnosis = "Correct Diagnosis (%)",
    Methodological_Error = "Methodological Error (%)",
    Prescriptive_Coherence = "Coherent Action (%)"
  ) %>%
  add_header_lines(values = "Results on 100 Monte Carlo simulations (Model: Gemini 3.7 Flash)") %>%
  add_header_lines(values = "LLM Evaluative Trade-off: Diagnosis vs Prescription") %>%
  bold(i = 1:2, part = "header") %>%
  bold(i = 3, part = "header") %>% 
  add_footer_lines(values = "Note: Coherent Action (100% - Methodological Error) represents the ability to refrain from applying forced imputations.") %>%
  italic(part = "footer") %>%
  theme_vanilla() %>%
  bg(bg = "white", part = "all") %>%
  color(j = "Correct_Diagnosis", color = "#27ae60", part = "body") %>%
  bold(j = "Correct_Diagnosis", part = "body") %>%
  
  color(j = "Methodological_Error", color = "#e74c3c", part = "body") %>%
  bold(j = "Methodological_Error", part = "body") %>%
  
  color(j = "Prescriptive_Coherence", color = "#f39c12", part = "body") %>%
  bold(j = "Prescriptive_Coherence", part = "body") %>%
  
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(align = "left", part = "footer") %>%
  autofit()

print(table_ft)

save_as_image(table_ft, path = "table.png", webshot = "webshot2")